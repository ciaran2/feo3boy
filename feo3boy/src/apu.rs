use std::error::Error;
use std::collections::{VecDeque};
use bitflags::bitflags;
use crate::memdev::{Addr, MemDevice, IoRegs, IoRegsContext};
use log::{debug, trace, info};

bitflags! {
    #[derive(Default)]
    pub struct SoundEnable : u8 {
        const ALL = 0b10000000;
        const CH4 = 0b00001000;
        const CH3 = 0b00000100;
        const CH2 = 0b00000010;
        const CH1 = 0b00000001;
        const WRITEABLE = 0b10000000;
    }
}


bitflags! {
    #[derive(Default)]
    pub struct SoundPan : u8 {
        const CH4_LEFT  = 0b10000000;
        const CH3_LEFT  = 0b01000000;
        const CH2_LEFT  = 0b00100000;
        const CH1_LEFT  = 0b00010000;
        const CH4_RIGHT = 0b00001000;
        const CH3_RIGHT = 0b00000100;
        const CH2_RIGHT = 0b00000010;
        const CH1_RIGHT = 0b00000001;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct SoundVolume : u8 {
        const VIN_LEFT  = 0b10000000;
        const VOL_LEFT  = 0b01110000;
        const VIN_RIGHT = 0b00001000;
        const VOL_RIGHT = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct PulseSweep : u8 {
        const PACE       = 0b01110000;
        const SLOPE_DIR  = 0b00001000;
        const SLOPE_CTRL = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct PulseTimer : u8 {
        const DUTY_CYCLE = 0b11000000;
        const INIT_TIMER = 0b00111111;
    }
}

impl PulseTimer {
    pub fn duty(&self) -> usize {
        ((*self | PulseTimer::DUTY_CYCLE).bits() >> 6) as usize
    }
}

bitflags! {
    #[derive(Default)]
    pub struct Envelope : u8 {
        const INIT_VOL  = 0b11110000;
        const DIRECTION = 0b00001000;
        const PACE      = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct WavetableLevel : u8 {
        const LEVEL = 0b01100000;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct NoiseControl : u8 {
        const CLOCK_SHIFT = 0b11110000;
        const LSFR_WIDTH  = 0b00001000;
        const CLOCK_DIV   = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct ChannelControl : u8 {
        const TRIGGER         = 0b10000000;
        const LENGTH_ENABLE   = 0b01000000;
        const WAVELENGTH_HIGH = 0b00000111;
        const READABLE        = 0b01000000;
    }
}

// We'll worry about double speed another time
const CLOCK_SPEED: u32= 4194304;

// maximum period of any waveform in t cycles
const MAX_PERIOD: u32 = 131072;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ApuState {
    pub output_buffer: VecDeque<(i16, i16)>,
    output_period: u32,
    sample_cursor: u32,
}

impl ApuState {
    pub fn new() -> Self {
        ApuState {
            output_buffer: VecDeque::new(),
            output_period: 0,
            sample_cursor: 0,
        }
    }

    pub fn set_output_sample_rate(&mut self, sample_rate: u32) {
        self.output_period = CLOCK_SPEED / sample_rate;
    }
}

pub trait Channel {
    fn get_sample(&self, sample_cursor: u32) -> u16;

    fn set_control(&mut self, control: ChannelControl);

    fn tick(&mut self, tcycles: u64);
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PulseChannel {
    period: u32,
    phase_offset: u32,
    active: bool,
    triggered: bool,
    timer_enable: bool,
    envelope: Envelope,
    timer: PulseTimer,
    timer_acc: u8,
    wavelength: u16,
    level: u16,
}

const pulse_table: [[u16;8];4] = [[1, 1, 1, 1, 1, 1, 1, 0],
                                  [0, 1, 1, 1, 1, 1, 1, 0],
                                  [0, 1, 1, 1, 1, 0, 0, 0],
                                  [1, 0, 0, 0, 0, 0, 0, 1]];

impl PulseChannel {
    fn set_envelope(&mut self, envelope: Envelope) {
        self.envelope = envelope;
        //set an envelope phase offset?
        //needs retrigger to take
    }

    fn generate_period(&mut self) {
        self.period = 32 * (2048 - self.wavelength as u32);
    }
}

impl Channel for PulseChannel {
    fn get_sample(&self, sample_cursor: u32) -> u16 {
        if self.active {
            let pulse_step = (((sample_cursor + self.phase_offset) % self.period) / (self.period / 8)) as usize;
            pulse_table[self.timer.duty()][pulse_step] * self.level
        }
        else {
            0
        }
    }

    fn set_control(&mut self, control: ChannelControl) {
        self.triggered = control.contains(ChannelControl::TRIGGER);
        self.timer_enable = control.contains(ChannelControl::LENGTH_ENABLE);
        self.wavelength = (self.wavelength & 0xff) | ((control & ChannelControl::WAVELENGTH_HIGH).bits() as u16) << 8;
        self.generate_period();
    }

    fn tick(&mut self, tcycles: u64) {
        if tcycles > (0xff - self.timer_acc) as u64 {
            if self.timer_enable { self.active = false; }
        }
        else {
            self.timer_acc += tcycles as u8;
        }

        if self.triggered {
            self.active = true;
            self.triggered = false;
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WavetableChannel {
    period: u32,
    phase_offset: u32,
    active: bool,
    triggered: bool,
    sample_table: [u8;32],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NoiseChannel {
    noise_control: NoiseControl,
}

pub trait ApuContext: IoRegsContext {
    fn apu(&self) -> &ApuState;
    fn apu_mut(&mut self) -> &mut ApuState;
    fn consume_sample(&mut self) -> Option<(i16, i16)>;
}

pub fn tick(ctx: &mut impl ApuContext, tcycles: u64) {

    let mut sample_cursor = ctx.apu().sample_cursor;

    if ctx.apu().output_period > 0 {
        let next_sample = sample_cursor % ctx.apu().output_period;
        if tcycles > next_sample.into() {
            ctx.ioregs_mut().ch1_mut().tick(next_sample as u64);
            ctx.ioregs_mut().ch2_mut().tick(next_sample as u64);
            sample_cursor += next_sample;
            let mono_sample = ctx.ioregs().ch1().get_sample(sample_cursor) +
                               ctx.ioregs().ch2().get_sample(sample_cursor);
            let mono_sample_signed = -(mono_sample as i16 - 32);
            ctx.apu_mut().output_buffer.push_back((mono_sample_signed, mono_sample_signed));
            ctx.ioregs_mut().ch1_mut().tick(tcycles - next_sample as u64);
            ctx.ioregs_mut().ch2_mut().tick(tcycles - next_sample as u64);
        }
        else {
            ctx.ioregs_mut().ch1_mut().tick(tcycles);
            ctx.ioregs_mut().ch2_mut().tick(tcycles);
        }
    }
    else {
        ctx.ioregs_mut().ch1_mut().tick(tcycles);
        ctx.ioregs_mut().ch2_mut().tick(tcycles);
    }

    if sample_cursor > MAX_PERIOD {
        sample_cursor -= MAX_PERIOD;
    }

    ctx.apu_mut().sample_cursor = sample_cursor;
}
