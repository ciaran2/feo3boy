use std::error::Error;
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
    pub struct SweepControl : u8 {
        const PACE       = 0b01110000;
        const SLOPE_DIR  = 0b00001000;
        const SLOPE_CTRL = 0b00000111;
    }
}

impl SweepControl {
    fn new_sweep(&self) -> Sweep {
        Sweep {
            shift: (*self & Self::SLOPE_CTRL).bits(),
            step: if self.contains(Self::SLOPE_DIR) { -1 } else { 1 },
            pace: (*self & Self::PACE).bits() >> 4,
            ticks: 0,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Sweep {
    shift: u8,
    step: i16,
    pace: u8,
    ticks: u8,
}

impl Sweep {
    fn apply(&self, wavelength: u16) -> u16 {
        if self.shift == 0 {
            wavelength
        }
        else {
            (wavelength as i16 + self.step * (wavelength as i16 >> self.shift)) as u16
        }
    }

    fn tick(&mut self) -> bool {
        if self.pace == 0 { return false; }

        self.ticks += 1;
        if self.ticks == self.pace {
            self.ticks = 0;
            true
        }
        else {
            false
        }
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
    pub struct EnvelopeControl : u8 {
        const INIT_VOL  = 0b11110000;
        const DIRECTION = 0b00001000;
        const PACE      = 0b00000111;
    }
}

impl EnvelopeControl {
    fn new_envelope(&self) -> Envelope {
        Envelope {
            level: (*self & Self::INIT_VOL).bits() as i8 >> 4,
            step: if self.contains(Self::DIRECTION) { 1 } else { -1 },
            pace: (*self & Self::PACE).bits(),
            ticks: 0,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Envelope {
    level: i8,
    step: i8,
    pace: u8,
    ticks: u8,
}

impl Envelope {
    fn level(&self) -> u16 {
        self.level as u16
    }

    fn tick(&mut self) {
        if self.pace > 0 {
            self.ticks += 1;
            if self.ticks == self.pace {
                self.ticks = 0;
                self.level = (self.level + self.step).clamp(0x0, 0xf);
            }
        }
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
        const LFSR_WIDTH  = 0b00001000;
        const CLOCK_DIV   = 0b00000111;
    }
}

impl NoiseControl {
    fn lfsr_mask(&self) -> u16 {
        if self.contains(Self::LFSR_WIDTH) {
            0x8080
        }
        else {
            0x8000
        }
    }

    fn period(&self) -> u32 {
        let r = (*self | Self::CLOCK_DIV).bits() as u32;
        let s = ((*self | Self::CLOCK_SHIFT).bits() >> 4) as u32;

        info!("r: {}, s: {}", r, s);

        if r == 0 {
            1 << (s + 3)
        }
        else {
            r << (s + 4)
        }
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

#[derive(Clone, Debug)]
pub struct ApuState {
    //pub output_buffer: VecDeque<(i16, i16)>,
    output_sample: Option<(i16, i16)>,
    output_period: f32,
    sample_cursor: f32,
}

impl ApuState {
    pub fn new() -> Self {
        ApuState {
            //output_buffer: VecDeque::new(),
            output_sample: None,
            output_period: 0.0,
            sample_cursor: 0.0,
        }
    }

    pub fn set_output_sample_rate(&mut self, sample_rate: u32) {
        self.output_period = CLOCK_SPEED as f32 / sample_rate as f32;
    }

    pub fn consume_output_sample(&mut self) -> Option<(i16, i16)> {
        let output_sample = self.output_sample;

        //if let Some(sample) = output_sample {
        //    info!("Emitting output sample {}, {}", sample.0, sample.1);
        //}

        self.output_sample = None;
        output_sample
    }
}

pub trait Channel {
    fn get_sample(&self, sample_cursor: f32) -> i16;

    fn read_control(&self) -> u8;
    fn set_control(&mut self, value: u8);
    fn set_length(&mut self, value: u8);

    fn check_trigger(&mut self);
    fn apu_tick(&mut self);
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PulseChannel {
    pub envelope_control: EnvelopeControl,
    envelope: Envelope,
    sweep_control: SweepControl,
    sweep: Sweep,
    pub timer: PulseTimer,
    period: u32,
    phase_offset: u32,
    active: bool,
    triggered: bool,
    wavelength: u16,
    level: u16,
    length_enable: bool,
    length_acc: u8,
    length_aticks: u8,
    envelope_aticks: u8,
    sweep_aticks: u8,
}

const PULSE_TABLE: [[u16;8];4] = [[1, 1, 1, 1, 1, 1, 1, 0],
                                  [0, 1, 1, 1, 1, 1, 1, 0],
                                  [0, 1, 1, 1, 1, 0, 0, 0],
                                  [1, 0, 0, 0, 0, 0, 0, 1]];

impl PulseChannel {
    pub fn wavelength_low(&self) -> u8 {
        (self.wavelength & 0xff) as u8
    }

    pub fn set_envelope(&mut self, value: u8) {
        self.envelope_control = EnvelopeControl::from_bits_truncate(value);
        //set an envelope phase offset?
        //needs retrigger to take
    }

    pub fn set_wavelength_low(&mut self, low_byte: u8) {
        self.wavelength = (self.wavelength & 0x700) | low_byte as u16;
        self.generate_period();
    }

    fn generate_period(&mut self) {
        self.period = 32 * (2048 - self.wavelength as u32);
    }
}

impl Channel for PulseChannel {
    fn get_sample(&self, sample_cursor: f32) -> i16 {
        if self.active {
            let pulse_step = (((sample_cursor as u32 + self.phase_offset) % self.period) / (self.period / 8)) as usize;
            debug!("Sampling pulse channel from step {} of duty cycle {}", pulse_step, self.timer.duty());
            let dac_input = PULSE_TABLE[self.timer.duty()][pulse_step] * self.envelope.level();
            -(dac_input as i16 - 8)
        }
        else {
            0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        }
        else {
            0
        }
    }

    fn set_control(&mut self, value: u8) {
        let control = ChannelControl::from_bits_truncate(value);

        self.triggered = control.contains(ChannelControl::TRIGGER);

        self.length_enable = control.contains(ChannelControl::LENGTH_ENABLE);
        self.wavelength = (self.wavelength & 0xff) | ((control & ChannelControl::WAVELENGTH_HIGH).bits() as u16) << 8;
        self.generate_period();
    }

    fn set_length(&mut self, value: u8) {
        self.timer = PulseTimer::from_bits_truncate(value);
        self.length_acc = (self.timer & PulseTimer::INIT_TIMER).bits()
    }

    fn check_trigger(&mut self) {
        if self.triggered {
            info!("Pulse channel triggered at frequency {}", CLOCK_SPEED / self.period);

            self.active = true;
            self.triggered = false;
            self.envelope = self.envelope_control.new_envelope();
            self.sweep = self.sweep_control.new_sweep();
        }
    }

    fn apu_tick(&mut self) {
        self.length_aticks += 1;
        self.envelope_aticks += 1;
        self.sweep_aticks += 1;

        if self.length_aticks == 2 {
            self.length_aticks = 0;
            if self.length_enable {
                self.length_acc = self.length_acc.wrapping_add(1);
                if self.length_acc == 64 {
                    self.active = false;
                }
            }
        }
        if self.sweep_aticks == 4 {
            self.sweep_aticks = 0;
            if self.sweep.tick() {
                let wavelength = self.sweep.apply(self.wavelength);
                if self.wavelength > 0x7ff {
                    self.wavelength = 0;
                    self.active = false;
                }
                else {
                    self.wavelength = wavelength;
                }
            }
        }
        if self.envelope_aticks == 8 {
            self.envelope_aticks = 0;
            self.envelope.tick();
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct WavetableChannel {
    period: u32,
    phase_offset: u32,
    enabled: bool,
    active: bool,
    triggered: bool,
    wavelength: u16,
    level_shift: u8,
    length_enable: bool,
    length_acc: u8,
    length_aticks: u8,
    sample_table: [u8;32],
}

impl WavetableChannel {
    pub fn wavelength_low(&self) -> u8 {
        (self.wavelength & 0xff) as u8
    }

    pub fn set_wavelength_low(&mut self, low_byte: u8) {
        self.wavelength = (self.wavelength & 0x700) | low_byte as u16;
        self.generate_period();
    }

    pub fn get_samples(&mut self, samples: usize) -> u8 {
        let base = samples * 2;
        self.sample_table[base] << 4 + self.sample_table[base + 1]
    }

    pub fn set_enable(&mut self, value: u8) {
        self.enabled = (value & 0x80) != 0;
    }

    pub fn set_samples(&mut self, samples: u16, value: u8) {
        let base = (samples * 2) as usize;
        self.sample_table[base] = (value & 0xf0) >> 4;
        self.sample_table[base + 1] = value & 0xf;
    }

    pub fn set_level(&mut self, value: u8) {
        let value = (value >> 5) & 0x3;
        self.level_shift = if value == 0 {
            4
        }
        else {
            value - 1
        };
    }

    pub fn generate_period(&mut self) {
        self.period = 64 * (2048 - self.wavelength as u32);
    }
}


impl Channel for WavetableChannel {
    fn get_sample(&self, sample_cursor: f32) -> i16 {
        if self.active {
            let wavetable_step = (((sample_cursor as u32 + self.phase_offset) % self.period) / (self.period / 32)) as usize;
            debug!("Sampling wavetable channel from step {} of sample table.", wavetable_step);
            let dac_input = (self.sample_table[wavetable_step] as u16) >> self.level_shift;
            -(dac_input as i16 - 8)
        }
        else {
            0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        }
        else {
            0
        }
    }

    fn set_control(&mut self, value: u8) {
        let control = ChannelControl::from_bits_truncate(value);

        self.triggered = control.contains(ChannelControl::TRIGGER);

        self.length_enable = control.contains(ChannelControl::LENGTH_ENABLE);
        self.wavelength = (self.wavelength & 0xff) | ((control & ChannelControl::WAVELENGTH_HIGH).bits() as u16) << 8;
        self.generate_period();
    }

    fn set_length(&mut self, value: u8) {
        self.length_acc = value;
    }

    fn check_trigger(&mut self) {
        if self.triggered {
            info!("Wavetable channel triggered at frequency {}", CLOCK_SPEED / self.period);

            self.active = true;
            self.triggered = false;
        }
    }

    fn apu_tick(&mut self) {
        self.length_aticks += 1;

        if self.length_aticks == 2 {
            self.length_aticks = 0;
            if self.length_enable {
                self.length_acc = self.length_acc.wrapping_add(1);
                if self.length_acc == 64 {
                    self.active = false;
                }
            }
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NoiseChannel {
    phase_offset: u32,
    enabled: bool,
    active: bool,
    triggered: bool,
    noise_control: NoiseControl,
    lfsr: u16,
    envelope_control: EnvelopeControl,
    envelope: Envelope,
    length_enable: bool,
    length_acc: u8,
    length_aticks: u8,
    envelope_aticks: u8,
    lfsr_ticks: u32,
}

impl NoiseChannel {
    pub fn set_noise_control(&mut self, value: u8) {
        self.noise_control = NoiseControl::from_bits_truncate(value);
    }

    pub fn set_envelope(&mut self, value: u8) {
        self.envelope_control = EnvelopeControl::from_bits_truncate(value);
        //set an envelope phase offset?
        //needs retrigger to take
    }

    // Unlike the other channels noise has state updates that
    //  need to be updated faster than the div-apu tick
    pub fn tick(&mut self, tcycles: u32) {
        self.lfsr_ticks += tcycles;
        if self.lfsr_ticks > self.noise_control.period() {
            self.lfsr_ticks -= self.noise_control.period(); 

            let feedback_bits = if (self.lfsr & 0x1) ^ ((self.lfsr >> 1) & 0x1) == 0 {
                0x8080
            }
            else {
                0x0
            };

            let lfsr_mask = self.noise_control.lfsr_mask();
            self.lfsr = (self.lfsr & !lfsr_mask) | (feedback_bits & lfsr_mask);
        }
    }
}

impl Channel for NoiseChannel {
    fn get_sample(&self, sample_cursor: f32) -> i16 {
        if self.active {
            let dac_input = (self.lfsr & 0x1) * self.envelope.level();
            -(dac_input as i16 - 8)
        }
        else {
            0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        }
        else {
            0
        }
    }

    fn set_control(&mut self, value: u8) {
        let control = ChannelControl::from_bits_truncate(value);

        self.triggered = control.contains(ChannelControl::TRIGGER);

        self.length_enable = control.contains(ChannelControl::LENGTH_ENABLE);
    }

    fn set_length(&mut self, value: u8) {
        self.length_acc = value & 0x1f;
    }

    fn check_trigger(&mut self) {
        if self.triggered {
            info!("Noise channel triggered");

            self.active = true;
            self.triggered = false;
            self.envelope = self.envelope_control.new_envelope();
        }
    }

    fn apu_tick(&mut self) {
        self.length_aticks += 1;
        self.envelope_aticks += 1;

        if self.length_aticks == 2 {
            self.length_aticks = 0;
            if self.length_enable {
                self.length_acc = self.length_acc.wrapping_add(1);
                if self.length_acc == 64 {
                    self.active = false;
                }
            }
        }
        if self.envelope_aticks == 8 {
            self.envelope_aticks = 0;
            self.envelope.tick();
        }
    }
}

pub trait ApuContext: IoRegsContext {
    fn apu(&self) -> &ApuState;
    fn apu_mut(&mut self) -> &mut ApuState;
}

/// to be called on divider bit 4 (5 double speed) falling edge
pub fn apu_tick(ctx: &mut impl ApuContext) {
    ctx.ioregs_mut().ch1_mut().apu_tick();
    ctx.ioregs_mut().ch2_mut().apu_tick();
    ctx.ioregs_mut().ch3_mut().apu_tick();
    ctx.ioregs_mut().ch4_mut().apu_tick();
}

pub fn tick(ctx: &mut impl ApuContext, tcycles: u64) {

    let mut sample_cursor = ctx.apu().sample_cursor;

    ctx.ioregs_mut().ch4_mut().tick(tcycles as u32);

    ctx.ioregs_mut().ch1_mut().check_trigger();
    ctx.ioregs_mut().ch2_mut().check_trigger();
    ctx.ioregs_mut().ch3_mut().check_trigger();
    ctx.ioregs_mut().ch4_mut().check_trigger();

    if ctx.apu().output_period > 0.0 {

        let next_sample = sample_cursor % ctx.apu().output_period;
        if tcycles as f32 > next_sample.into() {
            sample_cursor += next_sample;
            let mono_sample = ctx.ioregs().ch1().get_sample(sample_cursor) +
                               ctx.ioregs().ch2().get_sample(sample_cursor) +
                               ctx.ioregs().ch3().get_sample(sample_cursor) +
                               ctx.ioregs().ch4().get_sample(sample_cursor);
            debug!("Mono sample: {}", mono_sample);
            //let mono_sample_signed = -(mono_sample as i16 - 32);
            //ctx.apu_mut().output_buffer.push_back((mono_sample_signed, mono_sample_signed));
            ctx.apu_mut().output_sample = Some((mono_sample, mono_sample));
        }
    }

    sample_cursor += tcycles as f32;

    if sample_cursor > MAX_PERIOD as f32 {
        sample_cursor -= MAX_PERIOD as f32;
    }

    ctx.apu_mut().sample_cursor = sample_cursor;
}
