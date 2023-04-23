use bitflags::bitflags;
use log::{debug, error, info, trace};

use crate::bits::BitGroup;
use crate::memdev::{MemDevice, RelativeAddr};

bitflags! {
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
    #[memdev(bitflags, writable = SoundEnable::WRITABLE)]
    #[repr(transparent)]
    pub struct SoundEnable : u8 {
        const ALL = 0b10000000;
        const CH4 = 0b00001000;
        const CH3 = 0b00000100;
        const CH2 = 0b00000010;
        const CH1 = 0b00000001;
        const WRITABLE = 0b10000000;
    }
}

bitflags! {
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[repr(transparent)]
    pub struct ChannelMix : u8 {
        const CH4 = 0b00001000;
        const CH3 = 0b00000100;
        const CH2 = 0b00000010;
        const CH1 = 0b00000001;
    }
}

bitflags! {
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
    #[memdev(bitflags)]
    #[repr(transparent)]
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

impl SoundPan {
    fn channel_mix(&self, right: bool) -> ChannelMix {
        let shift = if right { 0 } else { 4 };

        ChannelMix::from_bits_truncate(self.bits() >> shift)
    }
}

/// Represents sound and volume settings.
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits)]
#[repr(transparent)]
pub struct SoundVolume(u8);

impl SoundVolume {
    const VIN_LEFT: BitGroup = BitGroup(0b1000_0000);
    const VOL_LEFT: BitGroup = BitGroup(0b0111_0000);
    const VIN_RIGHT: BitGroup = BitGroup(0b0000_1000);
    const VOL_RIGHT: BitGroup = BitGroup(0b0000_0111);

    /// Get the vin_left
    #[inline]
    pub fn vin_left(self) -> bool {
        Self::VIN_LEFT.extract_bool(self.0)
    }

    /// Set the vin_left
    #[inline]
    pub fn set_vin_left(&mut self, val: bool) {
        Self::VIN_LEFT.apply(&mut self.0, val as u8);
    }

    /// Get the left volume.
    #[inline]
    pub fn vol_left(self) -> u8 {
        Self::VOL_LEFT.extract(self.0)
    }

    /// Set the left volume.
    #[inline]
    pub fn set_vol_left(&mut self, val: u8) {
        Self::VOL_LEFT.apply(&mut self.0, val);
    }

    /// Get the vin_right
    #[inline]
    pub fn vin_right(self) -> bool {
        Self::VIN_RIGHT.extract_bool(self.0)
    }

    /// Set the vin_right
    #[inline]
    pub fn set_vin_right(&mut self, val: bool) {
        Self::VIN_RIGHT.apply(&mut self.0, val as u8);
    }

    /// Get the right volume.
    #[inline]
    pub fn vol_right(self) -> u8 {
        Self::VOL_RIGHT.extract(self.0)
    }

    /// Set the right volume.
    #[inline]
    pub fn set_vol_right(&mut self, val: u8) {
        Self::VOL_RIGHT.apply(&mut self.0, val);
    }

    fn vol_multiplier(&self, right: bool) -> f32 {
        let shift = if right { 0 } else { 4 };
        let vol_digital = (self.0 >> shift) & 0x7;

        (0.125 * (vol_digital as f32 + 1.0)).max(1.0)
    }
}

/// Represends sweep control settings.
#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits, readable = SweepControl::RW_BITS, writable = SweepControl::RW_BITS)]
#[repr(transparent)]
pub struct SweepControl(u8);

impl SweepControl {
    const PACE: BitGroup = BitGroup(0b0111_0000);
    const SLOPE_DIR: BitGroup = BitGroup(0b0000_1000);
    const SLOPE_CTRL: BitGroup = BitGroup(0b0000_0111);
    const RW_BITS: u8 = 0b0111_1111;

    /// Get the pace.
    #[inline]
    pub fn pace(&self) -> u8 {
        Self::PACE.extract(self.0)
    }

    /// Set the pace
    #[inline]
    pub fn set_pace(&mut self, val: u8) {
        Self::PACE.apply(&mut self.0, val);
    }

    /// Get whether the slope is negative.
    #[inline]
    pub fn slope_negative(&self) -> bool {
        Self::SLOPE_DIR.extract_bool(self.0)
    }

    /// Get an i16 indicating the slope direction; -1 for negative, +1 for positive.
    #[inline]
    pub fn slope_dir(&self) -> i16 {
        if self.slope_negative() {
            -1
        } else {
            1
        }
    }

    /// Set the vin_right
    #[inline]
    pub fn set_slope_negative(&mut self, val: bool) {
        Self::SLOPE_DIR.apply(&mut self.0, val as u8);
    }

    /// Get the slope control.
    #[inline]
    pub fn slope_ctrl(&self) -> u8 {
        Self::SLOPE_CTRL.extract(self.0)
    }

    /// Set the right volume.
    #[inline]
    pub fn set_slope_ctrl(&mut self, val: u8) {
        Self::SLOPE_CTRL.apply(&mut self.0, val);
    }

    /// Build a new [`Sweep`] from the current [`SweepControl`] settings.
    fn new_sweep(&self) -> Sweep {
        Sweep {
            shift: self.slope_ctrl(),
            step: self.slope_dir(),
            pace: self.pace(),
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
        } else {
            (wavelength as i16 + self.step * (wavelength as i16 >> self.shift)) as u16
        }
    }

    fn tick(&mut self) -> bool {
        if self.pace == 0 {
            return false;
        }

        self.ticks += 1;
        if self.ticks == self.pace {
            self.ticks = 0;
            true
        } else {
            false
        }
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits)]
#[repr(transparent)]
pub struct PulseTimer(u8);

impl PulseTimer {
    const DUTY_CYCLE: BitGroup = BitGroup(0b1100_0000);
    const INIT_TIMER: BitGroup = BitGroup(0b0011_1111);

    /// Update the value in-place.
    #[inline]
    fn set(&mut self, val: u8) {
        self.0 = val;
    }

    /// Get the duty cycle.
    #[inline]
    pub fn duty_cycle(&self) -> u8 {
        Self::DUTY_CYCLE.extract(self.0)
    }

    /// Gets the duty cycle as a usize index.
    #[inline]
    pub fn duty(&self) -> usize {
        self.duty_cycle() as usize
    }

    /// Set the duty cycle.
    #[inline]
    pub fn set_duty_cycle(&mut self, val: u8) {
        Self::DUTY_CYCLE.apply(&mut self.0, val);
    }

    /// Get the init timer
    #[inline]
    pub fn init_timer(&self) -> u8 {
        Self::INIT_TIMER.extract(self.0)
    }

    /// Set the init timer.
    #[inline]
    pub fn set_init_timer(&mut self, val: u8) {
        Self::INIT_TIMER.apply(&mut self.0, val);
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits)]
#[repr(transparent)]
pub struct EnvelopeControl(u8);

impl EnvelopeControl {
    const INIT_VOL: BitGroup = BitGroup(0b1111_0000);
    const DIRECTION: BitGroup = BitGroup(0b0000_1000);
    const PACE: BitGroup = BitGroup(0b0000_0111);

    #[inline]
    fn set(&mut self, val: u8) {
        self.0 = val;
    }

    /// Get the init vol
    #[inline]
    pub fn init_vol(&self) -> i8 {
        Self::INIT_VOL.extract(self.0) as i8
    }

    /// Set the init vol.
    #[inline]
    pub fn set_init_vol(&mut self, val: i8) {
        Self::INIT_VOL.apply(&mut self.0, val as u8);
    }

    /// Get whether the direction is negative.
    #[inline]
    pub fn direction_negative(&self) -> bool {
        !Self::DIRECTION.extract_bool(self.0)
    }

    /// Get a signed value indicating the direction, either `-1` or `1`.
    #[inline]
    pub fn direction(&self) -> i8 {
        if self.direction_negative() {
            -1
        } else {
            1
        }
    }

    /// Set whether the direction is negative
    #[inline]
    pub fn set_direction_negative(&mut self, val: bool) {
        Self::DIRECTION.apply(&mut self.0, val as u8);
    }

    /// Get the pace.
    #[inline]
    pub fn pace(&self) -> u8 {
        Self::PACE.extract(self.0)
    }

    /// Set the init vol.
    #[inline]
    pub fn set_pace(&mut self, val: u8) {
        Self::PACE.apply(&mut self.0, val);
    }

    pub fn dac_enabled(&self) -> bool {
        (self.0 & 0xf8) != 0
    }

    fn new_envelope(&self) -> Envelope {
        Envelope {
            level: self.init_vol(),
            step: self.direction(),
            pace: self.pace(),
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

#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits, readable = WavetableLevel::RW_BITS, writable = WavetableLevel::RW_BITS)]
#[repr(transparent)]
pub struct WavetableLevel(u8);

impl WavetableLevel {
    const LEVEL: BitGroup = BitGroup(0b0110_0000);
    const RW_BITS: u8 = 0b0110_1000;

    /// Get the level.
    #[inline]
    pub fn level(self) -> u8 {
        Self::LEVEL.extract(self.0)
    }

    /// Set the level.
    #[inline]
    pub fn set_level(&mut self, val: u8) {
        Self::LEVEL.apply(&mut self.0, val);
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits)]
#[repr(transparent)]
pub struct NoiseControl(u8);

impl NoiseControl {
    const CLOCK_SHIFT: BitGroup = BitGroup(0b1111_0000);
    const LFSR_WIDTH: BitGroup = BitGroup(0b0000_1000);
    const CLOCK_DIV: BitGroup = BitGroup(0b0000_0111);

    fn set(&mut self, val: u8) {
        self.0 = val;
    }

    /// Get the clock shift.
    #[inline]
    pub fn clock_shift(&self) -> u8 {
        Self::CLOCK_SHIFT.extract(self.0)
    }

    /// Set the clock shift.
    #[inline]
    pub fn set_clock_shift(&mut self, val: u8) {
        Self::CLOCK_SHIFT.apply(&mut self.0, val);
    }

    /// Get the LFSR width bit.
    #[inline]
    pub fn lfsr_width(&self) -> bool {
        Self::LFSR_WIDTH.extract_bool(self.0)
    }

    /// Get the LFSR mask based on whether the LFSR bit is set.
    fn lfsr_mask(&self) -> u16 {
        if self.lfsr_width() {
            0x4080
        } else {
            0x4000
        }
    }

    /// Set the LFSR width bit.
    #[inline]
    pub fn set_lfsr_width(&mut self, val: bool) {
        Self::LFSR_WIDTH.apply(&mut self.0, val as u8);
    }

    /// Get the clock div.
    #[inline]
    pub fn clock_div(&self) -> u8 {
        Self::CLOCK_DIV.extract(self.0)
    }

    /// Set the clock div.
    #[inline]
    pub fn set_clock_div(&mut self, val: u8) {
        Self::CLOCK_DIV.apply(&mut self.0, val);
    }

    fn period(&self) -> u32 {
        let r = self.clock_div() as u32;
        let s = self.clock_shift() as u32;

        trace!("r: {}, s: {}", r, s);

        if r == 0 {
            1 << (s + 3)
        } else {
            r << (s + 4)
        }
    }
}

bitflags! {
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, MemDevice)]
    #[memdev(bitflags, readable = ChannelControl::READABLE)]
    pub struct ChannelControl : u8 {
        const TRIGGER            = 0b1000_0000;
        const LENGTH_ENABLE      = 0b0100_0000;
        /// Bit 0 of `WAVELENGTH_HIGH`. This pseudo-flag allows `from_bits_truncate` to
        /// set this bit independently of the rest of the wavelength bits.
        const WAVELENGTH_HIGH_B0 = 0b0000_0001;
        /// Bit 1 of `WAVELENGTH_HIGH`. This pseudo-flag allows `from_bits_truncate` to
        /// set this bit independently of the rest of the wavelength bits.
        const WAVELENGTH_HIGH_B1 = 0b0000_0010;
        /// Bit 2 of `WAVELENGTH_HIGH`. This pseudo-flag allows `from_bits_truncate` to
        /// set this bit independently of the rest of the wavelength bits.
        const WAVELENGTH_HIGH_B2 = 0b0000_0100;
        const READABLE           = 0b0100_0000;
    }
}

impl ChannelControl {
    const WAVELENGTH_HIGH: BitGroup = BitGroup(0b0000_0111);

    pub fn wavelength_high(self) -> u8 {
        Self::WAVELENGTH_HIGH.extract(self.bits())
    }

    pub fn set_wavelength_high(&mut self, val: u8) {
        Self::WAVELENGTH_HIGH.apply_bits(self, val);
    }
}

const HIGH_PASS_CUTOFF: f32 = 200.0;

#[derive(Clone, Debug, Default)]
struct HighPassFilter {
    last_input: (f32, f32),
    last_output: (f32, f32),
    alpha: f32,
}

impl HighPassFilter {
    fn set_sample_rate(&mut self, sample_rate: u32) {
        self.alpha = 1.0 / (2.0 * 3.1416 / (sample_rate as f32) * HIGH_PASS_CUTOFF + 1.0);
    }

    fn filter_sample(&mut self, input: (f32, f32)) -> (f32, f32) {
        let output = (
            self.alpha * (input.0 + self.last_output.0 - self.last_input.0),
            self.alpha * (input.1 + self.last_output.1 - self.last_input.1),
        );

        self.last_input = input;
        self.last_output = output;

        output
    }
}

// We'll worry about double speed another time
const CLOCK_SPEED: u32 = 4194304;

// maximum period of any waveform in t cycles
const MAX_PERIOD: u32 = 131072;

#[derive(Clone, Debug)]
pub struct ApuState {
    //pub output_buffer: VecDeque<(i16, i16)>,
    output_sample: Option<(f32, f32)>,
    output_period: f32,
    sample_cursor: f32,
    hpf: HighPassFilter,
}

impl ApuState {
    pub fn new() -> Self {
        ApuState {
            //output_buffer: VecDeque::new(),
            output_sample: None,
            output_period: 0.0,
            sample_cursor: 0.0,
            hpf: HighPassFilter::default(),
        }
    }

    pub fn set_output_sample_rate(&mut self, sample_rate: u32) {
        self.output_period = CLOCK_SPEED as f32 / sample_rate as f32;
        self.hpf.set_sample_rate(sample_rate);
        info!("Setting output period {}", self.output_period);
    }

    pub fn consume_output_sample(&mut self) -> Option<(f32, f32)> {
        let output_sample = self.output_sample;

        //if let Some(sample) = output_sample {
        //    info!("Emitting output sample {}, {}", sample.0, sample.1);
        //}

        self.output_sample = None;
        output_sample
    }
}

pub trait Channel {
    fn get_sample(&self) -> f32;

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
    sample_cursor: u32,
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

const PULSE_TABLE: [[u16; 8]; 4] = [
    [1, 1, 1, 1, 1, 1, 1, 0],
    [0, 1, 1, 1, 1, 1, 1, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 1],
];

impl PulseChannel {
    pub fn wavelength_low(&self) -> u8 {
        (self.wavelength & 0xff) as u8
    }

    pub fn set_envelope(&mut self, value: u8) {
        self.envelope_control.set(value);
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

    pub fn tick(&mut self, tcycles: u32) {
        if self.period > 0 {
            self.sample_cursor = (self.sample_cursor + tcycles) % self.period;
        }
    }
}

impl Channel for PulseChannel {
    fn get_sample(&self) -> f32 {
        if self.envelope_control.dac_enabled() && self.active {
            let pulse_step = (((self.sample_cursor as u32 + self.phase_offset) % self.period)
                / (self.period / 8)) as usize;
            debug!(
                "Sampling pulse channel from step {} of duty cycle {}",
                pulse_step,
                self.timer.duty()
            );
            let dac_input = PULSE_TABLE[self.timer.duty()][pulse_step] * self.envelope.level();
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        } else {
            0
        }
    }

    fn set_control(&mut self, value: u8) {
        let control = ChannelControl::from_bits_truncate(value);

        self.triggered = control.contains(ChannelControl::TRIGGER);

        self.length_enable = control.contains(ChannelControl::LENGTH_ENABLE);
        self.wavelength = (self.wavelength & 0xff) | (control.wavelength_high() as u16) << 8;
        self.generate_period();
    }

    fn set_length(&mut self, value: u8) {
        self.timer.set(value);
        self.length_acc = self.timer.init_timer();
    }

    fn check_trigger(&mut self) {
        if self.triggered {
            self.active = true;
            self.triggered = false;
            self.envelope = self.envelope_control.new_envelope();
            self.sweep = self.sweep_control.new_sweep();

            info!(
                "Pulse channel triggered at frequency {} with envelope {:?}, sweep control: {:x}, sweep {:?}, length_enable {}, length_acc {}",
                CLOCK_SPEED / self.period, self.envelope, self.sweep_control.0, self.sweep, self.length_enable, self.length_acc
            );
        }
    }

    fn apu_tick(&mut self) {
        self.length_aticks += 1;
        self.envelope_aticks += 1;
        self.sweep_aticks += 1;

        if self.sweep.pace == 0 {
            self.sweep.pace = self.sweep_control.pace();
        }

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
                if wavelength > 0x7ff {
                    self.wavelength = 0;
                    self.active = false;
                } else {
                    self.wavelength = wavelength;
                    self.generate_period();
                }
            }
        }
        if self.envelope_aticks == 8 {
            self.envelope_aticks = 0;
            self.envelope.tick();
        }
    }
}

impl MemDevice for PulseChannel {
    const LEN: usize = 5;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match addr.index() {
            0x00 => self.sweep_control.read_byte_relative(addr),
            0x01 => self.timer.read_byte_relative(addr.offset_by(0x01)),
            0x02 => self
                .envelope_control
                .read_byte_relative(addr.offset_by(0x02)),
            0x03 => self.wavelength_low(),
            0x04 => self.read_control(),
            _ => panic!("Address {} out of range for PulseChannel", addr),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        match addr.index() {
            0x00 => self.sweep_control.write_byte_relative(addr, val),
            0x01 => self.set_length(val),
            0x02 => self.set_envelope(val),
            0x03 => self.set_wavelength_low(val),
            0x04 => self.set_control(val),
            _ => panic!("Address {} out of range for PulseChannel", addr),
        }
    }

    memdev_bytes_from_byte!(PulseChannel);
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct WavetableChannel {
    period: u32,
    phase_offset: u32,
    enabled: bool,
    active: bool,
    triggered: bool,
    sample_cursor: u32,
    wavelength: u16,
    level_shift: u8,
    length_enable: bool,
    length_acc: u8,
    length_aticks: u8,
    /// Contains sample values which have been split in half to allow nybbles to be
    /// efficiently indexed when sampling.
    sample_table: [u8; 32],
}

impl WavetableChannel {
    pub fn wavelength_low(&self) -> u8 {
        (self.wavelength & 0xff) as u8
    }

    pub fn set_wavelength_low(&mut self, low_byte: u8) {
        self.wavelength = (self.wavelength & 0x700) | low_byte as u16;
        self.generate_period();
    }

    pub fn get_samples(&self, samples: usize) -> u8 {
        let base = samples * 2;
        (self.sample_table[base] << 4) + self.sample_table[base + 1]
    }

    pub fn set_enable(&mut self, value: u8) {
        self.enabled = (value & 0x80) != 0;
    }

    pub fn set_samples(&mut self, samples: usize, value: u8) {
        let base = samples * 2;
        self.sample_table[base] = (value & 0xf0) >> 4;
        self.sample_table[base + 1] = value & 0xf;
    }

    pub fn set_level(&mut self, value: u8) {
        let value = (value >> 5) & 0x3;
        self.level_shift = if value == 0 { 4 } else { value - 1 };
    }

    pub fn generate_period(&mut self) {
        self.period = 64 * (2048 - self.wavelength as u32);
    }

    pub fn tick(&mut self, tcycles: u32) {
        if self.period > 0 {
            self.sample_cursor = (self.sample_cursor + tcycles) % self.period;
        }
    }
}

impl Channel for WavetableChannel {
    fn get_sample(&self) -> f32 {
        if self.enabled && self.active {
            let wavetable_step = (((self.sample_cursor as u32 + self.phase_offset) % self.period)
                / (self.period / 32)) as usize;
            debug!(
                "Sampling wavetable channel from step {} of sample table.",
                wavetable_step
            );
            let dac_input = (self.sample_table[wavetable_step] as u16) >> self.level_shift;
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        } else {
            0
        }
    }

    fn set_control(&mut self, value: u8) {
        let control = ChannelControl::from_bits_truncate(value);

        self.triggered = control.contains(ChannelControl::TRIGGER);

        self.length_enable = control.contains(ChannelControl::LENGTH_ENABLE);
        self.wavelength = (self.wavelength & 0xff) | (control.wavelength_high() as u16) << 8;
        self.generate_period();
    }

    fn set_length(&mut self, value: u8) {
        self.length_acc = value;
    }

    fn check_trigger(&mut self) {
        if self.triggered {
            info!(
                "Wavetable channel triggered at frequency {}",
                CLOCK_SPEED / self.period
            );
            info!("  Wavetable sample table: {:?}", self.sample_table);

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

impl MemDevice for WavetableChannel {
    const LEN: usize = 0x15;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match addr.relative() {
            // Channel settings block.
            0x00..=0x04 => 0xff,
            // Channel "samples" block. ApuRegs will remap this portion to the correct
            // offset.
            0x05..=0x14 => self.get_samples(addr.offset_by(0x05).index()),
            _ => panic!("Address {addr}  out of range for WavetableChannel"),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        match addr.relative() {
            // Channel settings block.
            0x00 => self.set_enable(val),
            0x01 => self.set_length(val),
            0x02 => self.set_level(val),
            0x03 => self.set_wavelength_low(val),
            0x04 => self.set_control(val),
            // Channel "samples" block. ApuRegs will remap this portion to the correct
            // offset.
            0x05..=0x14 => self.set_samples(addr.offset_by(0x05).index(), val),
            _ => panic!("Address {addr}  out of range for WavetableChannel"),
        }
    }

    memdev_bytes_from_byte!(WavetableChannel);
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
        self.noise_control.set(value);
    }

    pub fn set_envelope(&mut self, value: u8) {
        self.envelope_control.set(value);
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
                0x4080
            } else {
                0x0
            };

            let lfsr_mask = self.noise_control.lfsr_mask();
            self.lfsr = ((self.lfsr & !lfsr_mask) | (feedback_bits & lfsr_mask)) >> 1;
        }
    }
}

impl Channel for NoiseChannel {
    fn get_sample(&self) -> f32 {
        if self.envelope_control.dac_enabled() && self.active {
            let dac_input = (self.lfsr & 0x1) * self.envelope.level();
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }

    fn read_control(&self) -> u8 {
        if self.length_enable {
            ChannelControl::LENGTH_ENABLE.bits()
        } else {
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
            self.active = true;
            self.triggered = false;
            self.lfsr = 0;
            self.envelope = self.envelope_control.new_envelope();
            info!(
                "Noise channel triggered with envelope {:?}, length_enable {}, length_acc {}",
                self.envelope, self.length_enable, self.length_acc
            );
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

impl MemDevice for NoiseChannel {
    const LEN: usize = 4;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match addr.relative() {
            0x00..=0x03 => 0xff,
            _ => panic!("Address {addr} out of range for NoiseChannel"),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        match addr.relative() {
            0x00 => self.set_length(val),
            0x01 => self.set_envelope(val),
            0x02 => self.set_noise_control(val),
            0x03 => self.set_control(val),
            _ => panic!("Address {addr} out of range for NoiseChannel"),
        }
    }

    memdev_bytes_from_byte!(NoiseChannel);
}

/// Memory-mapped IO registers used by the APU.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ApuRegs {
    pub ch1: PulseChannel,
    pub ch2: PulseChannel,
    pub ch3: WavetableChannel,
    pub ch4: NoiseChannel,
    pub sound_volume: SoundVolume,
    pub sound_pan: SoundPan,
    pub sound_enable: SoundEnable,
    pub wavetable: [u8; 16],
}

memdev_fields!(ApuRegs, len: 0x30, {
    0x00..=0x04 => ch1,
    0x05 => 0xff,
    0x06..=0x09 => { ch2, skip_over: 1 },
    0x0a..=0x0e => ch3,
    0x0f => 0xff,
    0x10..=0x13 => ch4,
    0x14 => sound_volume,
    0x15 => sound_pan,
    0x16..=0x1f => 0xff,
    0x20..=0x2f => { ch3, skip_over: 5 },
});

pub trait ApuContext {
    fn apu(&self) -> &ApuState;
    fn apu_mut(&mut self) -> &mut ApuState;

    fn apu_regs(&self) -> &ApuRegs;
    fn apu_regs_mut(&mut self) -> &mut ApuRegs;
}

fn get_stereo_sample(ctx: &impl ApuContext, right: bool) -> f32 {
    let channel_mix = ctx.apu_regs().sound_pan.channel_mix(right);

    ctx.apu_regs().sound_volume.vol_multiplier(right)
        * (if channel_mix.contains(ChannelMix::CH1) {
            0.25 * ctx.apu_regs().ch1.get_sample()
        } else {
            0.0
        } + if channel_mix.contains(ChannelMix::CH2) {
            0.25 * ctx.apu_regs().ch2.get_sample()
        } else {
            0.0
        } + if channel_mix.contains(ChannelMix::CH3) {
            0.25 * ctx.apu_regs().ch3.get_sample()
        } else {
            0.0
        } + if channel_mix.contains(ChannelMix::CH4) {
            0.25 * ctx.apu_regs().ch4.get_sample()
        } else {
            0.0
        })
}

/// to be called on divider bit 4 (5 double speed) falling edge
pub fn apu_tick(ctx: &mut impl ApuContext) {
    ctx.apu_regs_mut().ch1.apu_tick();
    ctx.apu_regs_mut().ch2.apu_tick();
    ctx.apu_regs_mut().ch3.apu_tick();
    ctx.apu_regs_mut().ch4.apu_tick();
}

pub fn tick(ctx: &mut impl ApuContext, tcycles: u64) {
    let mut sample_cursor = ctx.apu().sample_cursor;

    ctx.apu_regs_mut().ch1.check_trigger();
    ctx.apu_regs_mut().ch2.check_trigger();
    ctx.apu_regs_mut().ch3.check_trigger();
    ctx.apu_regs_mut().ch4.check_trigger();

    if ctx.apu().output_period > 0.0 {
        let next_sample = ctx.apu().output_period - sample_cursor;
        if tcycles as f32 > next_sample {
            ctx.apu_regs_mut().ch1.tick(next_sample as u32);
            ctx.apu_regs_mut().ch2.tick(next_sample as u32);
            ctx.apu_regs_mut().ch3.tick(next_sample as u32);
            ctx.apu_regs_mut().ch4.tick(next_sample as u32);

            let left_sample = get_stereo_sample(ctx, false);
            let right_sample = get_stereo_sample(ctx, true);
            let stereo_sample = ctx.apu_mut().hpf.filter_sample((left_sample, right_sample));
            ctx.apu_mut().output_sample = Some(stereo_sample);

            ctx.apu_regs_mut()
                .ch1
                .tick(tcycles as u32 - next_sample as u32);
            ctx.apu_regs_mut()
                .ch2
                .tick(tcycles as u32 - next_sample as u32);
            ctx.apu_regs_mut()
                .ch3
                .tick(tcycles as u32 - next_sample as u32);
            ctx.apu_regs_mut()
                .ch4
                .tick(tcycles as u32 - next_sample as u32);
        } else {
            ctx.apu_regs_mut().ch1.tick(tcycles as u32);
            ctx.apu_regs_mut().ch2.tick(tcycles as u32);
            ctx.apu_regs_mut().ch3.tick(tcycles as u32);
            ctx.apu_regs_mut().ch4.tick(tcycles as u32);
        }

        ctx.apu_mut().sample_cursor = (sample_cursor + tcycles as f32) % ctx.apu().output_period;
    }
}
