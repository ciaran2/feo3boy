use std::f32::consts::PI;
use std::mem;
use std::ops::{Mul, MulAssign};
use std::time::Duration;

use bitflags::bitflags;
use log::{debug, info, trace};

use crate::bits::{ApplyBitGroup, BitGroup};
use crate::clock::{
    cycles_to_duration, ClockSnapshot, ClockSpeed, DCycle, SystemClock, SystemClockContext,
    TimedChangeTracker,
};
use crate::memdev::{MemDevice, RelativeAddr};
use crate::timer::TimerDivider;

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

/// Represents the APU-DIV counter which increments whenever the appropriate DIV bit
/// goes from 1 to 0. This counter is used to trigger various other increments within the
/// APU, including envelope sweep, sound length countdown, and channel 1 frequency sweep.
///
/// It's somewhat unclear how those ticks are triggered from APU-DIV. Given that the
/// lowest one is 2x APU-DIV increment, I choose to treat them as a falling-edge detector
/// for bits 0, 1, and 2 of this register.
#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash)]
pub struct ApuDiv {
    /// Current value of this counter.
    counter: u8,
    /// Value of the counter as of the last APU-DIV tick (so this stays the same across
    /// MTicks between APU-DIV ticks).
    prev_counter: u8,
}

impl ApuDiv {
    /// Increment the counter, saving the previous counter value for falling-edge
    /// detection.
    ///
    /// This method modifies the APU-DIV in-place, but also returns a copy of the updated
    /// value of `self` for convenience.
    fn tick_apu_div(&mut self) -> Self {
        self.prev_counter = mem::replace(&mut self.counter, self.counter.wrapping_add(1));
        *self
    }

    /// Check if the most-recent APU-DIV tick was a falling-edge for the 256 Hz (nominal)
    /// cycle.
    ///
    /// Note that this will return true during all m-cycles between apu_div ticks when it
    /// was true on the last apu_div tick, so it should only be used on the apu-div tick
    /// cycle.
    fn is_256hz_tick(&self) -> bool {
        const BIT0: u8 = 1 << 0;
        self.prev_counter & BIT0 == 1 && self.counter & BIT0 == 0
    }

    /// Check if the most-recent APU-DIV tick was a falling-edge for the 128 Hz (nominal)
    /// cycle.
    ///
    /// Note that this will return true during all m-cycles between apu_div ticks when it
    /// was true on the last apu_div tick, so it should only be used on the apu-div tick
    /// cycle.
    fn is_128hz_tick(&self) -> bool {
        const BIT1: u8 = 1 << 1;
        self.prev_counter & BIT1 == 1 && self.counter & BIT1 == 0
    }

    /// Check if the most-recent APU-DIV tick was a falling-edge for the 64 Hz (nominal)
    /// cycle.
    ///
    /// Note that this will return true during all m-cycles between apu_div ticks when it
    /// was true on the last apu_div tick, so it should only be used on the apu-div tick
    /// cycle.
    fn is_64hz_tick(&self) -> bool {
        const BIT2: u8 = 1 << 2;
        self.prev_counter & BIT2 == 1 && self.counter & BIT2 == 0
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

#[derive(Default, Debug, Clone, PartialEq, Eq, MemDevice)]
#[memdev(byte, read = Self::read, write = Self::write)]
struct Sweep {
    /// Equivalent to the slope control. When wavelength is recomputed due to a sweep-pace
    /// increment completing, this value is used to determine how much to modify the
    /// wavelength by. `L[t+1] = L[t] +/- L[t] / 2^<shift>`, since division by a power of
    /// two is equivalent to shift, we call this shift.
    shift: u8,
    /// If set to 1, sweep subtracts from the wavelength, otherwise it adds.
    sweep_subtract: bool,
    /// Pace in the register visible to the CPU. Changing this does not change the time
    /// remaining in the current sweep iteration until the next time the channel is
    /// triggered or the the next sweep iteration starts.
    ///
    /// Pace is nominally in terms of number of 128 Hz ticks, in actuality it is in
    /// increments of 4 APU-DIV ticks.
    ///
    /// Maximum value is 7, so pace * 4 still fits in a u8.
    pace: u8,
    /// Number of APU-DIV ticks remaining in the current sweep. This is stored in terms of
    /// APU-DIV ticks. We effectively count down from pace * 4 to zero and trigger
    /// whenever we reach zero.
    ticks_in_sweep: u8,
}

impl Sweep {
    const PACE: BitGroup = BitGroup(0b0111_0000);
    const SLOPE_DIR: BitGroup = BitGroup(0b0000_1000);
    const SLOPE_CTRL: BitGroup = BitGroup(0b0000_0111);

    /// Read this as a single byte register.
    fn read(&self) -> u8 {
        0xff.with_bits(Self::PACE, self.pace)
            .with_bits(Self::SLOPE_DIR, self.sweep_subtract as u8)
            .with_bits(Self::SLOPE_CTRL, self.shift)
    }

    /// Write this as a single byte register.
    fn write(&mut self, value: u8) {
        let pace = Self::PACE.extract(value);
        let sweep_subtract = Self::SLOPE_DIR.extract_bool(value);
        let shift = Self::SLOPE_CTRL.extract(value);

        if mem::replace(&mut self.pace, pace) == 0 || pace == 0 {
            // If the old pace was zero or the new pace is zero, the new pace applies
            // immediately. This implements both instantly stopping sweeps when pace is
            // set to 0 and instantly starting sweeps when the pace is updated from 0, but
            // not resetting the current sweep if the pace was non-zero.
            self.start_sweep();
        }
        self.sweep_subtract = sweep_subtract;
        self.shift = shift;
    }

    /// Resets `ticks_in_sweep` to `4 * pace` to start a new sweep.
    fn start_sweep(&mut self) {
        self.ticks_in_sweep = self.pace * 4;
    }

    /// Get the next wavelength, given the current wavelength and sweep direction. Also
    /// returns a boolean value indicating if the sweep should cause a wavelength overflow
    /// (which should turn off the channel). Note that the overflow can still be set even
    /// if `shift == 0`, which otherwise disables sweep.
    fn next_wavelength(&self, wavelength: u16) -> (u16, bool) {
        // Wavelength is 11 bits, so it should be no more than 0x7FF.
        debug_assert!(wavelength <= 0x7FF);
        if self.shift == 0 {
            let would_overflow = wavelength * 2 > 0x7FF;
            (wavelength, would_overflow)
        } else {
            let delta = wavelength >> self.shift;
            let new_wavelength = if self.sweep_subtract {
                wavelength - delta
            } else {
                wavelength + delta
            };
            let clamped_wavelength = new_wavelength & 0x7FF;
            let overflowed = new_wavelength > 0x7FF;
            (clamped_wavelength, overflowed)
        }
    }

    /// Run one apu-div tick on this register. Returns true if the sweep completed.
    fn tick_apu_div(&mut self, apu_div: ApuDiv) -> bool {
        if self.pace == 0 || !apu_div.is_128hz_tick() {
            return false;
        }

        self.ticks_in_sweep -= 1;
        self.ticks_in_sweep == 0
    }
}

/// Length timer for a channel. This is a 6 bit register which counts up at 256 Hz
/// (nominal, tied to APU-DIV) when enabled. When it reaches 64 (i.e. overflows) the
/// channel shuts down.
#[derive(Default, Debug, Clone, Eq, PartialEq, Hash)]
pub struct LengthTimer {
    /// Counter register of the length timer.
    counter: u8,
    /// Whether the length timer is enabled.
    enabled: bool,
}

impl LengthTimer {
    const COUNTER_MASK: u8 = 0x3F;

    /// Sets the value of the timer. The value will be masked to 6 bits.
    fn set_counter(&mut self, val: u8) {
        self.counter = val & Self::COUNTER_MASK;
    }

    /// Sets whether the counter is enabled.
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Get the enabled value.
    fn enabled(&self) -> bool {
        self.enabled
    }

    /// Tick the length counter if it is enabled and this cycle is a 256 Hz (nominal)
    /// cycle of the apu-div. Returns true if the counter has hit 64 (and rolled over).
    fn tick_apu_div(&mut self, apu_div: ApuDiv) -> bool {
        if !self.enabled || !apu_div.is_256hz_tick() {
            return false;
        }
        let inc = self.counter + 1;
        self.counter = inc & Self::COUNTER_MASK;
        inc > Self::COUNTER_MASK
    }
}

/// Values set in the envelope control register as visible to the CPU.
///
/// Changes to the enveolpe don't take effect until the channel is triggered.
#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits)]
#[repr(transparent)]
pub struct EnvelopeControl(u8);

impl EnvelopeControl {
    const INIT_VOL: BitGroup = BitGroup(0b1111_0000);
    const DIRECTION: BitGroup = BitGroup(0b0000_1000);
    const PACE: BitGroup = BitGroup(0b0000_0111);
    /// Bits which control the DAC enable.
    const DAC_ENABLE: BitGroup = BitGroup(0b1111_1000);

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
    pub fn direction_increasing(&self) -> bool {
        Self::DIRECTION.extract_bool(self.0)
    }

    /// Set whether the direction is increasing.
    #[inline]
    pub fn set_direction_increasing(&mut self, val: bool) {
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

    /// Returns true if the bits of the dac_enable portion of the register are non-zero.
    pub fn dac_enabled(&self) -> bool {
        Self::DAC_ENABLE.extract_bool(self.0)
    }
}

/// Internal state of the envelope, not reloaded until the channel is triggered.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Envelope {
    /// Volume level the envelope is set to from 0 to 8.
    level: i8,
    /// True if the envelope is increasing.
    direction: i8,
    /// Pace is the number of APU-DIV 64 Hz (nominal) ticks before a level change.
    pace: u8,
    /// Number of 64 Hz (nominal) ticks elapced at the specified pace. Changes to the
    /// output level will happen when this reaches the value of `pace`.
    elapsed_pace_ticks: u8,
}

impl Envelope {
    fn level(&self) -> u16 {
        self.level as u16
    }

    fn tick_apu_div(&mut self, apu_div: ApuDiv) {
        if self.pace > 0 && apu_div.is_64hz_tick() {
            self.elapsed_pace_ticks += 1;
            if self.elapsed_pace_ticks == self.pace {
                self.elapsed_pace_ticks = 0;
                self.level = (self.level + self.direction).clamp(0x0, 0xf);
            }
        }
    }

    /// Reload the internal envelope control from the register (for a channel trigger).
    fn reload_register(&mut self, reg: &EnvelopeControl) {
        self.level = reg.init_vol();
        self.direction = if reg.direction_increasing() { 1 } else { -1 };
        self.pace = reg.pace();
        self.elapsed_pace_ticks = 0;
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

/// Utility for calculating the phase of a wavelength value at a particular time.
///
/// -   `NUM_STEPS` is the number of steps in the counter that the wavelength system is
///     incrementing, that is 8 for pulse channels and 32 for the wave channel.
///
/// -   `PERIOD_MULTIPLIER` is a multiplier to apply to the period (calculated as 2048 -
///     wavelength) to convert it to a number of D-Cycles. For pulse channels, this is 2,
///     since they step at fractions of 1 MiHz, while for channel 3 this is 1 since it
///     steps at fractions of 2 MiHz.
///
/// This assumes that wavelength works similarly between channel 1/2 and 3. Specifically,
/// we assume that writing a new value to wavelength only takes effect when the previous
/// wavelength step completes. The documentation we have says explicitly that wavelength
/// changes written to channel 3 only apply the next time wave ram is read, but no such
/// thing is specified for channels 1 and 2. Reading the description in SameSuite, it says
/// that channel 1 frequency changes apply 'after the current sample' which sounds like
/// roughtly the same thing, so I think it is safe to assume that both of these work
/// similarly.
///
/// The way wavelength appears to work is that the wavelength value is used as a reset-to
/// point for an 11 bit timer/counter register, and the sample is incremented whenever
/// that counter overflows. After overflow the counter resets to the wavelength value.
/// This neatly explains why changing the wavelength value doesn't take effect
/// immediately; the counter probably only resets on overflow or when the channel is
/// triggered.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct WavelengthCalculator<const NUM_STEPS: u64, const PERIOD_MULTIPLIER: u64> {
    /// The latest wavelength setting.
    wavelength: u16,
    /// The d-cycle when the currently-specified wavelength begins to apply. This is
    /// either the time the channel was triggered or if the wavelength is changed, it is
    /// the time when the previous wavelength count expires. Note that this might be in
    /// the future!
    wavelength_start_time: DCycle,
    /// Value of the counter being incremented as of `wavelength_start_time`. If
    /// `wavelength_start_time <= now`, the counter value is `initial_count +
    /// overflow_count` where `overflow_count` is the number of times the wavelength
    /// counter would have overflowed. If `wavelength_start_time > now`, the count is
    /// exactly `initial_count - 1` The later is wrapped back to `NUM_STEPS - 1` in the
    /// event of an underflow.
    initial_count: u64,

    /// Triggering dirty tracker. When triggered, initial count and start time are reset.
    /// Applies on the next update_if_dirty.
    triggered: bool,

    /// Updated wavelength value. Applies on the next update_if_dirty.
    new_wavelength: Option<u16>,
}

impl<const NUM_STEPS: u64, const PERIOD_MULTIPLIER: u64>
    WavelengthCalculator<NUM_STEPS, PERIOD_MULTIPLIER>
{
    /// Set the wavelength: note update_if_dirty is needed for the value to take effect.
    ///
    /// Triggering will take precedence over this.
    #[inline]
    fn set_wavelength(&mut self, wavelength: u16) {
        debug_assert!(wavelength & !0x07ff == 0, "wavelength had high-order bits set");
        self.new_wavelength = Some(wavelength);
    }

    /// Gets the 16 bit wavelength register. Note: if set_wavelength was called, this will return
    /// the old wavelength until `update_if_dirty` is called.
    #[inline]
    fn wavelength(&self) -> u16 {
        self.wavelength
    }

    /// Set the wavelength triggered. `update_if_dirty` is needed for this to take effect.
    #[inline]
    fn trigger(&mut self) {
        self.triggered = true;
    }

    /// Check if a wavelength update is pending and
    fn update_if_dirty(&mut self, now: ClockSnapshot) {
        if self.triggered {
            // It's fine to reset this to zero because wavelength_start_time is not in the
            // future, so there is no chance of us needing to subtract.
            self.initial_count = 0;
            self.wavelength_start_time = now.elapsed_fixed_cycles();
            if let Some(new_wavelength) = self.new_wavelength.take() {
                self.wavelength = new_wavelength;
            }
            self.triggered = false;
        } else if let Some(new_wavelength) = self.new_wavelength.take() {
            // If the end of the current duty step is in the future, we already changed
            // the wavelength once and computed the time to change to the new wavelength
            // value. We don't need to recompute wavelength_start_time or initial_count,
            // we can just apply the wavelenght value. This also works fine if we are
            // exactly on the cycle when the wavelength is changing.
            //
            // We only need to recompute the changeover cycle if we are in the middle of a
            // duty step, that is if the start time is in the past.
            if self.wavelength_start_time < now.elapsed_fixed_cycles() {
                // The new wavelength_start_time will be the next smallest multiple of the
                // period greater than or equal to now.
                let cycles_since_wavelength_started =
                    now.elapsed_fixed_cycles() - self.wavelength_start_time;
                let period = self.period();
                let increments = cycles_since_wavelength_started.as_u64() / period;
                let current_count = self.initial_count + increments;
                // The time since the start of the current duty step is the remainder after dividing
                // by the period. This is the number of ticks left in
                // `cycles_since_wavelength_started` between when the duty cycle started and `now`.
                let ticks_since_current_duty_step_started =
                    cycles_since_wavelength_started.as_u64() % period;
                // Calculate the number of cycles until the next period start. This is the length of
                // the periiod minus the number of cycles we've already seen in the period. However,
                // if ticks_since_current_duty_step_started is zero, we are already on a duty step
                // boundary right now. We can cover the 'already on a boundary' case by taking this
                // result mod the period length.
                let ticks_until_duty_step_ends =
                    (period - ticks_since_current_duty_step_started) % period;
                // The start time for the new wavelength is the end of the next duty cycle.
                self.wavelength_start_time =
                    now.elapsed_fixed_cycles() + ticks_until_duty_step_ends;
                // The initial count for the new wavelength is the current count if we are already
                // at a boundary, otherwise it's one more than the current count. We apply modulus
                // to keep the initial_count small, though that's probably unnecessary, since we'll
                // never increment this enough to overflow u64.
                self.initial_count = if ticks_until_duty_step_ends == 0 {
                    current_count % NUM_STEPS
                } else {
                    // In this case, we may need to subtract 1 if the duty step is requested before
                    // the new wavelength starts. To make sure we can always subtract 1 without
                    // underflowing, we apply the `+ 1` *after* doing the `% NUM_STEPS` so the value
                    // is always at least 1.
                    current_count % NUM_STEPS + 1
                };
            }
            self.wavelength = new_wavelength;
        }
    }

    /// Compute the current duty step for the wavelength.
    fn duty_step(&self, now: DCycle) -> u64 {
        if self.wavelength_start_time <= now {
            let cycles_since_wavelength_started = now - self.wavelength_start_time;
            // We have effectively incremented the duty step a number of times equivalent
            // to the number of cycles elapsed divided by the number of cycles per
            // increment.
            let increments = cycles_since_wavelength_started.as_u64() / self.period();
            (self.initial_count + increments) % NUM_STEPS
        } else {
            // When subracting, we never need to apply the modulus, since that was done when
            // calculating initial_count.
            self.initial_count - 1
        }
    }

    /// The period is the time between duty step increments, expressed in D-Cycles.
    fn period(&self) -> u64 {
        (2048 - self.wavelength) * PERIOD_MULTIPLIER;
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PulseChannel {
    /// Envelope register. Values here are only picked up when the channel is triggered.
    pub envelope_control: EnvelopeControl,
    /// Currently active envelope.
    envelope: Envelope,
    /// Sweep register and currently active sweep settings.
    sweep: Sweep,

    /// Selected duty cycle. Part of the duty cycle/length register. Stored as an index
    /// into the duty-cycle array.
    duty_cycle: usize,

    /// Length timer. When length is active, this counts up until it reaches 64.
    length_timer: LengthTimer,

    /// Whether the channel is currently active.
    active: bool,

    /// 11-bit register which controls the rate that the channel sweeps through the Pulse
    /// Table. Essentially, the bit selected in the pulse table increments every time a
    /// counter starting from `wavelength` overflows, if incrementing at 1 MiHz (the
    /// standard-speed m-cycle frequency, or once every 2 d-cycles).
    wavelength: WavelengthCalculator<8, 2>,
}

impl PulseChannel {
    // The duty cycle/length register:
    /// Duty cycle portion of the length + duty cycle register.
    const DUTY_CYCLE: BitGroup = BitGroup(0b1100_0000);
    /// Length portion of the length + duty cycle register.
    const LENGTH_TIMER: BitGroup = BitGroup(0b0011_1111);

    // The Wavelength high and control register:
    /// Trigger bit of the wavelength-high/control register.
    const TRIGGER: BitGroup = BitGroup(0b1000_0000);
    /// The length enable portion of the wavelength-high/control register.
    const LENGTH_ENABLE: BitGroup = BitGroup(0b0100_0000);
    /// Wavelength-high portion of the wavelength-high/control register.
    const WAVELENGTH_HIGH: BitGroup = BitGroup(0b0000_0111);

    /// Table representing each possible duty cycle of the pulse channel.
    ///
    /// TODO: Why are these offset? Is it confirmed that these should start/end this way?
    const PULSE_TABLE: [[u16; 8]; 4] = [
        [1, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 1, 1, 0],
        [0, 1, 1, 1, 1, 0, 0, 0],
        [1, 0, 0, 0, 0, 0, 0, 1],
    ];

    /// Return true if the channel is active.
    pub fn active(&self) -> bool {
        self.active
    }

    /// Get the lower byte of the wavelength.
    pub fn wavelength_low(&self) -> u8 {
        let [wavelength_low, _] = self.wavelength.wavelength().to_le_bytes();
        wavelength_low
    }

    /// Set the lower byte of the wavelength. This should only be called from a memory access
    /// through system ram, and generally only via the CPU.
    fn set_wavelength_low(&mut self, wavelength_low: u8) {
        let [_, wavelength_high] = self.wavelength.wavelength().to_le_bytes();
        self.wavelength.set_wavelength(u16::from_le_bytes([wavelength_low, wavelength_high]));
    }

    /// Read the wavelength high bits adn readable control bits.
    pub fn wavelength_high_and_control(&self) -> u8 {
        // length_enable is the only readable bit.
        0xff.with_bits(Self::LENGTH_ENABLE, self.length_timer.enabled())
    }

    /// Set the value of the wavelength high and control register. This should only be called from a
    /// memory access through system ram, and generally only via the CPU.
    fn set_wavelength_high_and_control(&mut self, value: u8) {
        let trigger = Self::TRIGGER.extract_bool(value);
        let length_enable = Self::LENGTH_ENABLE.extract_bool(value);
        let wavelength_high = Self::WAVELENGTH_HIGH.extract(value);
        let [wavelength_low, _] = self.wavelength.wavelength().to_le_bytes();

        self.wavelength.set_wavelength(u16::from_le_bytes([wavelength_low, wavelength_high]));
        self.length_timer.set_enabled(length_enable);
        // Channel is turned on if triggered and the DAC is enabled. If the DAC is
        // disabled, triggering does nothing.
        if trigger && self.envelope_control.dac_enabled() {
            self.triggered.set(());
            self.active = true;
            self.sweep.start_sweep();
            self.envelope.reload_register(&self.envelope_control);
        }
    }

    /// Reads out the value of the combined length and duty-cycle register. Since the
    /// length register is write-only, this only allows viewing the duty cycle.
    pub fn length_and_duty_cycle(&self) -> u8 {
        0xff.with_bits(Self::DUTY_CYCLE, self.duty_cycle as u8)
    }

    /// Set the combined length and duty cycle register.
    fn set_length_and_duty_cycle(&mut self, value: u8) {
        let duty_cycle = Self::DUTY_CYCLE.extract(value);
        let length = Self::LENGTH_TIMER.extract(value);
        self.duty_cycle = duty_cycle as usize;
        self.length_timer.set_counter(length);
    }

    /// Run one tick at the APU-DIV clock rate.
    fn tick_apu_div(&mut self, apu_div: ApuDiv) {
        if self.length_timer.tick_apu_div(apu_div) {
            self.active = false;
        }

        if self.sweep.tick_apu_div(apu_div) {
            let (wavelength, overflowed) = self.sweep.next_wavelength(self.wavelength);
            if overflowed {
                self.wavelength = 0;
                self.active = false;
            } else {
                self.wavelength = wavelength;
                self.sweep.start_sweep();
            }
        }
        self.envelope.tick_apu_div(apu_div);
    }

    /// Update any time-tracking fields with the current clock snapshot.
    fn update_if_dirty(&mut self, now: ClockSnapshot) {
        self.wavelength.update_if_dirty(now);
    }

    /// Get the index into the pulse table for the current time based on when the channel
    /// was last triggered.
    fn pulse_step(&self, now: DCycle) -> usize {
        self.wavelength.duty_step(now) as usize
    }

    /// Get a sample from this channel.
    fn get_sample(&self, now: DCycle) -> f32 {
        // We don't check the DAC here because anytime a memory write disables the DAC, we
        // turn the channel active to false as well, and don't allow a write to turn the
        // channel on unless the DAC is active.
        if self.active {
            let pulse_step = self.pulse_step(now);
            debug!(
                "Sampling pulse channel from step {} of duty cycle {}",
                pulse_step, self.duty_cycle,
            );
            let dac_input = Self::PULSE_TABLE[self.duty_cycle][pulse_step] * self.envelope.level();
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }
}

impl MemDevice for PulseChannel {
    const LEN: usize = 5;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(PulseChannel, addr, |addr| {
            0x00 => self.sweep.read_byte_relative(addr),
            0x01 => self.length_and_duty_cycle(),
            0x02 => self.envelope_control.read_byte_relative(addr),
            0x03 => self.wavelength_low(),
            0x04 => self.wavelength_high_and_control(),
        })
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        dispatch_memdev_byte!(PulseChannel, addr, |addr| {
            0x00 => self.sweep.write_byte_relative(addr, val),
            0x01 => self.set_length_and_duty_cycle(val),
            0x02 => {
                self.envelope_control.write_byte_relative(addr, val);
                if !self.envelope_control.dac_enabled() {
                    // Disabling the DAC disables the channel.
                    self.active = false;
                }
            },
            0x03 => self.set_wavelength_low(val),
            0x04 => self.set_wavelength_high_and_control(val),
        })
    }

    memdev_bytes_from_byte!(PulseChannel);
}

/// Provides the state of the wavetable channel (Channel 3).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct WavetableChannel {
    /// Whether the DAC is enabled. Unlike other channels, this is controlled directly
    /// rather than via some of the envelope bits.
    dac_enable: bool,
    /// Whether the channel is active. The channel can be disabled with the dac still
    /// active, but not the other way around.
    active: bool,

    /// Length timer for the channel. On Channel 3, the length timer is 8 bits long,
    /// however we (currently) treat it as still just 6 bits. It is unclear what happens
    /// if you write a length greater than 64 to the length register on channel 3. Does
    /// that disable it immediately, since the length is already above 64? Or does the
    /// value get truncated and we count up to 64 from there?
    ///
    /// In the current setup we assume the value is truncated to 6 bits.
    length_timer: LengthTimer,

    /// Output level setting (this is the register which would have the envelope function, but
    /// channel 3 has no envelope functionality).
    output_level: u8,

    /// Wavelength position calculator for the location in the sample table.
    wavelength: WavelengthCalculator<32, 1>,

    /// Sample to emit when first triggered.
    initial_sample: u8,

    period: u32,
    phase_offset: u32,
    triggered: bool,
    sample_cursor: u32,
    wavelength: u16,
    level_shift: u8,
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
            info!("Wavetable channel triggered");
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

const HIGH_PASS_CUTOFF: f32 = 200.0;

#[derive(Clone, Debug, Default)]
struct HighPassFilter {
    last_input: Sample,
    last_output: Sample,
    alpha: f32,
}

impl HighPassFilter {
    fn set_sample_rate(&mut self, sample_rate: u32) {
        self.alpha = 1.0 / (2.0 * PI / (sample_rate as f32) * HIGH_PASS_CUTOFF + 1.0);
    }

    fn filter_sample(&mut self, input: Sample) -> Sample {
        let output = self.alpha
            * Sample {
                left: input.left + self.last_output.left - self.last_input.left,
                right: input.right + self.last_output.right - self.last_input.right,
            };

        self.last_input = input;
        self.last_output = output;

        output
    }
}

/// A single audio sample.
#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub struct Sample {
    /// Wave value for the left speaker.
    pub left: f32,
    /// Wave value for the right speaker.
    pub right: f32,
}

impl Mul<f32> for Sample {
    type Output = Sample;

    fn mul(self, rhs: f32) -> Self::Output {
        Sample {
            left: self.left * rhs,
            right: self.right * rhs,
        }
    }
}

impl Mul<Sample> for f32 {
    type Output = Sample;

    #[inline]
    fn mul(self, rhs: Sample) -> Self::Output {
        Sample {
            left: self * rhs.left,
            right: self * rhs.right,
        }
    }
}

impl MulAssign<f32> for Sample {
    #[inline]
    fn mul_assign(&mut self, rhs: f32) {
        *self = *self * rhs;
    }
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

impl ApuRegs {
    /// Tick the APU-DIV given the status of the APU-DIV register as of this apu-div tick
    /// cycle. Should only be called on the cycle when ApuDiv was incremented.
    fn tick_apu_div(&mut self, apu_div: ApuDiv) {
        self.ch1.tick_apu_div(apu_div);
        self.ch2.tick_apu_div(apu_div);
    }

    /// Update any time-tracking registers with the current clock snapshot.
    pub fn update_if_dirty(&mut self, now: ClockSnapshot) {
        self.ch1.update_if_dirty(now);
        self.ch2.update_if_dirty(now);
    }
}

#[derive(Clone, Debug)]
pub struct ApuState {
    /// The most recently generated output audio sample, if any.
    output_sample: Option<Sample>,
    /// Number of samples generated per second.
    sample_rate: u64,
    /// Counter of what sample we are up to. This count is non-cumulative and only tracks
    /// from the last time the sample rate was changed.
    sample: u64,
    /// Time that the sample rate was last changed. When the sample rate is changed, this
    /// gets reset to the SystemClock time for the next cycle after the cycle when the
    /// rate was changed.
    sample_rate_changed_at: Option<Duration>,
    /// Previous value of the bit selected from the DIV register that drives the sound
    /// clock. The sound clock triggers whenever this hits a falling edge. This bit is
    /// slected from divider bit 13 or 14 depending on whether the system is running at
    /// normal or double speed.
    prev_divider_bit: bool,
    /// Internal APU-DIV counter, incremented when APU-DIV changes.
    apu_div: ApuDiv,
    hpf: HighPassFilter,
}

impl ApuState {
    pub fn new() -> Self {
        ApuState {
            // Number of samples saved is low -- the runner should be moving these to the
            // audio system faster than we're generating them.
            output_sample: None,
            sample_rate: 0,
            sample: 0,
            sample_rate_changed_at: Some(Duration::ZERO),
            prev_divider_bit: false,
            apu_div: ApuDiv::default(),
            hpf: HighPassFilter::default(),
        }
    }

    pub fn set_output_sample_rate(&mut self, sample_rate: u32) {
        self.sample_rate = sample_rate as u64;
        self.sample = 0;
        self.sample_rate_changed_at = None;
        self.hpf.set_sample_rate(sample_rate);
    }

    pub fn consume_output_sample(&mut self) -> Option<Sample> {
        self.output_sample.take()
    }

    /// Calculate the time at which the next sample should be generated.
    fn next_sample_time(&self) -> Duration {
        let time_since_changed = cycles_to_duration(self.sample, self.sample_rate);
        let changed_at = self
            .sample_rate_changed_at
            .expect("sample rate change time was not set");
        changed_at + time_since_changed
    }

    /// Update the `sample_rate_changed_at` to be the time of the next sample at the
    /// current sample rate, if it isn't set.
    fn recompute_sample_time_if_needed(&mut self, clock: &SystemClock) {
        if self.sample_rate_changed_at.is_none() {
            self.sample_rate_changed_at = Some(clock.elapsed_time());
        }
    }
}

pub trait ApuContext: SystemClockContext {
    fn apu(&self) -> &ApuState;
    fn apu_mut(&mut self) -> &mut ApuState;

    fn apu_regs(&self) -> &ApuRegs;
    fn apu_regs_mut(&mut self) -> &mut ApuRegs;

    /// Get the timer divider register, needed to determine when the APU clock ticks.
    fn divider(&self) -> &TimerDivider;
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
pub fn tick_apu_div(ctx: &mut impl ApuContext) {
    let apu_div = ctx.apu_mut().apu_div.tick_apu_div();
    ctx.apu_regs_mut().tick_apu_div(apu_div);
}

pub fn tick(ctx: &mut impl ApuContext) {
    let divider_bit_selector = match ctx.clock().speed() {
        ClockSpeed::Normal => 0x2000,
        ClockSpeed::Double => 0x4000,
    };
    let divider_bit = ctx.divider().value() & divider_bit_selector != 0;
    let prev_divider_bit = mem::replace(&mut ctx.apu_mut().prev_divider_bit, divider_bit);
    if prev_divider_bit && !divider_bit {
        tick_apu_div(ctx);
    }

    ctx.apu_regs_mut().ch1.check_trigger();
    ctx.apu_regs_mut().ch2.check_trigger();
    ctx.apu_regs_mut().ch3.check_trigger();
    ctx.apu_regs_mut().ch4.check_trigger();

    if ctx.apu().sample_rate > 0 {
        let next_sample_time = ctx.apu().next_sample_time();
        let next_sample = ctx.apu().output_period - sample_cursor;
        if tcycles as f32 > next_sample {
            ctx.apu_regs_mut().ch1.tick(next_sample as u32);
            ctx.apu_regs_mut().ch2.tick(next_sample as u32);
            ctx.apu_regs_mut().ch3.tick(next_sample as u32);
            ctx.apu_regs_mut().ch4.tick(next_sample as u32);

            let sample = Sample {
                left: get_stereo_sample(ctx, false),
                right: get_stereo_sample(ctx, true),
            };
            let stereo_sample = ctx.apu_mut().hpf.filter_sample(sample);
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
    }
}
