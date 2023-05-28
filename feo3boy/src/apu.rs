use std::f32::consts::PI;
use std::ops::{Mul, MulAssign};
use std::time::Duration;
use std::{fmt, mem};

use bitflags::bitflags;
use log::{debug, warn};

use crate::bits::{ApplyBitGroup, BitGroup};
use crate::clock::{cycles_to_duration, ClockSpeed, DCycle, SystemClockContext};
use crate::memdev::{MemDevice, ReadCtx, RelativeAddr, WriteCtx};
use crate::timer::TimerDivider;

mod lfsr;

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

impl ChannelMix {
    /// Return the channel mix multiplier for the specified channel. Returns 0.25 if `self` contains
    /// the specified channel, otherwise returns 0.0.
    fn get_channel_mix_multiplier(self, channel: ChannelMix) -> f32 {
        if self.contains(channel) {
            0.25
        } else {
            0.0
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum SoundSide {
    Left,
    Right,
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
    fn channel_mix(&self, side: SoundSide) -> ChannelMix {
        ChannelMix::from_bits_truncate(match side {
            SoundSide::Left => self.bits() >> 4,
            SoundSide::Right => self.bits(),
        })
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
        self.prev_counter = self.counter;
        self.counter = self.counter.wrapping_add(1);
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

    /// Get the volumen multiplier.
    fn vol_multiplier(&self, side: SoundSide) -> f32 {
        let vol_digital = match side {
            SoundSide::Left => self.vol_left(),
            SoundSide::Right => self.vol_right(),
        };
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
    pub fn set(&mut self, val: u8) {
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
    /// Last time the wavelength was triggered. This is used to tell if we are currently in the
    /// first cycle.
    triggered_time: DCycle,
    /// Length of the initial period in d-cycles.
    initial_period: DCycle,
}

impl<const NUM_STEPS: u64, const PERIOD_MULTIPLIER: u64> Default
    for WavelengthCalculator<NUM_STEPS, PERIOD_MULTIPLIER>
{
    fn default() -> Self {
        Self::new()
    }
}

impl<const NUM_STEPS: u64, const PERIOD_MULTIPLIER: u64>
    WavelengthCalculator<NUM_STEPS, PERIOD_MULTIPLIER>
{
    /// Create a new WavelenghtCalculator with the wavelength set to zero.
    const fn new() -> Self {
        Self {
            wavelength: 0,
            wavelength_start_time: DCycle::ZERO,
            initial_count: 0,
            triggered_time: DCycle::ZERO,
            // The period should always be reset when we trigger the wavelength for the first time,
            // but starting with a non-zero value helps with certain unit tests and improves
            // consistency.
            initial_period: DCycle::new(2048 * PERIOD_MULTIPLIER),
        }
    }

    /// Set the wavelength, recalcuating the start time and initial count.
    #[inline]
    fn set_wavelength(&mut self, now: DCycle, wavelength: u16) {
        debug_assert!(
            wavelength & !0x07ff == 0,
            "wavelength had high-order bits set"
        );

        // If the end of the current duty step is in the future, we already changed
        // the wavelength once and computed the time to change to the new wavelength
        // value. We don't need to recompute wavelength_start_time or initial_count,
        // we can just apply the wavelenght value. This also works fine if we are
        // exactly on the cycle when the wavelength is changing.
        //
        // We only need to recompute the changeover cycle if we are in the middle of a
        // duty step, that is if the start time is in the past.
        if self.wavelength_start_time < now {
            // The new wavelength_start_time will be the next smallest multiple of the
            // period greater than or equal to now.
            let cycles_since_wavelength_started = now - self.wavelength_start_time;
            let period = self.period();
            let increments = cycles_since_wavelength_started.as_u64() / period.as_u64();
            let current_count = self.initial_count + increments;
            // The time since the start of the current duty step is the remainder after dividing
            // by the period. This is the number of ticks left in
            // `cycles_since_wavelength_started` between when the duty cycle started and `now`.
            let ticks_since_current_duty_step_started =
                cycles_since_wavelength_started.as_u64() % period.as_u64();
            // Calculate the number of cycles until the next period start. This is the length of
            // the periiod minus the number of cycles we've already seen in the period. However,
            // if ticks_since_current_duty_step_started is zero, we are already on a duty step
            // boundary right now. We can cover the 'already on a boundary' case by taking this
            // result mod the period length.
            let ticks_until_duty_step_ends =
                (period - ticks_since_current_duty_step_started).as_u64() % period.as_u64();
            // The start time for the new wavelength is the end of the next duty cycle.
            self.wavelength_start_time = now + ticks_until_duty_step_ends;
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
        self.wavelength = wavelength;

        // If we updated the wavelength at the same time that the wavelength was triggered, we
        // should change the initial_period, which is used to tell if we are still on the first
        // cycle.
        if self.triggered_time == now {
            self.initial_period = self.period();
        }
    }

    /// Gets the 16 bit wavelength register.
    #[inline]
    fn wavelength(&self) -> u16 {
        self.wavelength
    }

    /// Set the wavelength triggered.
    #[inline]
    fn trigger(&mut self, now: DCycle) {
        self.initial_count = 0;
        self.wavelength_start_time = now;
        self.triggered_time = now;
        self.initial_period = self.period();
    }

    /// Compute the current duty step for the wavelength.
    fn duty_step(&self, now: DCycle) -> u64 {
        if self.wavelength_start_time <= now {
            let cycles_since_wavelength_started = now - self.wavelength_start_time;
            // We have effectively incremented the duty step a number of times equivalent
            // to the number of cycles elapsed divided by the number of cycles per
            // increment.
            let increments = cycles_since_wavelength_started.as_u64() / self.period().as_u64();
            (self.initial_count + increments) % NUM_STEPS
        } else {
            // When subracting, we never need to apply the modulus, since that was done when
            // calculating initial_count.
            self.initial_count - 1
        }
    }

    /// The period is the time between duty step increments, expressed in D-Cycles.
    fn period(&self) -> DCycle {
        DCycle::new((2048 - self.wavelength as u64) * PERIOD_MULTIPLIER)
    }

    /// Return true if we are in the first cycle. This is used by Channel 3 to determine if it
    /// should emit its old initial sample.
    fn is_first_duty_cycle(&self, now: DCycle) -> bool {
        now < self.triggered_time + self.initial_period
    }

    /// Get the duty step as-of 1 d-cycle ago, or none if we are in the first duty cycle.
    ///
    /// This is used by channel 3 to set `initial_step` when it is being shut down so that it plays
    /// the correct initial sample the next time it is triggered.
    ///
    /// Important! This should never be used on the same cycle as `set_wavelength`. If that happens,
    /// it may return an incorrect duty step value because `duty_step()` assumes that time never
    /// runs backwards. As long as wavelength is only written through memory, that should never
    /// happen in practice, as turning off the channel (disabling the dac or the whole audio system)
    /// requires a write to a different memory location than the wavelength, so that should be
    /// sufficient to guarantee that there is at least one cycle between those.
    fn get_last_duty_step(&self, now: DCycle) -> Option<u64> {
        if self.is_first_duty_cycle(now) {
            // There is no previous duty step if we are on the first step.  In this case, channel 3
            // should not update its initial_step, since no new sample has been read.
            None
        } else {
            Some(self.duty_step(now - 1))
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PulseChannel {
    /// Envelope register. Values here are only picked up when the channel is triggered.
    envelope_control: EnvelopeControl,
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

    /// Set the lower byte of the wavelength. This should only be called from a memory access
    /// through system ram, and generally only via the CPU.
    fn set_wavelength_low(&mut self, now: DCycle, wavelength_low: u8) {
        let [_, wavelength_high] = self.wavelength.wavelength().to_le_bytes();
        self.wavelength
            .set_wavelength(now, u16::from_le_bytes([wavelength_low, wavelength_high]));
    }

    /// Only the length bit is readable, so this is only intended for implementing memdev.
    fn wavelength_high_and_control(&self) -> u8 {
        // length_enable is the only readable bit.
        0xff.with_bits(Self::LENGTH_ENABLE, self.length_timer.enabled() as u8)
    }

    /// Set the value of the wavelength high and control register. This should only be called from a
    /// memory access through system ram, and generally only via the CPU.
    fn set_wavelength_high_and_control(&mut self, now: DCycle, value: u8) {
        let trigger = Self::TRIGGER.extract_bool(value);
        let length_enable = Self::LENGTH_ENABLE.extract_bool(value);
        let wavelength_high = Self::WAVELENGTH_HIGH.extract(value);
        let [wavelength_low, _] = self.wavelength.wavelength().to_le_bytes();

        self.wavelength
            .set_wavelength(now, u16::from_le_bytes([wavelength_low, wavelength_high]));
        self.length_timer.set_enabled(length_enable);
        // Channel is turned on if triggered and the DAC is enabled. If the DAC is
        // disabled, triggering does nothing.
        if trigger && self.envelope_control.dac_enabled() {
            self.wavelength.trigger(now);
            self.active = true;
            self.sweep.start_sweep();
            self.envelope.reload_register(&self.envelope_control);
        }
    }

    /// Reads out the value of the combined length and duty-cycle register. Since the
    /// length register is write-only, this only allows viewing the duty cycle.
    fn length_and_duty_cycle(&self) -> u8 {
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
    fn tick_apu_div(&mut self, now: DCycle, apu_div: ApuDiv) {
        if self.length_timer.tick_apu_div(apu_div) {
            self.active = false;
        }

        if self.sweep.tick_apu_div(apu_div) {
            let (wavelength, overflowed) = self.sweep.next_wavelength(self.wavelength.wavelength());
            if overflowed {
                self.wavelength.set_wavelength(now, 0);
                self.active = false;
            } else {
                self.wavelength.set_wavelength(now, wavelength);
                self.sweep.start_sweep();
            }
        }
        self.envelope.tick_apu_div(apu_div);
    }

    /// Get a sample from this channel.
    fn get_sample(&self, now: DCycle) -> f32 {
        // We don't check the DAC here because anytime a memory write disables the DAC, we
        // turn the channel active to false as well, and don't allow a write to turn the
        // channel on unless the DAC is active.
        if self.active {
            let pulse_step = self.wavelength.duty_step(now) as usize;
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

    fn read_byte_relative(&self, ctx: &ReadCtx, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(PulseChannel, addr, |addr| {
            0x00 => self.sweep.read_byte_relative(ctx, addr),
            0x01 => self.length_and_duty_cycle(),
            0x02 => self.envelope_control.read_byte_relative(ctx, addr),
            0x03 => 0xff, // wavelength is write-only.
            0x04 => self.wavelength_high_and_control(),
        })
    }

    fn write_byte_relative(&mut self, ctx: &WriteCtx, addr: RelativeAddr, val: u8) {
        dispatch_memdev_byte!(PulseChannel, addr, |addr| {
            0x00 => self.sweep.write_byte_relative(ctx, addr, val),
            0x01 => self.set_length_and_duty_cycle(val),
            0x02 => {
                self.envelope_control.write_byte_relative(ctx, addr, val);
                if !self.envelope_control.dac_enabled() {
                    // Disabling the DAC disables the channel.
                    self.active = false;
                }
            },
            0x03 => self.set_wavelength_low(ctx.atime().elapsed_fixed_cycles(), val),
            0x04 => self.set_wavelength_high_and_control(ctx.atime().elapsed_fixed_cycles(),val),
        })
    }

    memdev_bytes_from_byte!(PulseChannel);
}

/// Provides the state of the wavetable channel (Channel 3).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct WavetableChannel {
    /// Whether the DAC is enabled. Unlike other channels, this is controlled directly
    /// rather than via some of the envelope bits.
    dac_enabled: bool,
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
    ///
    /// The supported levels are mute, 100%, 50%, and 25%, so this is stored as a right-shift (mute
    /// is implemented by a shift of 4, which works since samples are at most 4 bits).
    output_level: u32,

    /// Wavelength position calculator for the location in the sample table.
    wavelength: WavelengthCalculator<32, 1>,

    /// Sample to emit when first triggered.
    initial_sample: u8,

    /// Contains sample values which have been split in half to allow nybbles to be
    /// efficiently indexed when sampling.
    sample_table: [u8; 32],
}

impl WavetableChannel {
    /// Bit of the dac enable register which is actually used to control the DAC.
    const DAC_ENABLED: BitGroup = BitGroup(0b1000_0000);

    /// Level selection. (Would be envelope in other channels).
    const LEVEL: BitGroup = BitGroup(0b0110_0000);

    // The Wavelength high and control register:
    /// Trigger bit of the wavelength-high/control register.
    const TRIGGER: BitGroup = BitGroup(0b1000_0000);
    /// The length enable portion of the wavelength-high/control register.
    const LENGTH_ENABLE: BitGroup = BitGroup(0b0100_0000);
    /// Wavelength-high portion of the wavelength-high/control register.
    const WAVELENGTH_HIGH: BitGroup = BitGroup(0b0000_0111);

    /// Read the value of the DAC enable register for a memory read.
    fn dac_enabled_reg(&self) -> u8 {
        0xff.with_bits(Self::DAC_ENABLED, self.dac_enabled as u8)
    }

    /// Update the dac_enabled value.
    fn set_dac_enabled(&mut self, now: DCycle, val: u8) {
        let dac_enabled = Self::DAC_ENABLED.extract_bool(val);
        // If we are now turning off the dac (and the channel was active), we need to capture the
        // last-read sample into initial_sample for next time.
        if !dac_enabled {
            if self.dac_enabled && self.active {
                // If no index is returned, we haven't read a new sample since the last time we
                // started, so we shouldn't update the initial_sample.
                if let Some(index) = self.wavelength.get_last_duty_step(now) {
                    self.initial_sample = self.sample_table[index as usize];
                }
            }
            self.active = false;
        }
        self.dac_enabled = dac_enabled;
    }

    /// Get the value of the level register.
    fn level_reg(&self) -> u8 {
        // Unsure what the other bits should read as. Since most things seem to default to 1 if not
        // connected, we're using 0xff like most other places.
        0xff.with_bits(Self::LEVEL, match self.output_level {
            4 => 0b00,
            0 => 0b01,
            1 => 0b10,
            2 => 0b11,
            _ => panic!("Invalid level setting, allowed values are 4 (mute), 0 (100%), 1 (50%), and 2 (25%)."),
        })
    }

    /// Set the level value.
    fn set_level(&mut self, val: u8) {
        self.output_level = match Self::LEVEL.extract(val) {
            0b00 => 4,
            0b01 => 0,
            0b10 => 1,
            0b11 => 2,
            _ => unreachable!(),
        };
    }

    /// Set the lower byte of the wavelength. This should only be called from a memory access
    /// through system ram, and generally only via the CPU.
    fn set_wavelength_low(&mut self, now: DCycle, wavelength_low: u8) {
        let [_, wavelength_high] = self.wavelength.wavelength().to_le_bytes();
        self.wavelength
            .set_wavelength(now, u16::from_le_bytes([wavelength_low, wavelength_high]));
    }

    /// Read the wavelength high bits and readable control bits. Note: only the length enable bit is
    /// readable. This is only intended for implementing memdev.
    fn wavelength_high_and_control(&self) -> u8 {
        // length_enable is the only readable bit.
        0xff.with_bits(Self::LENGTH_ENABLE, self.length_timer.enabled() as u8)
    }

    /// Set the value of the wavelength high and control register. This should only be called from a
    /// memory access through system ram, and generally only via the CPU.
    fn set_wavelength_high_and_control(&mut self, now: DCycle, value: u8) {
        let trigger = Self::TRIGGER.extract_bool(value);
        let length_enable = Self::LENGTH_ENABLE.extract_bool(value);
        let wavelength_high = Self::WAVELENGTH_HIGH.extract(value);
        let [wavelength_low, _] = self.wavelength.wavelength().to_le_bytes();

        self.wavelength
            .set_wavelength(now, u16::from_le_bytes([wavelength_low, wavelength_high]));
        self.length_timer.set_enabled(length_enable);
        // Channel is turned on if triggered and the DAC is enabled. If the DAC is
        // disabled, triggering does nothing.
        if trigger && self.dac_enabled {
            self.wavelength.trigger(now);
            self.active = true;
        }
    }

    /// Read the sample pair at the given address relative to the start of the sample table.
    fn get_sample_pair(&self, samples: RelativeAddr) -> u8 {
        let base = samples.index() * 2;
        (self.sample_table[base] << 4) | self.sample_table[base + 1]
    }

    /// Set the sample pair at the given address relative to the start of the sample table.
    fn set_sample_pair(&mut self, samples: RelativeAddr, value: u8) {
        let base = samples.index() * 2;
        self.sample_table[base] = (value & 0xf0) >> 4;
        self.sample_table[base + 1] = value & 0xf;
    }

    /// Run one tick at the APU-DIV clock rate.
    fn tick_apu_div(&mut self, now: DCycle, apu_div: ApuDiv) {
        if self.length_timer.tick_apu_div(apu_div) {
            // When we stop the channel due to length expired, we need to update the initial_sample
            // for next time the channel is triggered.

            // If no index is returned, we haven't read a new sample since the last time we started,
            // so we shouldn't update the initial_sample.
            if let Some(index) = self.wavelength.get_last_duty_step(now) {
                self.initial_sample = self.sample_table[index as usize];
            }
            self.active = false;
        }
    }

    /// Get a sample from this channel.
    fn get_sample(&self, now: DCycle) -> f32 {
        // When dac gets disabled, we also deactivate the channel, so no need to check dac_enable.
        if self.active {
            let dac_input = if self.wavelength.is_first_duty_cycle(now) {
                self.initial_sample
            } else {
                let wavetable_step = self.wavelength.duty_step(now) as usize;
                self.sample_table[wavetable_step]
            };
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }
}

impl MemDevice for WavetableChannel {
    const LEN: usize = 0x15;

    fn read_byte_relative(&self, _ctx: &ReadCtx, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(WavetableChannel, addr, |addr| {
            0x00 => self.dac_enabled_reg(),
            0x01 => 0xff, // length timer is write-only.
            0x02 => self.level_reg(),
            0x03 => 0xff, // wavelength is write-only.
            0x04 => self.wavelength_high_and_control(),
            // Channel "samples" block. ApuRegs will remap this portion to the correct
            // offset.
            0x05..=0x14 => self.get_sample_pair(addr),
        })
    }

    fn write_byte_relative(&mut self, ctx: &WriteCtx, addr: RelativeAddr, val: u8) {
        dispatch_memdev_byte!(WavetableChannel, addr, |addr| {
            0x00 => self.set_dac_enabled(ctx.atime().elapsed_fixed_cycles(), val),
            0x01 => self.length_timer.set_counter(val),
            0x02 => self.set_level(val),
            0x03 => self.set_wavelength_low(ctx.atime().elapsed_fixed_cycles(), val),
            0x04 => self.set_wavelength_high_and_control(ctx.atime().elapsed_fixed_cycles(), val),
            // Channel "samples" block. ApuRegs will remap this portion to the correct
            // offset.
            0x05..=0x15 => self.set_sample_pair(addr, val),
        })
    }

    memdev_bytes_from_byte!(WavetableChannel);
}

/// A trait for types which implement the LFSR used in the noise channel.
pub trait LinearFeedbackShiftRegister: Clone + fmt::Debug + Default + PartialEq + Eq {
    /// Apply the 'increment/shift' operation of the LFSR the given number of times and return bit 0
    /// at the end of the operation. The output is always either 0 or 1.
    fn increment_by_and_get_output(&mut self, increments: u64) -> u16;

    /// Return true if the LFSR is using short mode.
    fn short(&self) -> bool;

    /// Set the width mode of the LFSR.
    fn set_short(&mut self, short_width: bool);

    /// Reset the LFSR.
    fn reset(&mut self);
}

/// Directly implements the LFSR by doing the actual shifts.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct DirectLinearFeedbackShiftRegister {
    /// Whether to use the shorter width (7 bits) instead of 15 bits.
    short_width: bool,

    /// The contents of the LFSR register.
    value: u16,
}

impl LinearFeedbackShiftRegister for DirectLinearFeedbackShiftRegister {
    fn increment_by_and_get_output(&mut self, increments: u64) -> u16 {
        /// Take an applier function and initial value and return the final value after n
        /// increments.
        ///
        /// The applier function should only set the relevant bits, not shift the result down.
        #[inline]
        fn do_increments<F: Fn(u16, u16) -> u16>(apply: F, mut value: u16, increments: u64) -> u16 {
            for _ in 0..increments {
                // the bit is the nxor of the last two bits.
                let bit = (!(((value & 0b10) >> 1) ^ (value & 0b01))) & 1;
                // Apply the bit and shift down.
                value = apply(value, bit) >> 1;
            }
            value
        }

        #[inline(always)]
        fn apply_long(value: u16, bit: u16) -> u16 {
            (value & 0x7fff) | (bit << 15)
        }

        #[inline(always)]
        fn apply_short(value: u16, bit: u16) -> u16 {
            (value & 0x7f7f) | (bit << 15) | (bit << 7)
        }

        // This ensures the conditional is not applied within a tight loop.
        self.value = if self.short_width {
            do_increments(apply_short, self.value, increments)
        } else {
            do_increments(apply_long, self.value, increments)
        };
        self.value & 1
    }

    fn short(&self) -> bool {
        self.short_width
    }

    fn set_short(&mut self, short_width: bool) {
        self.short_width = short_width;
    }

    fn reset(&mut self) {
        self.value = 0;
    }
}

/// Represents the LFSR used in the Noise channel.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct TabledLinearFeedbackShiftRegister {
    /// Whether to use the shorter width (7 bits) instead of 15 bits.
    short_width: bool,

    /// Bits from the upper portion of the LFSR which need to be reapplied when switching from 7
    /// bits to 15 bits, if we haven't gone enough steps for them to all be shifted away.
    saved_high_bits: u16,
    /// Increment during which we changed from wide to short. This is used only to determine how
    /// many bits from `saved_high_bits` should be reapplied when changing from 7 bit mode back to
    /// 15 bit mode.
    changed_increment: u64,

    /// Offset applied to elapsed_increments before indexing into the LFSR table. This is used to
    /// adjust for changes between 15 bit mode and 7 bit mode.
    index_offset: u64,
    /// Number of increments that have elapsed.
    elapsed_increments: u64,

    /// If locked, the LFSR is in a state where it can only emit 1 (which is not covered by the LFSR
    /// table).
    locked: bool,
}

impl TabledLinearFeedbackShiftRegister {
    /// Get the current LFSR15 index.
    fn idx15(&self) -> usize {
        ((self.elapsed_increments + self.index_offset) % lfsr::LFSR15_PATTERN.len() as u64) as usize
    }

    /// Get the current LFSR7 index.
    fn idx7(&self) -> usize {
        ((self.elapsed_increments + self.index_offset) % lfsr::LFSR7_PATTERN.len() as u64) as usize
    }
}

impl LinearFeedbackShiftRegister for TabledLinearFeedbackShiftRegister {
    /// Increment the LFSR `increments` number of times.
    ///
    /// The return value will be either 0 or 1.
    fn increment_by_and_get_output(&mut self, increments: u64) -> u16 {
        self.elapsed_increments += increments;
        if self.locked {
            1
        } else if self.short_width {
            lfsr::LFSR7_PATTERN[self.idx7()] & 0x01
        } else {
            lfsr::LFSR15_PATTERN[self.idx15()] & 0x01
        }
    }

    fn short(&self) -> bool {
        self.short_width
    }

    fn set_short(&mut self, short_width: bool) {
        if short_width != self.short_width {
            if short_width {
                if self.locked {
                    // If we were locked at 15 bits, we are still going to be locked at 7 bits, and
                    // don't need to change anything, but to be sure we convert back correctly
                    // later, we'll store the locked bit pattern in saved_high_bits and update the
                    // changed_increment.
                    self.changed_increment = self.elapsed_increments;
                    self.saved_high_bits = 0x7f80;
                } else {
                    // Changing from wide to short, so we need to recalculate the index_offset such
                    // that the current value of `elapsed_increments` has the same value for the
                    // 7-bit secton as we have for the 15-bit section.

                    // Get the current 15 bit value.
                    let value = lfsr::LFSR15_PATTERN[self.idx15()];

                    // Store information about the high bits from the 15 bit register so we can
                    // restore them later if needed.
                    self.changed_increment = self.elapsed_increments;
                    self.saved_high_bits = value & 0x7f80;

                    // Get just the last bits of the 7-bit portion. This is the relevant part and is
                    // used in the reverse lookup table to figure out where we are in the 7-bit
                    // sequence.
                    let masked_lower_bits = value & 0x007f;

                    match lfsr::LFSR7_REVERSE_LOOKUP.get(masked_lower_bits as usize) {
                        // If the index is out of bounds, the value is all 1 and will cause us to be
                        // locked in 7 bit mode.
                        None => {
                            self.locked = true;
                        }
                        Some(&idx7) => {
                            let pat_len = lfsr::LFSR7_PATTERN.len() as u64;
                            let idx7 = idx7 as u64;
                            // Find an index_offset which, when added to the current increment
                            // produces the correct current index into the LFSR7_PATTERN.
                            //
                            // Basically, we're trying to solve `index = elapsed + offset` but mod
                            // pat_len, which is `index - elapsed = offset`. Since `index` and
                            // `offset` are `% pat_len`, we adjust the formula to avoid underflow.
                            //
                            // Essentially, we add pat_len to idx7 to make sure it is large enough
                            // to subtract from, and then mod elapsed by `pat_len` to ensure it is
                            // smaller than `idx7 + pat_len`. This produces an index_offset which is
                            // somewhere in the range `1..(2 * pat_len)`.
                            //
                            // Since mod will be applied again when calculating the final index, we
                            // don't need to apply mod to index_offset again here.
                            self.index_offset =
                                idx7 + pat_len - (self.elapsed_increments % pat_len);
                        }
                    };
                }
            } else {
                // Changing from 7 to 15 can result in unlocking if done soon enough!

                // Get the current value of the LFSR assuming we have shifted enough times that bits
                // from 15 bit mode have all been shifted away. We will later reapply bits from 15
                // bit mode if necessary.
                let base_value = if self.locked {
                    0x7fff
                } else {
                    lfsr::LFSR7_PATTERN[self.idx7()]
                };

                // Amount that saved_high_bits have been shifted down.
                let amount_shifted = self.elapsed_increments - self.changed_increment;
                // Figure out the actual value of the LFSR by reapplying the saved_high_bits.
                let value = if amount_shifted < 8 {
                    let reapplied = (self.saved_high_bits >> amount_shifted) & 0x7f8;
                    let reapply_mask = (0x7f8 >> amount_shifted) & 0x7f8;
                    (base_value & !reapply_mask) | (reapplied & reapply_mask)
                } else {
                    base_value
                };

                match lfsr::LFSR15_REVERSE_LOOKUP.get(value as usize) {
                    None => {
                        self.locked = true;
                    }
                    Some(&idx15) => {
                        let pat_len = lfsr::LFSR15_PATTERN.len() as u64;
                        let idx15 = idx15 as u64;
                        // Find an index_offset which, when added to the current increment
                        // produces the correct current index into the LFSR15_PATTERN.
                        //
                        // The math here works exactly the same as for 7 bit mode.
                        self.index_offset = idx15 + pat_len - (self.elapsed_increments % pat_len);
                    }
                }
            }
        }
    }

    fn reset(&mut self) {
        // We don't change the long/short setting but do reset all the fields used in calculating
        // the current value (to set the output back to 0).

        // The changed_increment and saved_high_bits need to be reset in case we are in 7 bit mode
        // and switch back to 15 bit mode on the firs cycle, so the zeroes from 15 bit mode will be
        // reapplied.
        self.changed_increment = 0;
        self.saved_high_bits = 0;
        self.elapsed_increments = 0;
        self.index_offset = 0;
        self.locked = false;
    }
}

/// The noise channel (Channel 4).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct NoiseChannel<LFSR> {
    /// Timer that controls turning off the noise channel.
    length_timer: LengthTimer,

    /// Envelope register. Values here are only picked up when the channel is triggered.
    envelope_control: EnvelopeControl,
    /// Currently active envelope.
    envelope: Envelope,

    /// The LFSR which determines the actual output of the channel.
    lfsr: LFSR,

    /// Shift applied when determining the period.
    clock_shift: u8,
    /// Divider used when determining the period.
    clock_divider: u8,

    /// DCycle when the currently set period (based on clock shift and divider) should take/took
    /// effect. Note that many periods at this period length may have elapsed since this time, this
    /// is just the time used as the basis for calculing how many steps have elapsed.
    period_start_time: DCycle,
    /// Value of the counter as of `period_start_time`. If `period_start_time <= now`, the counter
    /// value is `initial_increments + times_incremented`, where `times_incremented` is the number
    /// of times the counter would have incremented since `period_start_time` at the current period
    /// length. If `period_start_time > now`, the count is exactly `initial_increments - 1`.
    initial_increments: u64,
    /// This is the number of increments passed to the LFSR. This is used to control how much the
    /// LFSR is advanced whenever a sample is read.
    consumed_increments: u64,

    /// Whether the channel is currently active.
    active: bool,
}

impl<LFSR: LinearFeedbackShiftRegister> NoiseChannel<LFSR> {
    // Noise Control Register Fields.
    /// Clock shift field of the noise control register.
    const CLOCK_SHIFT: BitGroup = BitGroup(0b1111_0000);
    /// LFSR width field of the noise control register.
    const LFSR_WIDTH: BitGroup = BitGroup(0b0000_1000);
    /// Clock div field of the noise control register.
    const CLOCK_DIV: BitGroup = BitGroup(0b0000_0111);

    // The control register:
    /// Trigger bit of the control register.
    const TRIGGER: BitGroup = BitGroup(0b1000_0000);
    /// The length enable portion of the control register.
    const LENGTH_ENABLE: BitGroup = BitGroup(0b0100_0000);

    /// Get the value of the noise control register.
    ///
    /// Only intended for implementing memory access.
    fn noise_control(&self) -> u8 {
        0xff.with_bits(Self::CLOCK_SHIFT, self.clock_shift)
            .with_bits(Self::LFSR_WIDTH, self.lfsr.short() as u8)
            .with_bits(Self::CLOCK_DIV, self.clock_divider)
    }

    /// Set the value of the noise control register.
    ///
    /// Only intended for implementing memory access.
    fn set_noise_control(&mut self, now: DCycle, value: u8) {
        let clock_shift = Self::CLOCK_SHIFT.extract(value);
        let short = Self::LFSR_WIDTH.extract_bool(value);
        let clock_div = Self::CLOCK_DIV.extract(value);

        self.lfsr.set_short(short);

        // We need to recalculate the period start time and initial number of increments for the new
        // shift/div.
        // The logic here is very similar to what is used in WavelengthCalculator, just the way we
        // compute the period is different.
        //
        // If the end of the current period is in the future, we already changed the period once and
        // computed the time to switch over, so we just need to update to the new period. This is
        // also fine if we are exactly on the cycle when the period is changing.
        //
        // We only need to recompute the changeover cycle if we are in the middle of a period, that
        // is if the start time for the current period is in the past.
        if self.period_start_time < now {
            // The new period_start_time will be the next smallest multiple of the period greater
            // than or equal to now.
            let cycles_since_period_started = now - self.period_start_time;
            let period = self.period();
            let increments = cycles_since_period_started.as_u64() / period.as_u64();
            let current_increments = self.initial_increments + increments;

            // The time since the start of the current increment is the remainder after dividing
            // by the period. This is the number of ticks left in
            // `cycles_since_period_started` between when the last increment happened and `now`.
            let ticks_since_current_increment_started =
                cycles_since_period_started.as_u64() % period.as_u64();
            // Calculate the number of cycles until the next period start. This is the length of
            // the periiod minus the number of cycles we've already seen in the period. However,
            // if ticks_since_current_increment_started is zero, we are already on an increment
            // boundary right now. We can cover the 'already on a boundary' case by taking this
            // result mod the period length.
            let ticks_until_increment_ends =
                (period - ticks_since_current_increment_started).as_u64() % period.as_u64();

            // The start time for the new period is the end of the current increment.
            self.period_start_time = now + ticks_until_increment_ends;

            // The initial number of increments for the new period is the current value if this
            // happens on a period boundary, otherwise it is one more than the current nubmer of
            // increments if the new period starts in the future. If the number of increments is
            // requested before the new period starts, the value will be `initial_increments - 1`.
            self.initial_increments = current_increments;
            if ticks_until_increment_ends > 0 {
                self.initial_increments += 1;
            }
        }
        self.clock_shift = clock_shift;
        self.clock_divider = clock_div;
        // We don't update consumed increments until those increments are actually sent to the LFSR.
    }

    /// Get the readable portion of the control register.
    ///
    /// Only intended for implementing memory access.
    fn control(&self) -> u8 {
        // Only length_enable is readable.
        0xff.with_bits(Self::LENGTH_ENABLE, self.length_timer.enabled() as u8)
    }

    /// Set the control register.
    ///
    /// Only intended for implementing memory access.
    fn set_control(&mut self, now: DCycle, value: u8) {
        let trigger = Self::TRIGGER.extract_bool(value);
        let length_enable = Self::LENGTH_ENABLE.extract_bool(value);

        self.length_timer.set_enabled(length_enable);

        // Channel is turned on if triggered and the DAC is enabled. If the DAC is
        // disabled, triggering does nothing.
        if trigger && self.envelope_control.dac_enabled() {
            self.active = true;
            self.envelope.reload_register(&self.envelope_control);
            self.lfsr.reset();
            self.consumed_increments = 0;
            self.initial_increments = 0;
            self.period_start_time = now;
        }
    }

    /// Calculate the length of the period in d-cycles.
    fn period(&self) -> DCycle {
        DCycle::new(if self.clock_divider == 0 {
            1 << (self.clock_shift + 2)
        } else {
            (self.clock_divider as u64) << (self.clock_shift + 3)
        })
    }

    /// Get the current number of increments at a given time. Assumes time doesn't move backwards
    /// from when the period is updated.
    fn increments(&self, now: DCycle) -> u64 {
        if self.period_start_time <= now {
            let cycles_since_period_start = now - self.period_start_time;
            let increments = cycles_since_period_start.as_u64() / self.period().as_u64();
            self.initial_increments + increments
        } else {
            self.initial_increments - 1
        }
    }

    /// Update the LFSR and get the current sample.
    ///
    /// Mut is needed so the LFSR can be updated.
    fn get_sample(&mut self, now: DCycle) -> f32 {
        // If the DAC is off, we always set active to false, so we don't need to check the DAC here.
        if self.active {
            let increments = self.increments(now);
            let new_increments = increments - self.consumed_increments;
            self.consumed_increments = increments;
            let dac_input =
                self.lfsr.increment_by_and_get_output(new_increments) * self.envelope.level();
            1.0 - (dac_input as f32 / 15.0 * 2.0)
        } else {
            0.0
        }
    }

    /// Apply the APU-DIV ticks.
    fn tick_apu_div(&mut self, apu_div: ApuDiv) {
        if self.length_timer.tick_apu_div(apu_div) {
            self.active = false;
        }
        self.envelope.tick_apu_div(apu_div);
    }
}

impl<LFSR: LinearFeedbackShiftRegister> MemDevice for NoiseChannel<LFSR> {
    const LEN: usize = 4;

    fn read_byte_relative(&self, ctx: &ReadCtx, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(NoiseChannel<LFSR>, addr, |addr| {
            0x00 => 0xff,
            0x01 => self.envelope_control.read_byte_relative(ctx, addr),
            0x02 => self.noise_control(),
            0x03 => self.control(),
        })
    }

    fn write_byte_relative(&mut self, ctx: &WriteCtx, addr: RelativeAddr, val: u8) {
        dispatch_memdev_byte!(NoiseChannel<LFSR>, addr, |addr| {
            0x00 => self.length_timer.set_counter(val),
            0x01 => {
                self.envelope_control.write_byte_relative(ctx, addr, val);
                if !self.envelope_control.dac_enabled() {
                    // Disabling the DAC disables the channel.
                    self.active = false;
                }
            },
            0x02 => self.set_noise_control(ctx.atime().elapsed_fixed_cycles(), val),
            0x03 => self.set_control(ctx.atime().elapsed_fixed_cycles(), val),
        })
    }

    memdev_bytes_from_byte!(NoiseChannel<LFSR>);
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
    pub ch4: NoiseChannel<TabledLinearFeedbackShiftRegister>,
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
    fn tick_apu_div(&mut self, now: DCycle, apu_div: ApuDiv) {
        self.ch1.tick_apu_div(now, apu_div);
        self.ch2.tick_apu_div(now, apu_div);
        self.ch3.tick_apu_div(now, apu_div);
        self.ch4.tick_apu_div(apu_div);
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
    /// gets reset to the SystemClock time.
    sample_rate_changed_at: Duration,
    /// Cached time that the next sample should be taken at.
    next_sample_time: Duration,
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
            sample_rate_changed_at: Duration::ZERO,
            next_sample_time: Duration::ZERO,
            prev_divider_bit: false,
            apu_div: ApuDiv::default(),
            hpf: HighPassFilter::default(),
        }
    }

    /// Set the output sample rate. The current clock snapshot should be provided.
    pub(crate) fn set_output_sample_rate(&mut self, now: DCycle, sample_rate: u32) {
        self.sample_rate = sample_rate as u64;
        self.sample = 0;
        self.sample_rate_changed_at = now.duration();
        self.next_sample_time = self.sample_rate_changed_at;
        self.hpf.set_sample_rate(sample_rate);
    }

    pub(crate) fn consume_output_sample(&mut self) -> Option<Sample> {
        self.output_sample.take()
    }

    /// Increments the number of samples by 1 and updates the time for the next sample.
    fn increment_sample_count(&mut self) {
        self.sample += 1;
        let time_since_changed = cycles_to_duration(self.sample, self.sample_rate);
        self.next_sample_time = self.sample_rate_changed_at + time_since_changed
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

/// to be called on divider bit 4 (5 double speed) falling edge
pub fn tick_apu_div(ctx: &mut impl ApuContext) {
    let now = ctx.clock().elapsed_fixed_cycles();
    let apu_div = ctx.apu_mut().apu_div.tick_apu_div();
    ctx.apu_regs_mut().tick_apu_div(now, apu_div);
}

pub fn tick(ctx: &mut impl ApuContext) {
    let divider_bit_selector = match ctx.clock().speed() {
        ClockSpeed::Normal => 0x2000,
        ClockSpeed::Double => 0x4000,
    };
    let divider_bit = ctx.divider().value(ctx.clock().elapsed_cycles()) & divider_bit_selector != 0;
    let prev_divider_bit = mem::replace(&mut ctx.apu_mut().prev_divider_bit, divider_bit);
    if prev_divider_bit && !divider_bit {
        tick_apu_div(ctx);
    }

    if ctx.apu().sample_rate > 0 {
        let now = ctx.clock().elapsed_time();
        if ctx.apu().next_sample_time < now {
            // Sample output has fallen behind the system clock somehow, we will reset the next
            // sample time to the current time and then emit samples from then.
            //
            // This should only be possible if the sample rate is faster than the d-cycle rate.
            warn!("Sample output behind, resetting to now");
            ctx.apu_mut().sample = 0;
            ctx.apu_mut().sample_rate_changed_at = now;
            ctx.apu_mut().next_sample_time = now;
        }
        for (cycle, time_range) in ctx.clock().current_cycle_fixed_cycles().time_ranges() {
            if time_range.start <= ctx.apu().next_sample_time
                && ctx.apu().next_sample_time < time_range.end
            {
                ctx.apu_mut().increment_sample_count();

                let channel_mix_left = ctx.apu_regs().sound_pan.channel_mix(SoundSide::Left);
                let vol_mul_left = ctx.apu_regs().sound_volume.vol_multiplier(SoundSide::Left);

                let channel_mix_right = ctx.apu_regs().sound_pan.channel_mix(SoundSide::Right);
                let vol_mul_right = ctx.apu_regs().sound_volume.vol_multiplier(SoundSide::Left);

                let ch1 = ctx.apu_regs().ch1.get_sample(cycle);
                let ch2 = ctx.apu_regs().ch2.get_sample(cycle);
                let ch3 = ctx.apu_regs().ch3.get_sample(cycle);
                let ch4 = ctx.apu_regs_mut().ch4.get_sample(cycle);

                fn blend_samples(
                    mix: ChannelMix,
                    vol_mul: f32,
                    ch1: f32,
                    ch2: f32,
                    ch3: f32,
                    ch4: f32,
                ) -> f32 {
                    vol_mul
                        * (mix.get_channel_mix_multiplier(ChannelMix::CH1) * ch1
                            + mix.get_channel_mix_multiplier(ChannelMix::CH2) * ch2
                            + mix.get_channel_mix_multiplier(ChannelMix::CH3) * ch3
                            + mix.get_channel_mix_multiplier(ChannelMix::CH4) * ch4)
                }

                let unfiltered = Sample {
                    left: blend_samples(channel_mix_left, vol_mul_left, ch1, ch2, ch3, ch4),
                    right: blend_samples(channel_mix_right, vol_mul_right, ch1, ch2, ch3, ch4),
                };

                ctx.apu_mut().output_sample = Some(ctx.apu_mut().hpf.filter_sample(unfiltered));
            }
        }
    }
}
