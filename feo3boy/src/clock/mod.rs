//! Implements a type for tracking the internal GB clock in both t-cycles, m-cycles and
//! Duration units relative to the GameBoy's system startup.

use std::iter::FusedIterator;
use std::mem;
use std::ops::{Deref, Range};
use std::time::Duration;

mod typed_cycles;

pub use typed_cycles::{DCycle, MCycle, TCycle};

/// Enum of possible system clock speeds.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub enum ClockSpeed {
    /// Clock speed is normal (4 MiHz).
    #[default]
    Normal = 1,
    /// Clock is at double speed (8 MiHz).
    Double = 2,
}

impl ClockSpeed {
    /// Amount that systemc clock speed is multiplied by.
    #[inline]
    pub const fn speed_multiplier(self) -> u64 {
        self as u64
    }

    /// Get the number of dcycles per mcycle at this clock speed.
    #[inline]
    pub const fn dcycles_per_mcycle(self) -> u64 {
        match self {
            ClockSpeed::Normal => 2,
            ClockSpeed::Double => 1,
        }
    }
}

/// Provides context access to the system clock.
pub trait SystemClockContext {
    /// Access the system clock.
    fn clock(&self) -> &SystemClock;
}

/// Represents the system's actual clock.
#[derive(Debug, Default, Clone)]
pub struct SystemClock {
    /// Number of m-cycles elapsed since startup.
    ///
    /// This overflows ever ~557 thousand years of game time.
    mcycle: MCycle,
    /// Number of d-cycles elapsed since startup.
    dcycle: DCycle,
    /// Speed that the clock is currently running at.
    speed: ClockSpeed,
}

impl SystemClock {
    /// Get a new, zeroed system clock.
    pub const fn new() -> Self {
        Self {
            mcycle: MCycle::ZERO,
            dcycle: DCycle::ZERO,
            speed: ClockSpeed::Normal,
        }
    }

    /// Get a snapshot of the current time in terms of variable m-cycles and fixed
    /// d-cycles.
    pub const fn snapshot(&self) -> ClockSnapshot {
        ClockSnapshot {
            mcycle: self.mcycle,
            dcycle: self.dcycle,
        }
    }

    /// Gets the number of m-cycles elapsed since system startup.
    ///
    /// This will overflow after about 557 thousand years of playtime (or about half that
    /// in doublespeed mode).
    #[inline]
    pub const fn elapsed_cycles(&self) -> MCycle {
        self.mcycle
    }

    /// Get the total duration that has elapsed since system startup.
    #[inline]
    pub fn elapsed_time(&self) -> Duration {
        self.dcycle.duration()
    }

    /// Get an interator over the d-cycles in the current m-cycle. This is either a single
    /// item (if running in double-speed mode) or a a pair of d-cycles (if running at
    /// normal speed).
    pub fn current_cycle_fixed_cycles(&self) -> DCycleIter {
        DCycleIter {
            cycles: self.dcycle.as_u64()..self.dcycle.as_u64() + self.speed.dcycles_per_mcycle(),
        }
    }

    /// Get the number of cycles elapsed at *fixed* speed to the start of the current
    /// MCycle.
    pub const fn elapsed_fixed_cycles(&self) -> DCycle {
        self.dcycle
    }

    /// Returns the current clock speed.
    #[inline]
    pub const fn speed(&self) -> ClockSpeed {
        self.speed
    }

    /// Advance the system clock by 1 m-cycle.
    pub fn advance1m(&mut self) {
        // This will never overflow in practical use
        self.mcycle += 1;
        self.dcycle += self.speed.dcycles_per_mcycle();
    }

    /// Sets the new clock speed.
    pub fn set_speed(&mut self, speed: ClockSpeed) {
        self.speed = speed;
    }
}

/// An iterator over a range of D-Cycles.
pub struct DCycleIter {
    /// The d-cycle values to iterator over.
    /// Invariant: ensure that end is always greater than or equal to start.
    cycles: Range<u64>,
}

impl DCycleIter {
    /// Get an interator over the time ranges covered by the d-cycles in this iterator,
    /// assuming the cycles are a count from the start of emulator time.
    pub fn time_ranges(self) -> DCycleTimeRanges {
        let start = DCycle::new(self.cycles.start).duration();
        DCycleTimeRanges {
            cycles: self,
            next_start: start,
        }
    }
}

impl Iterator for DCycleIter {
    type Item = DCycle;

    fn next(&mut self) -> Option<Self::Item> {
        self.cycles.next().map(DCycle::new)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.cycles.size_hint()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.cycles.nth(n).map(DCycle::new)
    }

    fn last(self) -> Option<Self::Item> {
        self.cycles.last().map(DCycle::new)
    }

    fn max(self) -> Option<Self::Item> {
        self.cycles.max().map(DCycle::new)
    }

    fn min(self) -> Option<Self::Item> {
        self.cycles.min().map(DCycle::new)
    }
}

impl DoubleEndedIterator for DCycleIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.cycles.next_back().map(DCycle::new)
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.cycles.nth_back(n).map(DCycle::new)
    }
}

impl FusedIterator for DCycleIter {}

/// Iterator over DCycles and their corresponding time ranges.
pub struct DCycleTimeRanges {
    /// Iterator over the d-cycles.
    cycles: DCycleIter,
    /// Start time of the next cycle that will be returned from DCycleIter. Used to avoid
    /// recomputing the time value unnecessarily.
    next_start: Duration,
}

impl Iterator for DCycleTimeRanges {
    type Item = (DCycle, Range<Duration>);

    fn next(&mut self) -> Option<Self::Item> {
        self.cycles.next().map(|d| {
            let end = (d + 1).duration();
            let start = mem::replace(&mut self.next_start, end);
            (d, start..end)
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.cycles.size_hint()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        if n == 0 {
            self.next()
        } else {
            self.cycles.nth(n).map(|d| {
                let start = d.duration();
                let end = (d + 1).duration();
                self.next_start = end;
                (d, start..end)
            })
        }
    }

    fn last(mut self) -> Option<Self::Item> {
        self.cycles.last().map(|d| {
            let start = d.duration();
            let end = (d + 1).duration();
            // This is unnecessary since self is consumed but is done to maintain
            // invariants.
            self.next_start = end;
            (d, start..end)
        })
    }
}

impl FusedIterator for DCycleTimeRanges {}

/// Type for tracking what clock cycle a value is changed at.
///
/// Values with change tracking are considered equal if have the same value and were updated at the
/// same cycle.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TimedChangeTracker<T> {
    /// Snapshot of the clock when the tracked value was last updated.
    changed_at: ClockSnapshot,
    /// The value stored.
    value: T,
}

impl<T> TimedChangeTracker<T> {
    /// Create a new TimedChangeTracker with the given initial value, set to last changed
    /// at time 0.
    pub const fn new(initial: T) -> Self {
        Self {
            changed_at: ClockSnapshot::new(),
            value: initial,
        }
    }

    /// Get the currently stored value. This is always the most up-to-date value,
    /// regardless of whether the time is set correctly.
    #[inline]
    pub const fn get(&self) -> &T {
        &self.value
    }

    /// Get the clock snapshot from when the value was last changed.
    pub const fn changed_snapshot(&self) -> &ClockSnapshot {
        &self.changed_at
    }

    /// Get the m-cycle where the value was last changed. This may not be up to date. In
    /// debug-mode, this panics if the cycle when the value was changed has not be updated
    /// since the value last changed.
    #[inline]
    pub const fn changed_cycle(&self) -> MCycle {
        self.changed_snapshot().elapsed_cycles()
    }

    /// Get the d-cycle where the value was last changed. This may not be up to date. In
    /// debug-mode, this panics if the cycle when the value was changed has not be updated
    /// since the value last changed.
    #[inline]
    pub const fn changed_fixed_cycle(&self) -> DCycle {
        self.changed_snapshot().elapsed_fixed_cycles()
    }

    /// Set the value, with the given modification time.
    pub fn set(&mut self, now: ClockSnapshot, value: T) -> T {
        self.changed_at = now;
        mem::replace(&mut self.value, value)
    }

    /// Set the value without updating the change tracker. This allows a new value to be
    /// set without affecting the time tracking, for example if there are two possible
    /// sources of changes and you only want to record times for one of them.
    pub fn set_untracked(&mut self, value: T) -> T {
        mem::replace(&mut self.value, value)
    }
}

// The derived default would result in the correct MCycle::ZERO, but doing it this way is more
// explicit.
impl<T: Default> Default for TimedChangeTracker<T> {
    #[inline]
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> Deref for TimedChangeTracker<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

/// Number of nanoseconds in a second, used when computing Durations from discrete units
/// with a given frequency.
const NANOS_PER_SEC: u64 = 1_000_000_000;

/// Converts a count of a number of cycles of some type and the cycle rate in
/// cycles-per-second to the duration it would take for that number of cycles to elapse.
#[inline]
pub const fn cycles_to_duration(num_cycles: u64, cycles_per_second: u64) -> Duration {
    let secs = num_cycles / cycles_per_second;
    let rem = num_cycles % cycles_per_second;
    let nanos = rem * NANOS_PER_SEC / cycles_per_second;
    Duration::new(secs, nanos as u32)
}

/// Snapshot of the SystemClock at a particular cycle. This records both the [`MCycle`]
/// and [`DCycle`], which makes it possible to tell both the number of CPU cycles elapsed
/// and the realtime duration elapsed.
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ClockSnapshot {
    /// Number of MCycles elapsed up to this snapshot.
    mcycle: MCycle,
    /// Number of DCycles elapsed up to this snapshot.
    dcycle: DCycle,
}

impl ClockSnapshot {
    /// Create a new ClockSnapshot set to zero time.
    pub const fn new() -> Self {
        Self {
            mcycle: MCycle::ZERO,
            dcycle: DCycle::ZERO,
        }
    }

    /// Gets the number of m-cycles elapsed since system startup.
    ///
    /// This will overflow after about 557 thousand years of playtime (or about half that
    /// in doublespeed mode).
    #[inline]
    pub const fn elapsed_cycles(&self) -> MCycle {
        self.mcycle
    }

    /// Get the total duration that has elapsed since system startup.
    #[inline]
    pub fn elapsed_time(&self) -> Duration {
        self.dcycle.duration()
    }

    /// Get the number of cycles elapsed at *fixed* speed to the start of the current
    /// MCycle.
    pub const fn elapsed_fixed_cycles(&self) -> DCycle {
        self.dcycle
    }
}
