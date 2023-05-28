//! This module just encapsulates basic arithmetic ops and other such trait
//! implementations for the [`MCycle`] and [`TCycle`] strong-typing zero-cost wrappers so
//! they don't clutter the main clock module.

use std::ops::{Add, AddAssign, Sub, SubAssign};
use std::time::Duration;

use crate::clock::{cycles_to_duration, ClockSpeed};

/// Typed wrapper for m-cycles.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct MCycle {
    /// Some number of m-cycles.
    mcycle: u64,
}

impl MCycle {
    /// Zero MCycles.
    pub const ZERO: Self = Self::new(0);

    /// Number of MCycles in a second at normal speed. When running at double-speed, this
    /// is doubled.
    const PER_SECOND: Self = Self::new(1_048_576);

    /// Create a new value with the given number of m-cycles.
    #[inline]
    pub const fn new(mcycle: u64) -> Self {
        Self { mcycle }
    }

    /// Get the number of t-cycles corresponding to this number of m-cycles.
    #[inline]
    pub const fn as_tcycle(self) -> TCycle {
        TCycle::new(self.mcycle * 4)
    }

    /// Get the number of MCycles as a u64.
    #[inline]
    pub const fn as_u64(self) -> u64 {
        self.mcycle
    }

    /// Get the number of d-cycles corresponding to this number of m-cycles at the given
    /// clock speed.
    #[inline]
    pub const fn as_dcycle(self, speed: ClockSpeed) -> DCycle {
        DCycle::new(self.mcycle * speed.dcycles_per_mcycle())
    }

    /// Get a duration corresponding to this number of m-cycles, rounded to down to the
    /// next smallest number of nanoseconds.
    pub const fn duration(self, speed: ClockSpeed) -> Duration {
        let per_second = Self::PER_SECOND.mcycle * speed.speed_multiplier();
        cycles_to_duration(self.mcycle, per_second)
    }

    /// Const implementation of adding two mcycles.
    #[inline]
    pub const fn add(self, rhs: Self) -> Self {
        Self::new(self.mcycle + rhs.mcycle)
    }

    /// Const implementation of subtracting two mcycles.
    #[inline]
    pub const fn sub(self, rhs: Self) -> Self {
        Self::new(self.mcycle - rhs.mcycle)
    }
}

impl From<u64> for MCycle {
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}

impl From<MCycle> for u64 {
    fn from(value: MCycle) -> Self {
        value.as_u64()
    }
}

impl Add for MCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        self.add(rhs)
    }
}

impl AddAssign for MCycle {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Add<u64> for MCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: u64) -> Self::Output {
        self + MCycle::new(rhs)
    }
}

impl AddAssign<u64> for MCycle {
    #[inline]
    fn add_assign(&mut self, rhs: u64) {
        *self = *self + rhs;
    }
}

impl Add<TCycle> for MCycle {
    type Output = TCycle;

    #[inline]
    fn add(self, rhs: TCycle) -> Self::Output {
        self.as_tcycle() + rhs
    }
}

impl Sub for MCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        self.sub(rhs)
    }
}

impl SubAssign for MCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Sub<u64> for MCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: u64) -> Self::Output {
        self - MCycle::new(rhs)
    }
}

impl SubAssign<u64> for MCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: u64) {
        *self = *self - rhs;
    }
}

impl Sub<TCycle> for MCycle {
    type Output = TCycle;

    #[inline]
    fn sub(self, rhs: TCycle) -> Self::Output {
        self.as_tcycle() - rhs
    }
}

/// Typed wrapper for t-cycles.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct TCycle {
    /// Some number of t-cycles.
    tcycle: u64,
}

impl TCycle {
    /// Zero TCycles.
    pub const ZERO: Self = Self::new(0);

    /// Number of TCycles in a second at normal speed. When running at double speed, this
    /// is doubled.
    pub const PER_SECOND: Self = MCycle::PER_SECOND.as_tcycle();

    /// Create a new value with the given number of t-cycles.
    #[inline]
    pub const fn new(tcycle: u64) -> Self {
        Self { tcycle }
    }

    /// Get the number of MCycles as a u64.
    #[inline]
    pub const fn as_u64(self) -> u64 {
        self.tcycle
    }

    /// Get a duration corresponding to this number of t-cycles, rounded to down to the
    /// next smallest number of nanoseconds.
    pub const fn duration(self, speed: ClockSpeed) -> Duration {
        let per_second = Self::PER_SECOND.tcycle * speed.speed_multiplier();
        cycles_to_duration(self.tcycle, per_second)
    }

    /// Const implementation of adding two tcycles.
    #[inline]
    pub const fn add(self, rhs: Self) -> Self {
        Self::new(self.tcycle + rhs.tcycle)
    }

    /// Const implementation of subtracting two tcycles.
    #[inline]
    pub const fn sub(self, rhs: Self) -> Self {
        Self::new(self.tcycle - rhs.tcycle)
    }
}

impl From<u64> for TCycle {
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}

impl From<MCycle> for TCycle {
    fn from(value: MCycle) -> Self {
        value.as_tcycle()
    }
}

impl From<TCycle> for u64 {
    fn from(value: TCycle) -> Self {
        value.as_u64()
    }
}

impl Add for TCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        self.add(rhs)
    }
}

impl AddAssign for TCycle {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Add<u64> for TCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: u64) -> Self::Output {
        self + TCycle::new(rhs)
    }
}

impl AddAssign<u64> for TCycle {
    #[inline]
    fn add_assign(&mut self, rhs: u64) {
        *self = *self + rhs;
    }
}

impl Add<MCycle> for TCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: MCycle) -> Self::Output {
        self + rhs.as_tcycle()
    }
}

impl AddAssign<MCycle> for TCycle {
    #[inline]
    fn add_assign(&mut self, rhs: MCycle) {
        *self = *self + rhs;
    }
}

impl Sub for TCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        self.sub(rhs)
    }
}

impl SubAssign for TCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Sub<u64> for TCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: u64) -> Self::Output {
        self - TCycle::new(rhs)
    }
}

impl SubAssign<u64> for TCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: u64) {
        *self = *self - rhs;
    }
}

impl Sub<MCycle> for TCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: MCycle) -> Self::Output {
        self - rhs.as_tcycle()
    }
}

impl SubAssign<MCycle> for TCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: MCycle) {
        *self = *self - rhs;
    }
}

/// D-cycle is a double-speed M-cycle. Unlike [`MCycle`] and [`TCycle`], the duration of a
/// D-cycle does not vary with the clock speed, which makes it more useful for measuring
/// time for parts of the system which aren't affected by clock speed changes. Because a
/// D-cycle always has a fixed duration, these are also referred to as 'fixed cycles'.
///
/// When the CPU is running at normal speed, DCycle is half of an MCycle (2 TCycles). When
/// running at double-speed, DCycle is the same as MCycle (4 TCycles).
///
/// Because of this, conversions are also different. While `MCycle` and `TCycle` can be
/// converted between each other and require a [`ClockSpeed`] to convert to [`Duration`],
/// `DCycle` can be converted to `Duration` independent of the clock speed, but needs a
/// clock speed to be converted to [`MCycle`] or [`TCycle`].
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(transparent)]
pub struct DCycle {
    /// Some number of d-cycles.
    dcycle: u64,
}

impl DCycle {
    /// Zero DCycles.
    pub const ZERO: Self = Self::new(0);

    /// Number of DCycles in a second. This is independent of the [`ClockSpeed`].
    const PER_SECOND: Self = Self::new(2_097_152);

    /// Create a new value with the given number of m-cycles.
    #[inline]
    pub const fn new(dcycle: u64) -> Self {
        Self { dcycle }
    }

    /// Get the number of t-cycles corresponding to this number of d-cycles.
    #[inline]
    pub const fn as_tcycle(self, speed: ClockSpeed) -> TCycle {
        TCycle::new(self.dcycle * 2 * speed.speed_multiplier())
    }

    /// Get the number of MCycles as a u64.
    #[inline]
    pub const fn as_u64(self) -> u64 {
        self.dcycle
    }

    /// Get a duration corresponding to this number of m-cycles, rounded to down to the
    /// next smallest number of nanoseconds.
    pub const fn duration(self) -> Duration {
        cycles_to_duration(self.dcycle, Self::PER_SECOND.dcycle)
    }

    /// Const implementation of adding two mcycles.
    #[inline]
    pub const fn add(self, rhs: Self) -> Self {
        Self::new(self.dcycle + rhs.dcycle)
    }

    /// Const implementation of subtracting two mcycles.
    #[inline]
    pub const fn sub(self, rhs: Self) -> Self {
        Self::new(self.dcycle - rhs.dcycle)
    }
}

impl From<u64> for DCycle {
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}

impl From<DCycle> for u64 {
    fn from(value: DCycle) -> Self {
        value.as_u64()
    }
}

impl Add for DCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        self.add(rhs)
    }
}

impl AddAssign for DCycle {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl Add<u64> for DCycle {
    type Output = Self;

    #[inline]
    fn add(self, rhs: u64) -> Self::Output {
        self + DCycle::new(rhs)
    }
}

impl AddAssign<u64> for DCycle {
    #[inline]
    fn add_assign(&mut self, rhs: u64) {
        *self = *self + rhs;
    }
}

impl Sub for DCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self::Output {
        self.sub(rhs)
    }
}

impl SubAssign for DCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

impl Sub<u64> for DCycle {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: u64) -> Self::Output {
        self - DCycle::new(rhs)
    }
}

impl SubAssign<u64> for DCycle {
    #[inline]
    fn sub_assign(&mut self, rhs: u64) {
        *self = *self - rhs;
    }
}
