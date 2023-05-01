//! Utilites for working with bits and bitflags.

use bitflags::BitFlags;

/// A group of bits within a byte. Provides utilities for extracting and setting the
/// selected bits.
#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct BitGroup(pub u8);

impl BitGroup {
    /// Masks the given value down to just these bits without applying any shifting.
    #[inline]
    pub const fn filter(self, val: u8) -> u8 {
        val & self.0
    }

    /// Extract these bits from the given u8 value.
    ///
    /// The return value will be the input `val` masked to only the bits in this BitGroup
    /// and shifted over so the right-most bit of the group is in the 0th bit index.
    #[inline]
    pub const fn extract(self, val: u8) -> u8 {
        (val & self.0) >> self.0.trailing_zeros()
    }

    /// Extract these bits from the given u8 value, applying sign-extension to them.
    ///
    /// The return value will be the input `val` masked to only the bits in this BitGroup
    /// and shifted over so the right-most bit of the group is in the 0th bit index.
    #[inline]
    pub const fn extract_signed(self, val: u8) -> i8 {
        (((val & self.0) << self.0.leading_zeros()) as i8)
            >> (self.0.leading_zeros() + self.0.trailing_zeros())
    }

    /// Extract this value as a boolean. Returns true if the value within the portion of
    /// the `val` covered by this [`BitGroup`] is non-zero.
    #[inline]
    pub const fn extract_bool(self, val: u8) -> bool {
        (val & self.0) != 0
    }

    /// Write `val` to the part of `dest` represented by this [`BitGroup`].
    ///
    /// Shift `val` to the left so that it starts at the same point as this `BitGroup` and
    /// then apply it to `dest` with a mask.
    #[inline]
    pub fn apply(self, dest: &mut u8, val: u8) {
        *dest = self.applied(*dest, val);
    }

    /// Write `val` to the part of `dest` represented by this [`BitGroup`], returning the
    /// result.
    ///
    /// Shift `val` to the left so that it starts at the same point as this `BitGroup` and
    /// then apply it to `dest` with a mask.
    #[inline]
    pub const fn applied(self, dest: u8, val: u8) -> u8 {
        (dest & !self.0) | ((val << self.0.trailing_zeros()) & self.0)
    }

    /// Write `val` to this bits-based value.
    #[inline]
    pub fn apply_bits<B>(self, dest: &mut B, val: u8)
    where
        B: BitFlags<Bits = u8>,
    {
        let mut bits = dest.bits();
        self.apply(&mut bits, val);
        *dest = B::from_bits_truncate(bits);
    }
}

/// Extension trait that adds the `with_bits` and `set_bits` methods to `u8`.
pub trait ApplyBitGroup {
    /// Make a new copy of `self` with the bits specified by the given `BitGroup` set to
    /// the `value` specified.
    fn with_bits(self, group: BitGroup, value: u8) -> Self;

    /// Set the bits specified by the given `BitGroup` to the `value` specified, in-place.
    fn set_bits(&mut self, group: BitGroup, value: u8);
}

impl ApplyBitGroup for u8 {
    fn with_bits(self, group: BitGroup, value: u8) -> Self {
        group.applied(self, value)
    }

    fn set_bits(&mut self, group: BitGroup, value: u8) {
        *self = self.with_bits(group, value)
    }
}
