//! Utilities for implementing operations.

use super::Flags;

/// Get the result of applying an 8 bit offset to a 16 bit address.
pub(super) fn offset_addr(addr: u16, offset: i8) -> u16 {
    // Perform sign-extension, then treat as u16.
    let offset = offset as i16 as u16;
    // In two's compliment, adding a negative is the same as adding with wraparound.
    addr.wrapping_add(offset)
}

/// Perform 8 bit addition, returning an un-masked set of flags:
/// * ZERO is set if the result is zero.
/// * SUB never set.
/// * CARRY is set if the result overflowed.
/// * HALFCARRY is set if there was an overflow from bit 3 -> 4.
pub(super) fn add8_flags(a: u8, b: u8) -> (u8, Flags) {
    let mut flags = Flags::empty();
    if (a & 0x7) + (b & 0x7) > 0x7 {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = a.overflowing_add(b);
    flags |= Flags::check_zero(res) | Flags::check_carry(carry);
    (res, flags)
}

/// Perform 8 bit subtraction, returning an un-masked set of flags:
/// * ZERO is set if the result is zero.
/// * SUB is always set.
/// * CARRY is set if the result underflowed (b > a).
/// * HALFCARRY is set if there was a borrow from bit 4 -> 3.
pub(super) fn sub8_flags(a: u8, b: u8) -> (u8, Flags) {
    let mut flags = Flags::SUB;
    if (a & 0x7) < (b & 0x7) {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = a.overflowing_sub(b);
    flags |= Flags::check_zero(res) | Flags::check_carry(carry);
    (res, flags)
}
