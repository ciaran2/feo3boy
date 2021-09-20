//! Utilities for implementing operations.

use super::Flags;

/// Get the result of applying an 8 bit offset to a 16 bit address.
/// * ZERO is never set.
/// * SUB is never set.
/// * CARRY is set based on the low order byte.
/// * HALFCARRY is set based on the lower order byte.
/// Carries are determined as if adding (using two's compliment).
pub(super) fn offset_addr(addr: u16, offset: i8) -> (u16, Flags) {
    // Perform sign-extension, then treat as u16.
    let offset = offset as i16 as u16;

    // JR ignores flags, SP+i8 is used in two instructions and sets flags for carry and half carry
    // based on the lower byte.
    let mut flags = Flags::empty();
    if (addr & 0xf) + (offset & 0xf) > 0xf {
        flags |= Flags::HALFCARRY;
    }
    // Since we're working on 16 bits, we calculate CARRY for the lower byte the same way we
    // typically do a HALFCARRY.
    if (addr & 0xff) + (offset & 0xff) > 0xff {
        flags |= Flags::CARRY;
    }

    // In two's compliment, adding a negative is the same as adding with wraparound.
    (addr.wrapping_add(offset), flags)
}

/// Perform 8 bit addition, returning an un-masked set of flags:
/// * ZERO is set if the result is zero.
/// * SUB never set.
/// * CARRY is set if the result overflowed.
/// * HALFCARRY is set if there was an overflow from bit 3 -> 4.
pub(super) fn add8_flags(a: u8, b: u8) -> (u8, Flags) {
    let mut flags = Flags::empty();
    if (a & 0xf) + (b & 0xf) > 0xf {
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
    if (a & 0xf) < (b & 0xf) {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = a.overflowing_sub(b);
    flags |= Flags::check_zero(res) | Flags::check_carry(carry);
    (res, flags)
}

/// Helper to perform a 9-bit rotation through the carry flag. Returns flags with:
/// * ZERO is set if the result is zero.
/// * SUB is never set.
/// * CARRY is set to the value of the MSB of a.
/// * HALFCARRY is never set.
pub(super) fn rotate_left9(a: u8, flags: Flags) -> (u8, Flags) {
    let rotated = a.rotate_left(1);
    // Pull the carry flag into the first bit.
    let res = rotated & 0xfe | flags.contains(Flags::CARRY) as u8;
    // Set the carry flag based on the bit that was rotated out of max position.
    let flags = Flags::check_carry(rotated & 1 != 0) | Flags::check_zero(res);
    (res, flags)
}

/// Helper to perform a 9-bit rotation through the carry flag. Returns flags with:
/// * ZERO is set if the result is zero.
/// * SUB is never set.
/// * CARRY is set to the value of the LSB of a.
/// * HALFCARRY is never set.
pub(super) fn rotate_right9(a: u8, flags: Flags) -> (u8, Flags) {
    let rotated = a.rotate_right(1);
    // Pull the carry flag into the highest bit.
    let res = rotated & 0x7f | ((flags.contains(Flags::CARRY) as u8) << 7);
    // Set the carry flag based on the bit that was rotated out of min position.
    let flags = Flags::check_carry(a & 1 != 0) | Flags::check_zero(res);
    (res, flags)
}
