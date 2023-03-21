//! Utilities for implementing operations.

use crate::gbz80core::CpuContext;
use crate::interrupts::Interrupts;

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

/// If `IME` is set, just halts the CPU until there is an interrupt available to be serviced. If
/// there's already an interrupt to be serviced, `HALT` is effectively a `NOP` and excution moved
/// directly to the interrupt handler then to the next instruction.
///
/// If `IME` is not set, there's a possibility of triggering a CPU bug:
/// *   If no interrupt is pending, the CPU still halts and resumes the next time an interrupt
///     becomes pending, but the interrupt just isn't handled because IME is off.
/// *   If there are enabled interrupts pending (`[IE] & [IF] != 0`), the bug is triggered:
///     *   In the normal case, the bug just prevents the program counter from incrementing
///         properly, so the byte after `HALT` will be read twice. (This presumably means that any
///         1-byte instruction will execute twice, and any two-byte instruction will read itself as
///         the following value, but that's somewhat unclear. Its also not clear what happens if an
///         `RST` is executed, since that's a 1 byte jump. Does overwritting the `PC` avert the bug?
///         Note that with any normal `CALL`, `JP`, or `JR`, the repeat byte would be used as the
///         jump target or offset instead.)
///     *   If `EI` executed just before `HALT` such that `IME` would become true after the `HALT`,
///         the bug is even weirder: the interrupt is serviced as normal, but then the interrupt
///         returns to `halt` which is executed again.
///
/// Implementing the behavior of preventing PC increment would require a bunch of complex extra
/// state which would have to be checked in a bunch of places, so for now this just panics if the
/// bug would be encountered.
pub(super) fn halt(ctx: &mut impl CpuContext) {
    if ctx.cpu().interrupt_master_enable.enabled() {
        // No need to special-case interrupts here, since the next `tick` call will un-halt anyway.
        ctx.cpu_mut().halted = true;
    } else {
        let enabled_interrupts = ctx.interrupts().enabled();
        let pending_interrupts = ctx.interrupts().queued();
        if enabled_interrupts.intersects(pending_interrupts) {
            // Halt doesn't actually happen, and instead the program counter will fail to
            // increment in the next step.
            ctx.cpu_mut().halt_bug = true;
        } else {
            // `tick` will un-halt next time ([IE] & [IF] != 0), but will not service the interrupt.
            ctx.cpu_mut().halted = true;
        }
    }
}
