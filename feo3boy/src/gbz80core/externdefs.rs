//! Definitions of opcode externs which are common to multiple executors.

use std::mem;

use crate::gbz80core::ExecutorContext;
use crate::memdev::{ReadCtx, WriteCtx};
use crate::{interrupts::Interrupts, memdev::RootMemDevice};

use feo3boy_opcodes::{
    gbz80types::Flags,
    microcode::args::{Reg16, Reg8},
};

/// Read an 8-bit value from a register.
#[inline]
pub(super) fn read_reg(ctx: &mut impl ExecutorContext, reg: Reg8) -> u8 {
    match reg {
        Reg8::Acc => ctx.cpu().regs.acc,
        Reg8::B => ctx.cpu().regs.b,
        Reg8::C => ctx.cpu().regs.c,
        Reg8::D => ctx.cpu().regs.d,
        Reg8::E => ctx.cpu().regs.e,
        Reg8::H => ctx.cpu().regs.h,
        Reg8::L => ctx.cpu().regs.l,
    }
}

/// Write an 8-bit value to a register.
#[inline]
pub(super) fn write_reg(ctx: &mut impl ExecutorContext, reg: Reg8, val: u8) {
    match reg {
        Reg8::Acc => ctx.cpu_mut().regs.acc = val,
        Reg8::B => ctx.cpu_mut().regs.b = val,
        Reg8::C => ctx.cpu_mut().regs.c = val,
        Reg8::D => ctx.cpu_mut().regs.d = val,
        Reg8::E => ctx.cpu_mut().regs.e = val,
        Reg8::H => ctx.cpu_mut().regs.h = val,
        Reg8::L => ctx.cpu_mut().regs.l = val,
    }
}

/// Read an 8-bit value from a register.
#[inline]
pub(super) fn read_reg16(ctx: &mut impl ExecutorContext, reg: Reg16) -> u16 {
    match reg {
        Reg16::AF => ctx.cpu().regs.af(),
        Reg16::BC => ctx.cpu().regs.bc(),
        Reg16::DE => ctx.cpu().regs.de(),
        Reg16::HL => ctx.cpu().regs.hl(),
        Reg16::Sp => ctx.cpu().regs.sp,
        Reg16::Pc => ctx.cpu().regs.pc,
    }
}

/// Write an 8-bit value to a register.
#[inline]
pub(super) fn write_reg16(ctx: &mut impl ExecutorContext, reg: Reg16, val: u16) {
    match reg {
        Reg16::AF => ctx.cpu_mut().regs.set_af(val),
        Reg16::BC => ctx.cpu_mut().regs.set_bc(val),
        Reg16::DE => ctx.cpu_mut().regs.set_de(val),
        Reg16::HL => ctx.cpu_mut().regs.set_hl(val),
        Reg16::Sp => ctx.cpu_mut().regs.sp = val,
        Reg16::Pc => ctx.cpu_mut().regs.pc = val,
    }
}

/// Pop a 16 bit value from the stack and use it to read an 8 bit value onto the
/// stack.
#[inline]
pub(super) fn read_mem(ctx: &mut impl ExecutorContext, addr: u16) -> u8 {
    let readctx = ReadCtx::new(ctx.clock().snapshot());
    ctx.mem().read(&readctx, addr)
}

/// Pop a 16 bit value from the stack and use it as the address, then pop an 8 bit
/// value from the stack and wite it to that address.
#[inline]
pub(super) fn write_mem(ctx: &mut impl ExecutorContext, addr: u16, val: u8) {
    let writectx = WriteCtx::new(ctx.clock().snapshot());
    ctx.mem_mut().write(&writectx, addr, val)
}

/// Fetches the flags register onto the microcode stack,
#[inline]
pub(super) fn get_flags_masked(ctx: &mut impl ExecutorContext, mask: Flags) -> Flags {
    ctx.cpu().regs.flags & mask
}

/// Pops flags off the microcode stack, masks them, and applies them to the flags
/// register.
#[inline]
pub(super) fn set_flags_masked(ctx: &mut impl ExecutorContext, mask: Flags, flags: Flags) {
    ctx.cpu_mut().regs.flags.merge(flags, mask)
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
pub(super) fn halt(ctx: &mut impl ExecutorContext) {
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

/// Enables interrupts, either immediately or after the next instruction.
#[inline]
pub(super) fn enable_interrupts(ctx: &mut impl ExecutorContext, immediate: bool) {
    if immediate {
        ctx.cpu_mut().interrupt_master_enable.set();
    } else {
        ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
    }
}

/// Disables interrupts immediately.
#[inline]
pub(super) fn disable_interrupts(ctx: &mut impl ExecutorContext) {
    ctx.cpu_mut().interrupt_master_enable.clear();
}

/// Retrieves the value of the halted flag from the CPU.
#[inline]
pub(super) fn check_halt(ctx: &mut impl ExecutorContext) -> bool {
    ctx.cpu().halted
}

/// Sets the CPU to not be halted.
#[inline]
pub(super) fn clear_halt(ctx: &mut impl ExecutorContext) {
    ctx.cpu_mut().halted = false;
}

/// Gets a bool indicating if IME is set.
#[inline]
pub(super) fn check_ime(ctx: &mut impl ExecutorContext) -> bool {
    ctx.cpu().interrupt_master_enable.enabled()
}

/// Gets the set of currently active and enabled interrupts.
#[inline]
pub(super) fn get_active_interrupts(ctx: &mut impl ExecutorContext) -> u8 {
    ctx.interrupts().active().bits()
}

/// Gets the address of the interrupt handler for the next active and enabled
/// interrupt from the interupt vector and clears that interrupt from the interrupt
/// vector. Does not disable interrupts.
#[inline]
pub(super) fn pop_interrupt(ctx: &mut impl ExecutorContext) -> u16 {
    match ctx.interrupts().active().iter().next() {
        Some(interrupt) => {
            ctx.interrupts_mut().clear(interrupt);
            interrupt.handler_addr()
        }
        None => panic!(
            "Must not use the PopInterrupt microcode instruction if there \
                are no active interrupts."
        ),
    }
}

/// Pushes the value of the Halt Bug flag onto the microcode stack, clearing the value
/// to false.
#[inline]
pub(super) fn pop_halt_bug(ctx: &mut impl ExecutorContext) -> bool {
    mem::replace(&mut ctx.cpu_mut().halt_bug, false)
}
