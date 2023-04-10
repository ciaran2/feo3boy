//! Contains the code-generated microcode executor.

use feo3boy_executor_generator::define_direct_executor;

use super::ExecutorContext;

mod tests;

/// Convenience type for an ExecutorContext with a unit state.
trait Ctx: ExecutorContext<State = ()> {}

impl<E: ExecutorContext<State = ()>> Ctx for E {}

#[define_direct_executor(DirectExecutorV2)]
pub mod externs {
    use std::mem;

    use crate::{interrupts::Interrupts, memdev::RootMemDevice};

    use super::Ctx;
    use feo3boy_opcodes::{
        gbz80types::Flags,
        microcode::args::{Reg16, Reg8},
    };

    /// Read an 8-bit value from a register.
    #[microcode_extern(ReadReg)]
    #[inline]
    pub(super) fn read_reg(ctx: &mut impl Ctx, reg: Reg8) -> u8 {
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
    #[microcode_extern(WriteReg)]
    #[inline]
    pub(super) fn write_reg(ctx: &mut impl Ctx, reg: Reg8, val: u8) {
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
    #[microcode_extern(ReadReg16)]
    #[inline]
    pub(super) fn read_reg16(ctx: &mut impl Ctx, reg: Reg16) -> u16 {
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
    #[microcode_extern(WriteReg16)]
    #[inline]
    pub(super) fn write_reg16(ctx: &mut impl Ctx, reg: Reg16, val: u16) {
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
    #[microcode_extern(ReadMem)]
    #[inline]
    pub(super) fn read_mem(ctx: &mut impl Ctx, addr: u16) -> u8 {
        ctx.mem().read(addr)
    }

    /// Pop a 16 bit value from the stack and use it as the address, then pop an 8 bit
    /// value from the stack and wite it to that address.
    #[microcode_extern(WriteMem)]
    #[inline]
    pub(super) fn write_mem(ctx: &mut impl Ctx, addr: u16, val: u8) {
        ctx.mem_mut().write(addr, val)
    }

    /// Fetches the flags register onto the microcode stack,
    #[microcode_extern(GetFlagsMasked)]
    #[inline]
    pub(super) fn get_flags_masked(ctx: &mut impl Ctx, mask: Flags) -> Flags {
        ctx.cpu().regs.flags & mask
    }

    /// Pops flags off the microcode stack, masks them, and applies them to the flags
    /// register.
    #[microcode_extern(SetFlagsMasked)]
    #[inline]
    pub(super) fn set_flags_masked(ctx: &mut impl Ctx, mask: Flags, flags: Flags) {
        ctx.cpu_mut().regs.flags.merge(flags, mask)
    }

    /// Puts the CPU into the halted state.
    #[microcode_extern(Halt)]
    #[inline]
    pub(super) fn halt(ctx: &mut impl Ctx) {
        crate::gbz80core::oputils::halt(ctx)
    }

    /// Enables interrupts, either immediately or after the next instruction.
    #[microcode_extern(EnableInterrupts)]
    #[inline]
    pub(super) fn enable_interrupts(ctx: &mut impl Ctx, immediate: bool) {
        if immediate {
            ctx.cpu_mut().interrupt_master_enable.set();
        } else {
            ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
        }
    }

    /// Disables interrupts immediately.
    #[microcode_extern(DisableInterrupts)]
    #[inline]
    pub(super) fn disable_interrupts(ctx: &mut impl Ctx) {
        ctx.cpu_mut().interrupt_master_enable.clear();
    }

    /// Retrieves the value of the halted flag from the CPU.
    #[microcode_extern(CheckHalt)]
    #[inline]
    pub(super) fn check_halt(ctx: &mut impl Ctx) -> bool {
        ctx.cpu().halted
    }

    /// Sets the CPU to not be halted.
    #[microcode_extern(ClearHalt)]
    #[inline]
    pub(super) fn clear_halt(ctx: &mut impl Ctx) {
        ctx.cpu_mut().halted = false;
    }

    /// Gets a bool indicating if IME is set.
    #[microcode_extern(CheckIme)]
    #[inline]
    pub(super) fn check_ime(ctx: &mut impl Ctx) -> bool {
        ctx.cpu().interrupt_master_enable.enabled()
    }

    /// Gets the set of currently active and enabled interrupts.
    #[microcode_extern(GetActiveInterrupts)]
    #[inline]
    pub(super) fn get_active_interrupts(ctx: &mut impl Ctx) -> u8 {
        ctx.interrupts().active().bits()
    }

    /// Gets the address of the interrupt handler for the next active and enabled
    /// interrupt from the interupt vector and clears that interrupt from the interrupt
    /// vector. Does not disable interrupts.
    #[microcode_extern(PopInterrupt)]
    #[inline]
    pub(super) fn pop_interrupt(ctx: &mut impl Ctx) -> u16 {
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
    #[microcode_extern(PopHaltBug)]
    #[inline]
    pub(super) fn pop_halt_bug(ctx: &mut impl Ctx) -> bool {
        mem::replace(&mut ctx.cpu_mut().halt_bug, false)
    }
}
