//! Provides the direct-execution implementations of combo-codes.

use std::mem;

use crate::gbz80core::microcode::combocodes::ComboCode;
use crate::gbz80core::microcode::r#impl as baseimpl;
use crate::gbz80core::microcode::MicrocodeFlow;
use crate::gbz80core::{CpuContext, Gbz80State};
use crate::interrupts::Interrupts;

impl ComboCode {
    pub(in crate::gbz80core::microcode) fn eval(self, ctx: &mut impl CpuContext) -> MicrocodeFlow {
        match self {
            Self::HandleHalt => handle_halt(ctx),
            Self::BeginInterruptHandlingOrSkip { steps } => {
                begin_interrupt_handling_or_skip(ctx, steps)
            }
            Self::NextImmediateAddr => next_immediate_addr(ctx),
            Self::ExecuteOpcode => execute_opcode(ctx),
            Self::HlInc => hl_inc(ctx),
            Self::HlDec => hl_dec(ctx),
            Self::SpInc => sp_inc(ctx),
            Self::DecSp => dec_sp(ctx),
            Self::JumpRst(addr) => jump_rst(ctx, addr),
        }
    }
}

/// Implements Halt handling.
fn handle_halt(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    if ctx.cpu().halted {
        if ctx.interrupts().active().is_empty() {
            // Revert to the start of instruction handling.
            baseimpl::fetch_next_instruction(ctx);
            return MicrocodeFlow::Yield1m;
        }
        ctx.cpu_mut().halted = false;
    }
    MicrocodeFlow::Continue
}

/// Begins the ISR routine or skips over ISR handling if ISR isn't enabled.
fn begin_interrupt_handling_or_skip(ctx: &mut impl CpuContext, steps: usize) -> MicrocodeFlow {
    if ctx.cpu().interrupt_master_enable.enabled() {
        if let Some(interrupt) = ctx.interrupts().active().iter().next() {
            ctx.interrupts_mut().clear(interrupt);
            let cpu = ctx.cpu_mut();

            cpu.interrupt_master_enable.clear();
            let ret_loc = if mem::take(&mut cpu.halt_bug) {
                cpu.regs.pc.wrapping_sub(1)
            } else {
                cpu.regs.pc
            };

            cpu.microcode.stack.pushu16(interrupt.handler_addr());
            cpu.microcode.stack.pushu16(ret_loc);
            return MicrocodeFlow::Yield1m;
        }
    }
    // Interrupt handling was not started, so instead skip over the remainder of the ISR
    // (which occupies the next `steps` in the microcode).
    ctx.cpu_mut().microcode.pc += steps;
    debug_assert!(ctx.cpu().microcode.pc <= ctx.cpu().microcode.instruction.len());
    MicrocodeFlow::Continue
}

/// Get the address for the next immediate value and update the PC.
fn next_immediate_addr(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    let addr = cpu.regs.pc;
    cpu.regs.pc = addr.wrapping_add(1);
    cpu.microcode.stack.pushu16(addr);
    MicrocodeFlow::Yield1m
}

/// Begin execution of the next opcode from the microcode stack.
fn execute_opcode(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    baseimpl::tick_ime_on_end(ctx);
    if let cpu @ Gbz80State { halt_bug: true, .. } = ctx.cpu_mut() {
        cpu.halt_bug = false;
        cpu.regs.pc = cpu.regs.pc.wrapping_sub(1);
    }
    baseimpl::parse_opcode(ctx);
    MicrocodeFlow::Continue
}

/// Retrieve HL onto the microcode stack with a post-increment.
fn hl_inc(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    let hl = cpu.regs.hl();
    cpu.regs.set_hl(hl.wrapping_add(1));
    cpu.microcode.stack.pushu16(hl);
    MicrocodeFlow::Continue
}

/// Retrieve HL onto the microcode stack with a post-decrement.
fn hl_dec(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    let hl = cpu.regs.hl();
    cpu.regs.set_hl(hl.wrapping_sub(1));
    cpu.microcode.stack.pushu16(hl);
    MicrocodeFlow::Continue
}

/// Retrieve and increment the stack pointer.
fn sp_inc(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    let sp = cpu.regs.sp;
    cpu.regs.sp = sp.wrapping_add(1);
    cpu.microcode.stack.pushu16(sp);
    MicrocodeFlow::Continue
}

/// Decrement and retrieve the stack pointer.
fn dec_sp(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    cpu.regs.sp = cpu.regs.sp.wrapping_sub(1);
    cpu.microcode.stack.pushu16(cpu.regs.sp);
    MicrocodeFlow::Continue
}

/// Set the PC to the given RST address.
fn jump_rst(ctx: &mut impl CpuContext, addr: u8) -> MicrocodeFlow {
    let cpu = ctx.cpu_mut();
    cpu.regs.pc = addr as u16;
    MicrocodeFlow::Continue
}
