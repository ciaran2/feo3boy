//! Provides a handwritten opcode executor that directly evaluates opcodes.

use std::fmt;
use std::ops::{Deref, DerefMut};

use feo3boy_opcodes::gbz80types::Flags;
use feo3boy_opcodes::microcode;
use feo3boy_opcodes::opcode::args::{AluOp, ConditionCode, Operand16, Operand8};
use feo3boy_opcodes::opcode::{CBOpcode, CBOperation, InternalFetch, Opcode};
use log::{debug, trace, warn};

use crate::gbz80core::executor::Executor;
use crate::gbz80core::{externdefs, ExecutorContext};
use crate::interrupts::Interrupts;

mod args;

/// Executor which evaluates GB Opcodes by decoding them, then matching on them and
/// calling a function which implements them. This executor is not capable of pausing
/// during instruction execution.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct DirectExecutor;

impl Executor for DirectExecutor {
    type State = ();

    fn run_single_instruction(ctx: &mut impl ExecutorContext<State = Self::State>) {
        InternalFetch.runner().run(ctx)
    }
}

/// Convenience type for an ExecutorContext with a unit state.
trait Ctx: ExecutorContext<State = ()> {}

impl<E: ExecutorContext<State = ()>> Ctx for E {}

/// Trait for wrapping a value in its runner.
trait Runner {
    /// Wrap `self` in [`Eval`]`(self)`.
    #[inline]
    fn runner(self) -> Run<Self>
    where
        Self: Sized,
    {
        Run(self)
    }
}

impl Runner for InternalFetch {}
impl Runner for Opcode {}
impl Runner for CBOpcode {}

/// Helper to provide convenient eval/read/write functions for types deinfed in the
/// feo3boy-opcodes crate.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Run<T>(T);

impl<T> Deref for Run<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Run<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: fmt::Display> fmt::Display for Run<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl Run<InternalFetch> {
    fn run(self, ctx: &mut impl Ctx) {
        if ctx.cpu().halted {
            if ctx.interrupts().active().is_empty() {
                ctx.yield1m();
                return;
            }
            ctx.cpu_mut().halted = false;
        }

        if self.service_interrupt(ctx) {
            return;
        }

        let previous_ime = ctx.cpu().interrupt_master_enable;
        let pc = ctx.cpu().regs.pc;
        trace!("Loading opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.runner().read(ctx);
        if ctx.cpu().halt_bug {
            let state = ctx.cpu_mut();
            state.regs.pc = state.regs.pc.wrapping_sub(1);
            state.halt_bug = false;
        }
        let opcode = Opcode::decode(opcode);
        debug!("Executing @ {:#6X}: {}", pc, opcode);
        opcode.runner().run(ctx);
        ctx.cpu_mut().interrupt_master_enable.tick(previous_ime);
    }

    fn service_interrupt(self, ctx: &mut impl Ctx) -> bool {
        if ctx.cpu().interrupt_master_enable.enabled() {
            if let Some(interrupt) = ctx.interrupts().active().iter().next() {
                ctx.yield1m();
                ctx.yield1m();
                ctx.interrupts_mut().clear(interrupt);
                ctx.cpu_mut().interrupt_master_enable.clear();
                let ret_loc = if ctx.cpu().halt_bug {
                    let state = ctx.cpu_mut();
                    state.halt_bug = false;
                    state.regs.pc.wrapping_sub(1)
                } else {
                    ctx.cpu().regs.pc
                };
                push_helper(ctx, ret_loc);
                ctx.yield1m();
                ctx.cpu_mut().regs.pc = interrupt.handler_addr();
                return true;
            }
        }
        false
    }
}

impl Run<Opcode> {
    fn run(self, ctx: &mut impl Ctx) {
        match *self {
            Opcode::Nop => {}
            Opcode::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Opcode::JumpRelative(cond) => jump_relative(ctx, cond),
            Opcode::Inc8(operand) => inc8(ctx, operand),
            Opcode::Dec8(operand) => dec8(ctx, operand),
            Opcode::Load8 { dest, source } => load8(ctx, dest, source),
            Opcode::Inc16(operand) => inc16(ctx, operand),
            Opcode::Dec16(operand) => dec16(ctx, operand),
            Opcode::Load16 { dest, source } => load16(ctx, dest, source),
            Opcode::Add16(operand) => add16(ctx, operand),
            Opcode::Halt => externdefs::halt(ctx),
            Opcode::AluOp { operand, op } => alu_op(ctx, operand, op),
            Opcode::AluUnary(op) => op.runner().run(ctx),
            Opcode::Call(cond) => call(ctx, cond),
            Opcode::Jump(cond) => jump(ctx, cond),
            Opcode::Ret(cond) => ret(ctx, cond),
            Opcode::Push(operand) => push(ctx, operand),
            Opcode::Pop(operand) => pop(ctx, operand),
            Opcode::PrefixCB => <Run<CBOpcode>>::load_and_run(ctx),
            Opcode::DisableInterrupts => externdefs::disable_interrupts(ctx),
            Opcode::EnableInterrupts => externdefs::enable_interrupts(ctx, false),
            Opcode::RetInterrupt => interrupt_return(ctx),
            Opcode::OffsetSp => offset_sp(ctx),
            Opcode::AddressOfOffsetSp => address_of_offset_sp(ctx),
            Opcode::JumpHL => jump_hl(ctx),
            Opcode::Reset(dest) => reset(ctx, dest),
            // A brief note on this doc page:
            // https://gbdev.io/pandocs/CPU_Comparison_with_Z80.html
            // says that the unused opcodes will lock up the CPU, rather than behave as a
            // no-op.
            Opcode::MissingInstruction(opcode) => {
                warn!(
                    "Missing instruction {:#04X} encountered. Treating as NOP instead of locking.",
                    opcode
                );
            }
        }
    }
}

/// Implements the relative jump instruction.
fn jump_relative(ctx: &mut impl Ctx, cond: ConditionCode) {
    // Loading the offset also moves the program counter over the next instruction, which is
    // good because the jump is relative to the following instruction.
    let offset = Operand8::Immediate.runner().read(ctx);
    let base = ctx.cpu().regs.pc;
    // JR doesn't set any flags.
    let (dest, _) = microcode::defs::offset_addr(base, offset);
    if cond.runner().check(ctx) {
        trace!("Relative jump by {} from {} to {}", offset, base, dest);
        // Modifying the PC takes an extra tick, which isn't used if the condition fails.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    } else {
        trace!("Skipping jump by {} from {} to {}", offset, base, dest);
    }
}

/// Implements 8 bit increment instruction.
fn inc8(ctx: &mut impl Ctx, operand: Operand8) {
    // Inc doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    let val = operand.runner().read(ctx);
    let (res, flags) = microcode::defs::add(val, 1);
    trace!("Evaluating INC {} ({} => {})", operand, val, res);
    ctx.cpu_mut().regs.flags.merge(flags, MASK);
    operand.runner().write(ctx, res);
}

/// Implements 8 bit decrement instruction.
fn dec8(ctx: &mut impl Ctx, operand: Operand8) {
    // Dec doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    let val = operand.runner().read(ctx);
    let (res, flags) = microcode::defs::sub(val, 1);
    trace!("Evaluating DEC {} ({} => {})", operand, val, res);
    ctx.cpu_mut().regs.flags.merge(flags, MASK);
    operand.runner().write(ctx, res);
}

/// Implements 8 bit load operations.
fn load8(ctx: &mut impl Ctx, dest: Operand8, source: Operand8) {
    let val = source.runner().read(ctx);
    trace!("Evaluating LD {},{} (<- {})", dest, source, val);
    dest.runner().write(ctx, val);
}

/// Implements 16 bit increment instruction.
fn inc16(ctx: &mut impl Ctx, operand: Operand16) {
    // 16 bit inc doesn't set any flags, and all actual operands are always registers, but it does
    // delay by 1 additional M cycle, probably because it has to operate on two bytes.
    let val = operand.runner().read(ctx);
    let res = val.wrapping_add(1);
    trace!("Evaluating INC {} ({} => {})", operand, val, res);
    ctx.yield1m();
    operand.runner().write(ctx, res);
}

/// Implements 16 bit decrement instruction.
fn dec16(ctx: &mut impl Ctx, operand: Operand16) {
    // 16 bit dec doesn't set any flags, and all actual operands are always registers, but it does
    // delay by 1 additional M cycle, probably because it has to operate on two bytes.
    let val = operand.runner().read(ctx);
    let res = val.wrapping_sub(1);
    trace!("Evaluating DEC {} ({} => {})", operand, val, res);
    ctx.yield1m();
    operand.runner().write(ctx, res);
}

/// Implements 16 bit load operations.
fn load16(ctx: &mut impl Ctx, dest: Operand16, source: Operand16) {
    let val = source.runner().read(ctx);
    if (dest, source) == (Operand16::Sp, Operand16::HL) {
        // Most of the 16 bit loads are <Pair>,<Immediate> and take time based on number of memory
        // accesses. There are two exceptions. LD (u16),SP, which is also just timed based on the
        // number of memory accesses, and LD SP,HL, which is all registers but still takes an extra
        // 1m cycle, which isn't automatically provided by Operand16 register interactions, so we
        // insert it here.
        ctx.yield1m();
    }
    trace!("Evaluating LD {},{} (<- {})", dest, source, val);
    dest.runner().write(ctx, val);
}

/// Implements 16 bit register add into HL. Never sets the zero flag and clears the subtract flag,
/// but does set carry and half-carry based on the upper byte of the operation (as if it was
/// performed by running the pseudo-instructions `add l,<arg-low>; adc h,<arg-high>`.
fn add16(ctx: &mut impl Ctx, arg: Operand16) {
    // 16 bit add never modifies the zero flag.
    const MASK: Flags = Flags::all().difference(Flags::ZERO);

    let lhs = ctx.cpu().regs.hl();
    let rhs = arg.runner().read(ctx); // This will always be a register in practice.

    let mut flags = Flags::empty();
    if (lhs & 0x7ff) + (rhs & 0x7ff) > 0x7ff {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = lhs.overflowing_add(rhs);
    flags |= Flags::check_carry(carry);

    // 16 bit adds have a time of 8t/2m, so 1 more cycle is needed in addition to their
    // instruction load time.
    ctx.yield1m();

    ctx.cpu_mut().regs.set_hl(res);
    ctx.cpu_mut().regs.flags.merge(flags, MASK);
}

/// Runs an ALU operation.
fn alu_op(ctx: &mut impl Ctx, operand: Operand8, op: AluOp) {
    let arg = operand.runner().read(ctx);
    op.runner().run(ctx, arg);
}

/// Performs a conditional call.
fn call(ctx: &mut impl Ctx, cond: ConditionCode) {
    // Conveniently, unconditional call behaves exactly the same as a conditional call with a true
    // value, down to the timing.
    let dest = Operand16::Immediate.runner().read(ctx);
    if cond.runner().check(ctx) {
        // Conditional jump has an extra internal delay if the condition is true.
        ctx.yield1m();
        push_helper(ctx, ctx.cpu().regs.pc);
        ctx.cpu_mut().regs.pc = dest;
    }
}

/// Performs a conditional absolute jump.
fn jump(ctx: &mut impl Ctx, cond: ConditionCode) {
    // Conveniently, unconditional call behaves exactly the same as a conditional call with a true
    // value, down to the timing.
    let dest = Operand16::Immediate.runner().read(ctx);
    if cond.runner().check(ctx) {
        // Branching adds an extra cycle despite not accessing memory.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    }
}

/// Performs a conditional return.
fn ret(ctx: &mut impl Ctx, cond: ConditionCode) {
    // Unlike Jump and Call, Ret is different depending on whether it is conditional or unconditional.
    if cond == ConditionCode::Unconditional {
        let dest = pop_helper(ctx);
        // There's an extra 1m delay after loading SP.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    } else {
        // Conditional branch always has this extra delay before evaluating.
        ctx.yield1m();
        if cond.runner().check(ctx) {
            let dest = pop_helper(ctx);
            // But there's also an extra delay after reading.
            ctx.yield1m();
            ctx.cpu_mut().regs.pc = dest;
        }
    }
}

/// Implements push instruction.
fn push(ctx: &mut impl Ctx, operand: Operand16) {
    // In practice, operand is always a register.
    let val = operand.runner().read(ctx);
    // Push has an extra delay before writing.
    ctx.yield1m();
    push_helper(ctx, val);
}

/// Implements pop instruction.
fn pop(ctx: &mut impl Ctx, operand: Operand16) {
    let val = pop_helper(ctx);
    // In practice, operand is always a register.
    operand.runner().write(ctx, val)
}

/// Push helper, shared between push and call. Pushes a caller-supplied 16 bit value onto the stack,
/// waiting 1m between each byte and decrementing the stack pointer by 2.
fn push_helper(ctx: &mut impl Ctx, val: u16) {
    let [low, high] = val.to_le_bytes();
    ctx.yield1m();
    let addr = ctx.cpu().regs.sp.wrapping_sub(1);
    ctx.cpu_mut().regs.sp = addr;
    externdefs::write_mem(ctx, addr, high);

    ctx.yield1m();
    let addr = ctx.cpu().regs.sp.wrapping_sub(1);
    ctx.cpu_mut().regs.sp = addr;
    externdefs::write_mem(ctx, addr, low);
}

/// Pop helper, shared between pop and ret. Pops value from the stack, waiting 1m between each byte
/// and incrementing the stack pointer by 2.
fn pop_helper(ctx: &mut impl Ctx) -> u16 {
    ctx.yield1m();
    let addr = ctx.cpu().regs.sp;
    ctx.cpu_mut().regs.sp = addr.wrapping_add(1);
    let low = externdefs::read_mem(ctx, addr);

    ctx.yield1m();
    let addr = ctx.cpu().regs.sp;
    ctx.cpu_mut().regs.sp = addr.wrapping_add(1);
    let high = externdefs::read_mem(ctx, addr);

    u16::from_le_bytes([low, high])
}

/// Enabled interrupts and returns.
fn interrupt_return(ctx: &mut impl Ctx) {
    let dest = pop_helper(ctx);
    // Theres an extra 1m of delay in here.
    ctx.yield1m();
    ctx.cpu_mut().regs.pc = dest;
    ctx.cpu_mut().interrupt_master_enable.set();
}

/// Offsets the stack pointer by an immediate value.
fn offset_sp(ctx: &mut impl Ctx) {
    let offset = Operand8::Immediate.runner().read(ctx);
    let (res, flags) = microcode::defs::offset_addr(ctx.cpu().regs.sp, offset);
    // This instruction takes two more cycles after loading the offset.
    ctx.yield1m();
    ctx.yield1m();
    ctx.cpu_mut().regs.sp = res;
    ctx.cpu_mut().regs.flags = flags;
}

/// Loads the result of offsetting the stack pointer by an immediate value into HL.
fn address_of_offset_sp(ctx: &mut impl Ctx) {
    let offset = Operand8::Immediate.runner().read(ctx);
    let (res, flags) = microcode::defs::offset_addr(ctx.cpu().regs.sp, offset);
    // Interestingly, this instruction is actually faster than `ADD SP,i8`.
    ctx.yield1m();
    ctx.cpu_mut().regs.set_hl(res);
    ctx.cpu_mut().regs.flags = flags;
}

// Similar to unconditional jump, but using HL as the target address.
fn jump_hl(ctx: &mut impl Ctx) {
    let regs = &mut ctx.cpu_mut().regs;
    regs.pc = regs.hl();
}

/// Executes the reset instruction. Similar to call with a fixed destination.
fn reset(ctx: &mut impl Ctx, dest: u8) {
    // There's an extra delay at the start of an RST instruction.
    ctx.yield1m();
    push_helper(ctx, ctx.cpu().regs.pc);
    ctx.cpu_mut().regs.pc = dest as u16;
}

impl Run<CBOpcode> {
    /// Load and execute a single CB-prefixed opcode from the given context.
    fn load_and_run(ctx: &mut impl Ctx) {
        let pc = ctx.cpu().regs.pc;
        trace!("Loading CB-opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.runner().read(ctx);
        let opcode = CBOpcode::decode(opcode);
        debug!("Executing CB @ {:#6X}: {}", pc, opcode);
        opcode.runner().run(ctx);
    }

    /// Execute this opcode on the given context.
    fn run(self, ctx: &mut impl Ctx) {
        let arg = self.operand.runner().read(ctx);
        match self.op {
            CBOperation::RotateLeft8 => {
                let (res, flags) = microcode::defs::rotate_left8(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::RotateLeft9 => {
                let (res, flags) = microcode::defs::rotate_left9(ctx.cpu().regs.flags, arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::RotateRight8 => {
                let (res, flags) = microcode::defs::rotate_right8(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::RotateRight9 => {
                let (res, flags) = microcode::defs::rotate_right9(ctx.cpu().regs.flags, arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::ShiftLeft => {
                let (res, flags) = microcode::defs::shift_left(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::ShiftRightSignExt => {
                let (res, flags) = microcode::defs::shift_right_sign_ext(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::ShiftRight => {
                let (res, flags) = microcode::defs::shift_right(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::Swap => {
                let (res, flags) = microcode::defs::swap(arg);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.runner().write(ctx, res);
            }
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                let flags = microcode::defs::test_bit(bit, arg);
                ctx.cpu_mut().regs.flags.merge(flags, MASK);
            }
            CBOperation::ResetBit(bit) => {
                let res = microcode::defs::reset_bit(bit, arg);
                self.operand.runner().write(ctx, res);
            }
            CBOperation::SetBit(bit) => {
                let res = microcode::defs::set_bit(bit, arg);
                self.operand.runner().write(ctx, res);
            }
        }
    }
}

#[cfg(test)]
use crate::{gbz80core::TestGb, memdev::GrowRam};

executor_tests! {
    tests,
    DirectExecutor,
    TestGb<GrowRam, ()>
}
