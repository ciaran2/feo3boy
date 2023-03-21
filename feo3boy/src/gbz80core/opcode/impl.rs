//! This is the direct-execution implementation of [`crate::gbz80core::opcode`]. It
//! provides methods to directly execute opcodes on the CPU using the `yield1m()` function
//! for delays.
//!
//! This implementation is only provided when the `microcode` feature is disabled.

use log::{debug, trace, warn};

use crate::gbz80core::opcode::args::{AluOp, ConditionCode, Operand16, Operand8};
use crate::gbz80core::opcode::{CBOpcode, CBOperation, Opcode};
use crate::gbz80core::oputils::{
    add8_flags, halt, offset_addr, rotate_left9, rotate_right9, sub8_flags,
};
use crate::gbz80core::{CpuContext, Flags};
use crate::interrupts::Interrupts;
use crate::memdev::MemDevice;

impl Opcode {
    /// Load and execute a single opcode from the given context.
    pub(in crate::gbz80core) fn load_and_execute(ctx: &mut impl CpuContext) {
        let pc = ctx.cpu().regs.pc;
        trace!("Loading opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.read(ctx);
        if ctx.cpu().halt_bug {
            let state = ctx.cpu_mut();
            state.regs.pc = state.regs.pc.wrapping_sub(1);
            state.halt_bug = false;
        }
        let opcode = Self::decode(opcode);
        debug!("Executing @ {:#6X}: {}", pc, opcode);
        opcode.execute(ctx);
    }

    /// Execute this opcode on the given context.
    fn execute(self, ctx: &mut impl CpuContext) {
        match self {
            Self::Nop => {}
            Self::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Self::JumpRelative(cond) => jump_relative(ctx, cond),
            Self::Inc8(operand) => inc8(ctx, operand),
            Self::Dec8(operand) => dec8(ctx, operand),
            Self::Load8 { dest, source } => load8(ctx, dest, source),
            Self::Inc16(operand) => inc16(ctx, operand),
            Self::Dec16(operand) => dec16(ctx, operand),
            Self::Load16 { dest, source } => load16(ctx, dest, source),
            Self::Add16(operand) => add16(ctx, operand),
            Self::Halt => halt(ctx),
            Self::AluOp { operand, op } => alu_op(ctx, operand, op),
            Self::AluUnary(op) => op.exec(ctx),
            Self::Call(cond) => call(ctx, cond),
            Self::Jump(cond) => jump(ctx, cond),
            Self::Ret(cond) => ret(ctx, cond),
            Self::Push(operand) => push(ctx, operand),
            Self::Pop(operand) => pop(ctx, operand),
            Self::PrefixCB => CBOpcode::load_and_execute(ctx),
            Self::DisableInterrupts => disable_interrupts(ctx),
            Self::EnableInterrupts => enable_interrupts(ctx),
            Self::RetInterrupt => interrupt_return(ctx),
            Self::OffsetSp => offset_sp(ctx),
            Self::AddressOfOffsetSp => address_of_offset_sp(ctx),
            Self::JumpHL => jump_hl(ctx),
            Self::Reset(dest) => reset(ctx, dest),
            // A brief note on this doc page:
            // https://gbdev.io/pandocs/CPU_Comparison_with_Z80.html
            // says that the unused opcodes will lock up the CPU, rather than behave as a
            // no-op.
            Self::MissingInstruction(opcode) => {
                warn!(
                    "Missing instruction {:#04X} encountered. Treating as NOP instead of locking.",
                    opcode
                );
            }
        }
    }
}

/// Implements the relative jump instruction.
fn jump_relative(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Loading the offset also moves the program counter over the next instruction, which is
    // good because the jump is relative to the following instruction.
    let offset = Operand8::Immediate.read(ctx) as i8;
    let base = ctx.cpu().regs.pc;
    // JR doesn't set any flags.
    let (dest, _) = offset_addr(base, offset);
    if cond.evaluate(ctx) {
        trace!("Relative jump by {} from {} to {}", offset, base, dest);
        // Modifying the PC takes an extra tick, which isn't used if the condition fails.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    } else {
        trace!("Skipping jump by {} from {} to {}", offset, base, dest);
    }
}

/// Implements 8 bit increment instruction.
fn inc8(ctx: &mut impl CpuContext, operand: Operand8) {
    // Inc doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    let val = operand.read(ctx);
    let (res, flags) = add8_flags(val, 1);
    trace!("Evaluating INC {} ({} => {})", operand, val, res);
    ctx.cpu_mut().regs.flags.merge(flags, MASK);
    operand.write(ctx, res);
}

/// Implements 8 bit decrement instruction.
fn dec8(ctx: &mut impl CpuContext, operand: Operand8) {
    // Dec doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    let val = operand.read(ctx);
    let (res, flags) = sub8_flags(val, 1);
    trace!("Evaluating DEC {} ({} => {})", operand, val, res);
    ctx.cpu_mut().regs.flags.merge(flags, MASK);
    operand.write(ctx, res);
}

/// Implements 8 bit load operations.
fn load8(ctx: &mut impl CpuContext, dest: Operand8, source: Operand8) {
    let val = source.read(ctx);
    trace!("Evaluating LD {},{} (<- {})", dest, source, val);
    dest.write(ctx, val);
}

/// Implements 16 bit increment instruction.
fn inc16(ctx: &mut impl CpuContext, operand: Operand16) {
    // 16 bit inc doesn't set any flags, and all actual operands are always registers, but it does
    // delay by 1 additional M cycle, probably because it has to operate on two bytes.
    let val = operand.read(ctx);
    let res = val.wrapping_add(1);
    trace!("Evaluating INC {} ({} => {})", operand, val, res);
    ctx.yield1m();
    operand.write(ctx, res);
}

/// Implements 16 bit decrement instruction.
fn dec16(ctx: &mut impl CpuContext, operand: Operand16) {
    // 16 bit dec doesn't set any flags, and all actual operands are always registers, but it does
    // delay by 1 additional M cycle, probably because it has to operate on two bytes.
    let val = operand.read(ctx);
    let res = val.wrapping_sub(1);
    trace!("Evaluating DEC {} ({} => {})", operand, val, res);
    ctx.yield1m();
    operand.write(ctx, res);
}

/// Implements 16 bit load operations.
fn load16(ctx: &mut impl CpuContext, dest: Operand16, source: Operand16) {
    let val = source.read(ctx);
    if (dest, source) == (Operand16::Sp, Operand16::HL) {
        // Most of the 16 bit loads are <Pair>,<Immediate> and take time based on number of memory
        // accesses. There are two exceptions. LD (u16),SP, which is also just timed based on the
        // number of memory accesses, and LD SP,HL, which is all registers but still takes an extra
        // 1m cycle, which isn't automatically provided by Operand16 register interactions, so we
        // insert it here.
        ctx.yield1m();
    }
    trace!("Evaluating LD {},{} (<- {})", dest, source, val);
    dest.write(ctx, val);
}

/// Implements 16 bit register add into HL. Never sets the zero flag and clears the subtract flag,
/// but does set carry and half-carry based on the upper byte of the operation (as if it was
/// performed by running the pseudo-instructions `add l,<arg-low>; adc h,<arg-high>`.
fn add16(ctx: &mut impl CpuContext, arg: Operand16) {
    // 16 bit add never modifies the zero flag.
    const MASK: Flags = Flags::all().difference(Flags::ZERO);

    let lhs = ctx.cpu().regs.hl();
    let rhs = arg.read(ctx); // This will always be a register in practice.

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
fn alu_op(ctx: &mut impl CpuContext, operand: Operand8, op: AluOp) {
    let arg = operand.read(ctx);
    op.exec(ctx, arg);
}

/// Performs a conditional call.
fn call(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Conveniently, unconditional call behaves exactly the same as a conditional call with a true
    // value, down to the timing.
    let dest = Operand16::Immediate.read(ctx);
    if cond.evaluate(ctx) {
        // Conditional jump has an extra internal delay if the condition is true.
        ctx.yield1m();
        push_helper(ctx, ctx.cpu().regs.pc);
        ctx.cpu_mut().regs.pc = dest;
    }
}

/// Performs a conditional absolute jump.
fn jump(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Conveniently, unconditional call behaves exactly the same as a conditional call with a true
    // value, down to the timing.
    let dest = Operand16::Immediate.read(ctx);
    if cond.evaluate(ctx) {
        // Branching adds an extra cycle despite not accessing memory.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    }
}

/// Performs a conditional return.
fn ret(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Unlike Jump and Call, Ret is different depending on whether it is conditional or unconditional.
    if cond == ConditionCode::Unconditional {
        let dest = pop_helper(ctx);
        // There's an extra 1m delay after loading SP.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
    } else {
        // Conditional branch always has this extra delay before evaluating.
        ctx.yield1m();
        if cond.evaluate(ctx) {
            let dest = pop_helper(ctx);
            // But there's also an extra delay after reading.
            ctx.yield1m();
            ctx.cpu_mut().regs.pc = dest;
        }
    }
}

/// Implements push instruction.
fn push(ctx: &mut impl CpuContext, operand: Operand16) {
    // In practice, operand is always a register.
    let val = operand.read(ctx);
    // Push has an extra delay before writing.
    ctx.yield1m();
    push_helper(ctx, val);
}

/// Implements pop instruction.
fn pop(ctx: &mut impl CpuContext, operand: Operand16) {
    let val = pop_helper(ctx);
    // In practice, operand is always a register.
    operand.write(ctx, val)
}

/// Push helper, shared between push and call. Pushes a caller-supplied 16 bit value onto the stack,
/// waiting 1m between each byte and decrementing the stack pointer by 2.
fn push_helper(ctx: &mut impl CpuContext, val: u16) {
    let [low, high] = val.to_le_bytes();
    ctx.yield1m();
    let addr = ctx.cpu_mut().regs.dec_sp();
    ctx.mem_mut().write(addr.into(), high);

    ctx.yield1m();
    let addr = ctx.cpu_mut().regs.dec_sp();
    ctx.mem_mut().write(addr.into(), low);
}

/// Pop helper, shared between pop and ret. Pops value from the stack, waiting 1m between each byte
/// and incrementing the stack pointer by 2.
fn pop_helper(ctx: &mut impl CpuContext) -> u16 {
    ctx.yield1m();
    let addr = ctx.cpu_mut().regs.inc_sp();
    let low = ctx.mem().read(addr.into());

    ctx.yield1m();
    let addr = ctx.cpu_mut().regs.inc_sp();
    let high = ctx.mem().read(addr.into());

    u16::from_le_bytes([low, high])
}

/// Checks if an interrupt should be serviced, and if so performs the hidden isr
/// instruction to jump to the interrupt handler. If an interrupt was handled, returns
/// true.
pub(in crate::gbz80core) fn service_interrupt(ctx: &mut impl CpuContext) -> bool {
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

/// DI instruction (applies immediately).
fn disable_interrupts(ctx: &mut impl CpuContext) {
    ctx.cpu_mut().interrupt_master_enable.clear();
}

/// EI instruction (applies after the following instruction).
fn enable_interrupts(ctx: &mut impl CpuContext) {
    ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
}

/// Enabled interrupts and returns.
fn interrupt_return(ctx: &mut impl CpuContext) {
    let dest = pop_helper(ctx);
    // Theres an extra 1m of delay in here.
    ctx.yield1m();
    ctx.cpu_mut().regs.pc = dest;
    ctx.cpu_mut().interrupt_master_enable.set();
}

/// Offsets the stack pointer by an immediate value.
fn offset_sp(ctx: &mut impl CpuContext) {
    let offset = Operand8::Immediate.read(ctx) as i8;
    let (res, flags) = offset_addr(ctx.cpu().regs.sp, offset);
    // This instruction takes two more cycles after loading the offset.
    ctx.yield1m();
    ctx.yield1m();
    ctx.cpu_mut().regs.sp = res;
    ctx.cpu_mut().regs.flags = flags;
}

/// Loads the result of offsetting the stack pointer by an immediate value into HL.
fn address_of_offset_sp(ctx: &mut impl CpuContext) {
    let offset = Operand8::Immediate.read(ctx) as i8;
    let (res, flags) = offset_addr(ctx.cpu().regs.sp, offset);
    // Interestingly, this instruction is actually faster than `ADD SP,i8`.
    ctx.yield1m();
    ctx.cpu_mut().regs.set_hl(res);
    ctx.cpu_mut().regs.flags = flags;
}

// Similar to unconditional jump, but using HL as the target address.
fn jump_hl(ctx: &mut impl CpuContext) {
    let regs = &mut ctx.cpu_mut().regs;
    regs.pc = regs.hl();
}

/// Executes the reset instruction. Similar to call with a fixed destination.
fn reset(ctx: &mut impl CpuContext, dest: u8) {
    // There's an extra delay at the start of an RST instruction.
    ctx.yield1m();
    push_helper(ctx, ctx.cpu().regs.pc);
    ctx.cpu_mut().regs.pc = dest as u16;
}

impl CBOpcode {
    /// Load and execute a single CB-prefixed opcode from the given context.
    fn load_and_execute(ctx: &mut impl CpuContext) {
        let pc = ctx.cpu().regs.pc;
        trace!("Loading CB-opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.read(ctx);
        let opcode = Self::decode(opcode);
        debug!("Executing CB @ {:#6X}: {}", pc, opcode);
        opcode.execute(ctx);
    }

    /// Execute this opcode on the given context.
    fn execute(self, ctx: &mut impl CpuContext) {
        let arg = self.operand.read(ctx);
        match self.op {
            CBOperation::RotateLeft8 => {
                let res = arg.rotate_left(1);
                ctx.cpu_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(res & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::RotateLeft9 => {
                let (res, flags) = rotate_left9(arg, ctx.cpu().regs.flags);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.write(ctx, res);
            }
            CBOperation::RotateRight8 => {
                let res = arg.rotate_right(1);
                ctx.cpu_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(res & 0x80 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::RotateRight9 => {
                let (res, flags) = rotate_right9(arg, ctx.cpu().regs.flags);
                ctx.cpu_mut().regs.flags = flags;
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftLeft => {
                let res = arg << 1;
                ctx.cpu_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 0x80 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftRightSignExt => {
                let res = ((arg as i8) >> 1) as u8;
                ctx.cpu_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftRight => {
                let res = arg >> 1;
                ctx.cpu_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::Swap => {
                let res = ((arg & 0x0f) << 4) | ((arg & 0xf0) >> 4);
                ctx.cpu_mut().regs.flags = Flags::check_zero(res);
                self.operand.write(ctx, res);
            }
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                let flags = Flags::check_zero(arg & (1 << bit)) | Flags::HALFCARRY;
                ctx.cpu_mut().regs.flags.merge(flags, MASK);
            }
            CBOperation::ResetBit(bit) => self.operand.write(ctx, arg & !(1 << bit)),
            CBOperation::SetBit(bit) => self.operand.write(ctx, arg | (1 << bit)),
        }
    }
}