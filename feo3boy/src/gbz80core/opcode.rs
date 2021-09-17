use std::fmt;

use log::{debug, trace};

use super::oputils::{add8_flags, offset_addr, rotate_left9, rotate_right9, sub8_flags};
use super::{AluOp, AluUnaryOp, ConditionCode, CpuContext, Flags, Operand16, Operand8};
use crate::interrupts::InterruptFlags;
use crate::memdev::MemDevice;

// Opcode References:
// - Decoding: www.z80.info/decoding.htm
// - GB Z80 Opcode Table: https://izik1.github.io/gbops/
// - Regular Z80 Opcode Table: http://z80-heaven.wikidot.com/opcode-reference-chart
// - Regular Z80 Flag Reference: http://www.z80.info/z80sflag.htm
// - GB Z80 Instruction Reference: https://rgbds.gbdev.io/docs/v0.4.1/gbz80.7

/// Parsed Opcode, not including any arguments that may be loaded from immediates, nor any follow-up
/// ops if the operation is a prefixed op.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Opcode {
    /// No operation.
    Nop,
    /// Stop -- not sure what this does or how this differs from Halt.
    Stop,
    /// Relative jump. Load a signed immediate for the jump destination, then check the condition,
    /// then jump if the condition is met.
    JumpRelative(ConditionCode),
    /// Increment the given 8 bit operand.
    Inc8(Operand8),
    /// Decrement the given 8 bit operand.
    Dec8(Operand8),
    /// Load a value from on 8 bit operand to another.
    Load8 { dest: Operand8, source: Operand8 },
    /// Increment the given 16 bit operand.
    Inc16(Operand16),
    /// Decrement the given 16 bit operand.
    Dec16(Operand16),
    /// Load a 16 bit value from one operand to another.
    Load16 { dest: Operand16, source: Operand16 },
    /// Add the given 16 bit operand to HL.
    Add16(Operand16),
    /// Halt instruction. Pauses but still accepts interrupts.
    Halt,
    /// Run the given operation on the ALU. The source is given by the operand, the destination is
    /// always `A`, the accumulator register.
    AluOp { op: AluOp, operand: Operand8 },
    /// Run the given unary operation on the ALU. These ops all affect only the accumulator and
    /// flags, or just the flags.
    AluUnary(AluUnaryOp),
    /// Conditional call. Load unsigned 16 bit immediate, check condtion, then if condition matches
    /// push current PC and jump to the loaded address.
    Call(ConditionCode),
    /// Conditional absolute jump. Load unsigned 16 bit immediate, check condition, then if
    /// condition matches, jump to the loaded address.
    Jump(ConditionCode),
    /// Conditional return. Check the condition, then pop the stack and jump to the address.
    Ret(ConditionCode),
    /// Push a 16 bit register pair to the stack, moving the stack pointer.
    Push(Operand16),
    /// Pop a 16 bit register pair from the stack, moving the stack pointer.
    Pop(Operand16),
    /// Loads and executes a prefixed instruction code.
    PrefixCB,
    /// Disable interrupt handling.
    DisableInterrupts,
    /// Enable Interrupt handling.
    EnableInterrupts,
    /// Interrupt return, similar to unconditional return but also enables interrupts.
    RetInterrupt,
    /// Load an 8 bit *signed* immediate and add it to the stack pointer.
    OffsetSp,
    /// Load an 8 bit *signed* immediate and add it to the stack pointer, storing the result in HL.
    AddressOfOffsetSp,
    /// Jump to the address stored in HL.
    JumpHL,
    /// Reset with numeric arg.
    Reset(u8),
    /// The instruction decoded to an opcode the processor doesn't actually have. This functions
    /// equivalently to Nop (I think????). Contained value is the raw opcode.
    MissingInstruction(u8),
}

impl Opcode {
    /// Decodes a u8 to an `Opcode`. All possible u8 values are valid `Opcode`s, so this cannot
    /// fail.
    pub fn decode(opcode: u8) -> Self {
        // Based on www.z80.info/decoding.htm, but adjusted based on codes which don't exist on the
        // GB Z80, using https://izik1.github.io/gbops/. Also referencing
        let x = (opcode & 0b11000000) >> 6;
        let p = (opcode & 0b00110000) >> 4;
        let y = (opcode & 0b00111000) >> 3;
        let q = (opcode & 0b00001000) != 0;
        let z = opcode & 0b00000111;
        match x {
            0 => match z {
                0 => match y {
                    0 => Self::Nop,
                    1 => Self::Load16 {
                        dest: Operand16::AddrImmediate,
                        source: Operand16::Sp,
                    },
                    2 => Self::Stop,
                    3..=7 => Self::JumpRelative(ConditionCode::from_relative_cond_code(y)),
                    _ => unreachable!(),
                },
                1 => match q {
                    false => Self::Load16 {
                        dest: Operand16::from_pair_code_sp(p),
                        source: Operand16::Immediate,
                    },
                    true => Self::Add16(Operand16::from_pair_code_sp(p)),
                },
                2 => match q {
                    false => Self::Load8 {
                        dest: Operand8::from_indirect(p),
                        source: Operand8::A,
                    },
                    true => Self::Load8 {
                        dest: Operand8::A,
                        source: Operand8::from_indirect(p),
                    },
                },
                3 => match q {
                    false => Self::Inc16(Operand16::from_pair_code_sp(p)),
                    true => Self::Dec16(Operand16::from_pair_code_sp(p)),
                },
                4 => Self::Inc8(Operand8::from_regcode(y)),
                5 => Self::Dec8(Operand8::from_regcode(y)),
                6 => Self::Load8 {
                    dest: Operand8::from_regcode(y),
                    source: Operand8::Immediate,
                },
                7 => Self::AluUnary(AluUnaryOp::from_ycode(y)),
                _ => unreachable!(),
            },
            1 => match (z, y) {
                (6, 6) => Self::Halt,
                _ => Self::Load8 {
                    dest: Operand8::from_regcode(y),
                    source: Operand8::from_regcode(z),
                },
            },
            2 => Self::AluOp {
                op: AluOp::from_ycode(y),
                operand: Operand8::from_regcode(z),
            },
            3 => match z {
                0 => match y {
                    0..=3 => Self::Ret(ConditionCode::from_absolute_cond_code(y)),
                    // On normal Z80 these are more conditionals, which are replaced with extra load
                    // ops on GB Z80.
                    4 => Self::Load8 {
                        dest: Operand8::AddrRelImmediate,
                        source: Operand8::A,
                    },
                    5 => Self::OffsetSp,
                    6 => Self::Load8 {
                        dest: Operand8::A,
                        source: Operand8::AddrRelImmediate,
                    },
                    7 => Self::AddressOfOffsetSp,
                    _ => unreachable!(),
                },
                1 => match q {
                    false => Opcode::Pop(Operand16::from_pair_code_af(p)),
                    true => match p {
                        0 => Opcode::Ret(ConditionCode::Unconditional),
                        1 => Opcode::RetInterrupt,
                        2 => Opcode::JumpHL,
                        3 => Opcode::Load16 {
                            dest: Operand16::Sp,
                            source: Operand16::HL,
                        },
                        _ => unreachable!(),
                    },
                },
                2 => match y {
                    0..=3 => Self::Jump(ConditionCode::from_absolute_cond_code(y)),
                    // On normal Z80 these are more conditionals, which are replaced with extra load
                    // ops on GB Z80.
                    4 => Self::Load8 {
                        dest: Operand8::AddrRelC,
                        source: Operand8::A,
                    },
                    5 => Self::Load8 {
                        dest: Operand8::AddrImmediate,
                        source: Operand8::A,
                    },
                    6 => Self::Load8 {
                        dest: Operand8::A,
                        source: Operand8::AddrRelC,
                    },
                    7 => Self::Load8 {
                        dest: Operand8::A,
                        source: Operand8::AddrImmediate,
                    },
                    _ => unreachable!(),
                },
                3 => match y {
                    0 => Self::Jump(ConditionCode::Unconditional),
                    1 => Self::PrefixCB,
                    // In, Out and EX insturctions are missing on GB Z80.
                    2..=5 => Self::MissingInstruction(opcode),
                    6 => Self::DisableInterrupts,
                    7 => Self::EnableInterrupts,
                    _ => unreachable!(),
                },
                4 => match y {
                    0..=3 => Self::Call(ConditionCode::from_absolute_cond_code(y)),
                    // These are conditional calls for conditions the GB Z80 doesn't have.
                    4..=7 => Self::MissingInstruction(opcode),
                    _ => unreachable!(),
                },
                5 => match q {
                    false => Opcode::Push(Operand16::from_pair_code_af(p)),
                    true => match p {
                        0 => Opcode::Call(ConditionCode::Unconditional),
                        // These are prefix instructions for prefixes the GB Z80 doesn't support.
                        1..=3 => Opcode::MissingInstruction(opcode),
                        _ => unreachable!(),
                    },
                },
                6 => Self::AluOp {
                    op: AluOp::from_ycode(y),
                    operand: Operand8::Immediate,
                },
                7 => Opcode::Reset(y * 8),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    /// Load and execute a single opcode from the given context.
    pub(super) fn load_and_execute(ctx: &mut impl CpuContext) {
        let pc = ctx.cpustate().regs.pc;
        trace!("Loading opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.read(ctx);
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
            Self::MissingInstruction(_) => {}
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Nop => f.write_str("NOP"),
            Self::Stop => f.write_str("STOP"),
            Self::JumpRelative(ConditionCode::Unconditional) => f.write_str("JR i8"),
            Self::JumpRelative(code) => write!(f, "JR {},i8", code),
            Self::Inc8(operand) => write!(f, "INC {}", operand),
            Self::Dec8(operand) => write!(f, "DEC {}", operand),
            Self::Load8 { dest, source } => write!(f, "LD {},{}", dest, source),
            Self::Inc16(operand) => write!(f, "INC {}", operand),
            Self::Dec16(operand) => write!(f, "DEC {}", operand),
            Self::Load16 { dest, source } => write!(f, "LD {},{}", dest, source),
            Self::Add16(operand) => write!(f, "ADD HL,{}", operand),
            Self::Halt => f.write_str("HALT"),
            Self::AluOp { op, operand } => write!(f, "{} A,{}", op, operand),
            Self::AluUnary(op) => fmt::Display::fmt(&op, f),
            Self::Call(ConditionCode::Unconditional) => f.write_str("CALL u16"),
            Self::Call(code) => write!(f, "CALL {},u16", code),
            Self::Jump(ConditionCode::Unconditional) => f.write_str("JP u16"),
            Self::Jump(code) => write!(f, "JP {},u16", code),
            Self::Ret(ConditionCode::Unconditional) => f.write_str("RET"),
            Self::Ret(code) => write!(f, "RET {}", code),
            Self::Pop(operand) => write!(f, "POP {}", operand),
            Self::Push(operand) => write!(f, "PUSH {}", operand),
            Self::PrefixCB => f.write_str("PREFIX CB"),
            Self::DisableInterrupts => f.write_str("DI"),
            Self::EnableInterrupts => f.write_str("EI"),
            Self::RetInterrupt => f.write_str("RETI"),
            Self::OffsetSp => f.write_str("ADD SP,i8"),
            Self::AddressOfOffsetSp => f.write_str("LD HL,SP+i8"),
            Self::JumpHL => f.write_str("JP HL"),
            Self::Reset(target) => write!(f, "RST {:02X}h", target),
            Self::MissingInstruction(opcode) => write!(f, "<Missing Instruction {:2X}>", opcode),
        }
    }
}

//////////////////////////
// Opcode Implementations:
//////////////////////////

/// Implements the relative jump instruction.
fn jump_relative(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Loading the offset also moves the program counter over the next instruction, which is
    // good because the jump is relative to the following instruction.
    let offset = Operand8::Immediate.read(ctx) as i8;
    let base = ctx.cpustate().regs.pc;
    // JR doesn't set any flags.
    let (dest, _) = offset_addr(base, offset);
    if cond.evaluate(ctx) {
        trace!("Relative jump by {} from {} to {}", offset, base, dest);
        ctx.cpustate_mut().regs.pc = dest;
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
    ctx.cpustate_mut().regs.flags.merge(flags, MASK);
    operand.write(ctx, res);
}

/// Implements 8 bit decrement instruction.
fn dec8(ctx: &mut impl CpuContext, operand: Operand8) {
    // Dec doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    let val = operand.read(ctx);
    let (res, flags) = sub8_flags(val, 1);
    trace!("Evaluating DEC {} ({} => {})", operand, val, res);
    ctx.cpustate_mut().regs.flags.merge(flags, MASK);
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

    let lhs = ctx.cpustate().regs.hl();
    let rhs = arg.read(ctx); // This will always be a register in practice.

    let mut flags = Flags::empty();
    if (lhs & 0x7ff) + (rhs & 0x7ff) > 0x7ff {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = lhs.overflowing_add(rhs);
    flags |= Flags::check_carry(carry);

    ctx.cpustate_mut().regs.set_hl(res);
    ctx.cpustate_mut().regs.flags.merge(flags, MASK);
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
fn halt(ctx: &mut impl CpuContext) {
    if ctx.cpustate().interrupt_master_enable.enabled() {
        // No need to special-case interrupts here, since the next `tick` call will un-halt anyway.
        ctx.cpustate_mut().halted = true;
    } else {
        let enabled_interrupts = InterruptFlags::get_interrupt_enable(ctx.mem());
        let pending_interrupts = ctx.cpustate().interrupt_vector;
        if enabled_interrupts.intersects(pending_interrupts) {
            panic!("Halt-Bug encountered (see method description).");
        } else {
            // `tick` will un-halt next time ([IE] & [IF] != 0), but will not service the interrupt.
            ctx.cpustate_mut().halted = true;
        }
    }
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
        push_helper(ctx, ctx.cpustate().regs.pc);
        ctx.cpustate_mut().regs.pc = dest;
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
        ctx.cpustate_mut().regs.pc = dest;
    }
}

/// Performs a conditional return.
fn ret(ctx: &mut impl CpuContext, cond: ConditionCode) {
    // Unlike Jump and Call, Ret is different depending on whether it is conditional or unconditional.
    if cond == ConditionCode::Unconditional {
        let dest = pop_helper(ctx);
        // There's an extra 1m delay after loading SP.
        ctx.yield1m();
        ctx.cpustate_mut().regs.pc = dest;
    } else {
        // Conditional branch always has this extra delay before evaluating.
        ctx.yield1m();
        if cond.evaluate(ctx) {
            let dest = pop_helper(ctx);
            // But there's also an extra delay after reading.
            ctx.yield1m();
            ctx.cpustate_mut().regs.pc = dest;
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
    let addr = ctx.cpustate_mut().regs.dec_sp();
    ctx.mem_mut().write(addr.into(), high);

    ctx.yield1m();
    let addr = ctx.cpustate_mut().regs.dec_sp();
    ctx.mem_mut().write(addr.into(), low);
}

/// Pop helper, shared between pop and ret. Pops value from the stack, waiting 1m between each byte
/// and incrementing the stack pointer by 2.
fn pop_helper(ctx: &mut impl CpuContext) -> u16 {
    ctx.yield1m();
    let addr = ctx.cpustate_mut().regs.inc_sp();
    let low = ctx.mem().read(addr.into());

    ctx.yield1m();
    let addr = ctx.cpustate_mut().regs.inc_sp();
    let high = ctx.mem().read(addr.into());

    u16::from_le_bytes([low, high])
}

/// DI instruction (applies immediately).
fn disable_interrupts(ctx: &mut impl CpuContext) {
    ctx.cpustate_mut().interrupt_master_enable.clear();
}

/// EI instruction (applies after the following instruction).
fn enable_interrupts(ctx: &mut impl CpuContext) {
    ctx.cpustate_mut()
        .interrupt_master_enable
        .set_next_instruction();
}

/// Enabled interrupts and returns.
fn interrupt_return(ctx: &mut impl CpuContext) {
    let dest = pop_helper(ctx);
    // Theres an extra 1m of delay in here.
    ctx.yield1m();
    ctx.cpustate_mut().regs.pc = dest;
    ctx.cpustate_mut().interrupt_master_enable.set();
}

/// Offsets the stack pointer by an immediate value.
fn offset_sp(ctx: &mut impl CpuContext) {
    let offset = Operand8::Immediate.read(ctx) as i8;
    let (res, flags) = offset_addr(ctx.cpustate().regs.sp, offset);
    // This instruction takes two more cycles after loading the offset.
    ctx.yield1m();
    ctx.yield1m();
    ctx.cpustate_mut().regs.sp = res;
    ctx.cpustate_mut().regs.flags = flags;
}

/// Loads the result of offsetting the stack pointer by an immediate value into HL.
fn address_of_offset_sp(ctx: &mut impl CpuContext) {
    let offset = Operand8::Immediate.read(ctx) as i8;
    let (res, flags) = offset_addr(ctx.cpustate().regs.sp, offset);
    // Interestingly, this instruction is actually faster than `ADD SP,i8`.
    ctx.yield1m();
    ctx.cpustate_mut().regs.set_hl(res);
    ctx.cpustate_mut().regs.flags = flags;
}

// Similar to unconditional jump, but using HL as the target address.
fn jump_hl(ctx: &mut impl CpuContext) {
    let regs = &mut ctx.cpustate_mut().regs;
    regs.sp = regs.hl();
}

/// Executes the reset instruction. Similar to call with a fixed destination.
fn reset(ctx: &mut impl CpuContext, dest: u8) {
    // There's an extra delay at the start of an RST instruction.
    ctx.yield1m();
    push_helper(ctx, ctx.cpustate().regs.pc);
    ctx.cpustate_mut().regs.pc = dest as u16;
}

//////////////////////
// CB Prefixed Opcodes
//////////////////////

/// Opcodes that come after a CB prefix opcode.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CBOpcode {
    /// The operand to operate on.
    pub operand: Operand8,
    /// The operation being performed.
    pub op: CBOperation,
}

impl CBOpcode {
    /// Decodes an 8-bit opcode found after a CB prefix into a CBOpcode.
    pub fn decode(opcode: u8) -> Self {
        let x = (opcode & 0b11000000) >> 6;
        let y = (opcode & 0b00111000) >> 3;
        let z = opcode & 0b00000111;
        let operand = Operand8::from_regcode(z);
        let op = match x {
            0 => match y {
                0 => CBOperation::RotateLeft8,
                1 => CBOperation::RotateRight8,
                2 => CBOperation::RotateLeft9,
                3 => CBOperation::RotateRight9,
                4 => CBOperation::ShiftLeft,
                5 => CBOperation::ShiftRightSignExt,
                6 => CBOperation::Swap,
                7 => CBOperation::ShiftRight,
                _ => unreachable!(),
            },
            1 => CBOperation::TestBit(y),
            2 => CBOperation::ResetBit(y),
            3 => CBOperation::SetBit(y),
            _ => unreachable!(),
        };
        Self { operand, op }
    }

    /// Load and execute a single CB-prefixed opcode from the given context.
    fn load_and_execute(ctx: &mut impl CpuContext) {
        let pc = ctx.cpustate().regs.pc;
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
                ctx.cpustate_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(res & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::RotateLeft9 => {
                let (res, flags) = rotate_left9(arg, ctx.cpustate().regs.flags);
                ctx.cpustate_mut().regs.flags = flags;
                self.operand.write(ctx, res);
            }
            CBOperation::RotateRight8 => {
                let res = arg.rotate_right(1);
                ctx.cpustate_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(res & 0x80 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::RotateRight9 => {
                let (res, flags) = rotate_right9(arg, ctx.cpustate().regs.flags);
                ctx.cpustate_mut().regs.flags = flags;
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftLeft => {
                let res = arg << 1;
                ctx.cpustate_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 0x80 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftRightSignExt => {
                let res = ((arg as i8) >> 1) as u8;
                ctx.cpustate_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::ShiftRight => {
                let res = arg >> 1;
                ctx.cpustate_mut().regs.flags =
                    Flags::check_zero(res) | Flags::check_carry(arg & 1 != 0);
                self.operand.write(ctx, res);
            }
            CBOperation::Swap => {
                let res = ((arg & 0x0f) << 4) | ((arg & 0xf0) >> 4);
                ctx.cpustate_mut().regs.flags = Flags::check_zero(res);
                self.operand.write(ctx, res);
            }
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                let flags = Flags::check_zero(arg & (1 << bit)) | Flags::HALFCARRY;
                ctx.cpustate_mut().regs.flags.merge(flags, MASK);
            }
            CBOperation::ResetBit(bit) => self.operand.write(ctx, arg & !(1 << bit)),
            CBOperation::SetBit(bit) => self.operand.write(ctx, arg | (1 << bit)),
        }
    }
}

impl fmt::Display for CBOpcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.op.is_bit_op() {
            write!(f, "{},{}", self.op, self.operand)
        } else {
            write!(f, "{} {}", self.op, self.operand)
        }
    }
}

/// Type of operation performed in a CB prefix opcode.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum CBOperation {
    /// 8-bit left rotate. Bit 7 goes to both the carry and bit 0.
    RotateLeft8,
    /// 9-bit left rotate. Bit 7 goes to carry and carry goes to bit 0.
    RotateLeft9,
    /// 8-bit right rotate. Bit 0 goes to both the carry and bit 7.
    RotateRight8,
    /// 9-bit left rotate. Bit 0 goes to carry and carry goes to bit 7.
    RotateRight9,
    /// Shift left. Bit 7 gotes to carry, and 0 fills in Bit 0.
    ShiftLeft,
    /// Shift right. Bit 0 goes to carry, and 0 fills in Bit 7.
    ShiftRight,
    /// Shift right with sign-extension. Bit 0 goes to carry, and Bit 7 is copied with its current
    /// value.
    ShiftRightSignExt,
    /// Swap the nybbles of the byte.
    Swap,
    /// Check if the given bit (given as an index in range 0..=7) is set in the operand.
    TestBit(u8),
    /// Sets the given bit (given as an index in range 0..=7) in the operand.
    SetBit(u8),
    /// Clears the given bit (given as an index in range 0..=7) in the operand.
    ResetBit(u8),
}

impl CBOperation {
    /// Returns true if this CBOperation is one of the ones that affects a single bit. Otherwise, it
    /// is one of the rotate/shift/swap operations.
    fn is_bit_op(self) -> bool {
        matches!(self, Self::TestBit(_) | Self::SetBit(_) | Self::ResetBit(_))
    }
}

impl fmt::Display for CBOperation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::RotateLeft8 => f.write_str("RLC"),
            Self::RotateRight8 => f.write_str("RRC"),
            Self::RotateLeft9 => f.write_str("RL"),
            Self::RotateRight9 => f.write_str("RR"),
            Self::ShiftLeft => f.write_str("SLA"),
            Self::ShiftRightSignExt => f.write_str("SRA"),
            Self::Swap => f.write_str("SWAP"),
            Self::ShiftRight => f.write_str("SRL"),
            Self::TestBit(bit) => write!(f, "BIT {}", bit),
            Self::ResetBit(bit) => write!(f, "RES {}", bit),
            Self::SetBit(bit) => write!(f, "SET {}", bit),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_decode_any_opcode() {
        for op in 0u8..=0xff {
            Opcode::decode(op);
        }
    }

    #[test]
    fn can_decode_any_cb_opcode() {
        for op in 0u8..=0xff {
            CBOpcode::decode(op);
        }
    }
}
