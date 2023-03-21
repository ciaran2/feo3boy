use std::fmt;
#[cfg(feature = "microcode")]
use std::mem::{self, MaybeUninit};

#[cfg(not(feature = "microcode"))]
use log::{debug, trace, warn};
#[cfg(feature = "microcode")]
use once_cell::sync::Lazy;

#[cfg(feature = "microcode")]
use crate::gbz80core::microcode::Instr;
use crate::gbz80core::microcode::{
    BitOp, GbStack16, Immediate8, InstrDef, Microcode, MicrocodeBuilder, Reg16, UnaryOp,
};
#[cfg(not(feature = "microcode"))]
use crate::gbz80core::oputils::{halt, rotate_left9, rotate_right9};
#[cfg(not(feature = "microcode"))]
use crate::gbz80core::CpuContext;
use crate::gbz80core::{AluOp, AluUnaryOp, ConditionCode, Flags, Operand16, Operand8};

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

    /// Get the [`Instr`] for a particular Opcode.
    #[cfg(feature = "microcode")]
    pub fn get_instruction(opcode: u8) -> Instr {
        /// Lookup table for Instrs for particular opcodes.
        static OPCODE_TABLE: Lazy<[InstrDef; 256]> = Lazy::new(|| {
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<InstrDef>; 256] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for i in 0..256 {
                data[i].write(Opcode::decode(i as u8).into());
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [InstrDef; 256]>(data) }
        });

        Instr::new(&OPCODE_TABLE[opcode as usize])
    }

    /// Load and execute a single opcode from the given context.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn load_and_execute(ctx: &mut impl CpuContext) {
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
    #[cfg(not(feature = "microcode"))]
    fn execute(self, ctx: &mut impl CpuContext) {
        match self {
            Self::Nop => {}
            Self::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Self::JumpRelative(cond) => ops::jump_relative(ctx, cond),
            Self::Inc8(operand) => ops::inc8(ctx, operand),
            Self::Dec8(operand) => ops::dec8(ctx, operand),
            Self::Load8 { dest, source } => ops::load8(ctx, dest, source),
            Self::Inc16(operand) => ops::inc16(ctx, operand),
            Self::Dec16(operand) => ops::dec16(ctx, operand),
            Self::Load16 { dest, source } => ops::load16(ctx, dest, source),
            Self::Add16(operand) => ops::add16(ctx, operand),
            Self::Halt => halt(ctx),
            Self::AluOp { operand, op } => ops::alu_op(ctx, operand, op),
            Self::AluUnary(op) => op.exec(ctx),
            Self::Call(cond) => ops::call(ctx, cond),
            Self::Jump(cond) => ops::jump(ctx, cond),
            Self::Ret(cond) => ops::ret(ctx, cond),
            Self::Push(operand) => ops::push(ctx, operand),
            Self::Pop(operand) => ops::pop(ctx, operand),
            Self::PrefixCB => CBOpcode::load_and_execute(ctx),
            Self::DisableInterrupts => ops::disable_interrupts(ctx),
            Self::EnableInterrupts => ops::enable_interrupts(ctx),
            Self::RetInterrupt => ops::interrupt_return(ctx),
            Self::OffsetSp => ops::offset_sp(ctx),
            Self::AddressOfOffsetSp => ops::address_of_offset_sp(ctx),
            Self::JumpHL => ops::jump_hl(ctx),
            Self::Reset(dest) => ops::reset(ctx, dest),
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

impl From<Opcode> for InstrDef {
    fn from(value: Opcode) -> Self {
        let builder = match value {
            Opcode::Nop => MicrocodeBuilder::new(),
            Opcode::Stop => Microcode::Stop.into(),
            Opcode::JumpRelative(cond) => ucode_ops::jump_relative(cond),
            Opcode::Inc8(operand) => ucode_ops::inc8(operand),
            Opcode::Dec8(operand) => ucode_ops::dec8(operand),
            Opcode::Load8 { dest, source } => MicrocodeBuilder::read(source).then_write(dest),
            Opcode::Inc16(operand) => ucode_ops::inc16(operand),
            Opcode::Dec16(operand) => ucode_ops::dec16(operand),
            Opcode::Load16 { dest, source } => ucode_ops::load16(dest, source),
            Opcode::Add16(operand) => ucode_ops::add16(operand),
            Opcode::Halt => Microcode::Halt.into(),
            Opcode::AluOp { operand, op } => MicrocodeBuilder::read(operand).then(op),
            Opcode::AluUnary(op) => op.into(),
            Opcode::Call(cond) => ucode_ops::call(cond),
            Opcode::Jump(cond) => ucode_ops::jump(cond),
            Opcode::Ret(cond) => ucode_ops::ret(cond),
            Opcode::Push(operand) => MicrocodeBuilder::read(operand)
                // Push has an extra delay before writing.
                .then_yield()
                .then_write(GbStack16),
            Opcode::Pop(operand) => MicrocodeBuilder::read(GbStack16).then_write(operand),
            Opcode::PrefixCB => MicrocodeBuilder::read(Immediate8).then(Microcode::ParseCBOpcode),
            Opcode::DisableInterrupts => Microcode::DisableInterrupts.into(),
            Opcode::EnableInterrupts => Microcode::EnableInterrupts { immediate: false }.into(),
            Opcode::RetInterrupt => ucode_ops::interrupt_return(),
            Opcode::OffsetSp => ucode_ops::offset_sp(),
            Opcode::AddressOfOffsetSp => ucode_ops::address_of_offset_sp(),
            Opcode::JumpHL => MicrocodeBuilder::read(Reg16::HL).then_write(Reg16::Pc),
            Opcode::Reset(dest) => ucode_ops::reset(dest),
            // A brief note on this doc page:
            // https://gbdev.io/pandocs/CPU_Comparison_with_Z80.html
            // says that the unused opcodes will lock up the CPU, rather than behave as a
            // no-op.
            // However, we just build them as an empty instruction, which is a no-op.
            Opcode::MissingInstruction(_) => MicrocodeBuilder::new(),
        };
        builder.build(value.to_string())
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

#[cfg(not(feature = "microcode"))]
pub(super) use ops::service_interrupt;

/// Opcode Implementations:
#[cfg(not(feature = "microcode"))]
mod ops {
    use log::trace;

    use crate::gbz80core::oputils::{add8_flags, offset_addr, sub8_flags};
    use crate::gbz80core::{AluOp, ConditionCode, CpuContext, Flags, Operand16, Operand8};
    use crate::interrupts::Interrupts;
    use crate::memdev::MemDevice;

    /// Implements the relative jump instruction.
    pub(super) fn jump_relative(ctx: &mut impl CpuContext, cond: ConditionCode) {
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
    pub(super) fn inc8(ctx: &mut impl CpuContext, operand: Operand8) {
        // Inc doesn't set the carry flag.
        const MASK: Flags = Flags::all().difference(Flags::CARRY);

        let val = operand.read(ctx);
        let (res, flags) = add8_flags(val, 1);
        trace!("Evaluating INC {} ({} => {})", operand, val, res);
        ctx.cpu_mut().regs.flags.merge(flags, MASK);
        operand.write(ctx, res);
    }

    /// Implements 8 bit decrement instruction.
    pub(super) fn dec8(ctx: &mut impl CpuContext, operand: Operand8) {
        // Dec doesn't set the carry flag.
        const MASK: Flags = Flags::all().difference(Flags::CARRY);

        let val = operand.read(ctx);
        let (res, flags) = sub8_flags(val, 1);
        trace!("Evaluating DEC {} ({} => {})", operand, val, res);
        ctx.cpu_mut().regs.flags.merge(flags, MASK);
        operand.write(ctx, res);
    }

    /// Implements 8 bit load operations.
    pub(super) fn load8(ctx: &mut impl CpuContext, dest: Operand8, source: Operand8) {
        let val = source.read(ctx);
        trace!("Evaluating LD {},{} (<- {})", dest, source, val);
        dest.write(ctx, val);
    }

    /// Implements 16 bit increment instruction.
    pub(super) fn inc16(ctx: &mut impl CpuContext, operand: Operand16) {
        // 16 bit inc doesn't set any flags, and all actual operands are always registers, but it does
        // delay by 1 additional M cycle, probably because it has to operate on two bytes.
        let val = operand.read(ctx);
        let res = val.wrapping_add(1);
        trace!("Evaluating INC {} ({} => {})", operand, val, res);
        ctx.yield1m();
        operand.write(ctx, res);
    }

    /// Implements 16 bit decrement instruction.
    pub(super) fn dec16(ctx: &mut impl CpuContext, operand: Operand16) {
        // 16 bit dec doesn't set any flags, and all actual operands are always registers, but it does
        // delay by 1 additional M cycle, probably because it has to operate on two bytes.
        let val = operand.read(ctx);
        let res = val.wrapping_sub(1);
        trace!("Evaluating DEC {} ({} => {})", operand, val, res);
        ctx.yield1m();
        operand.write(ctx, res);
    }

    /// Implements 16 bit load operations.
    pub(super) fn load16(ctx: &mut impl CpuContext, dest: Operand16, source: Operand16) {
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
    pub(super) fn add16(ctx: &mut impl CpuContext, arg: Operand16) {
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
    pub(super) fn alu_op(ctx: &mut impl CpuContext, operand: Operand8, op: AluOp) {
        let arg = operand.read(ctx);
        op.exec(ctx, arg);
    }

    /// Performs a conditional call.
    pub(super) fn call(ctx: &mut impl CpuContext, cond: ConditionCode) {
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
    pub(super) fn jump(ctx: &mut impl CpuContext, cond: ConditionCode) {
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
    pub(super) fn ret(ctx: &mut impl CpuContext, cond: ConditionCode) {
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
    pub(super) fn push(ctx: &mut impl CpuContext, operand: Operand16) {
        // In practice, operand is always a register.
        let val = operand.read(ctx);
        // Push has an extra delay before writing.
        ctx.yield1m();
        push_helper(ctx, val);
    }

    /// Implements pop instruction.
    pub(super) fn pop(ctx: &mut impl CpuContext, operand: Operand16) {
        let val = pop_helper(ctx);
        // In practice, operand is always a register.
        operand.write(ctx, val)
    }

    /// Push helper, shared between push and call. Pushes a caller-supplied 16 bit value onto the stack,
    /// waiting 1m between each byte and decrementing the stack pointer by 2.
    pub(super) fn push_helper(ctx: &mut impl CpuContext, val: u16) {
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
    pub(super) fn pop_helper(ctx: &mut impl CpuContext) -> u16 {
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
    pub(super) fn disable_interrupts(ctx: &mut impl CpuContext) {
        ctx.cpu_mut().interrupt_master_enable.clear();
    }

    /// EI instruction (applies after the following instruction).
    pub(super) fn enable_interrupts(ctx: &mut impl CpuContext) {
        ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
    }

    /// Enabled interrupts and returns.
    pub(super) fn interrupt_return(ctx: &mut impl CpuContext) {
        let dest = pop_helper(ctx);
        // Theres an extra 1m of delay in here.
        ctx.yield1m();
        ctx.cpu_mut().regs.pc = dest;
        ctx.cpu_mut().interrupt_master_enable.set();
    }

    /// Offsets the stack pointer by an immediate value.
    pub(super) fn offset_sp(ctx: &mut impl CpuContext) {
        let offset = Operand8::Immediate.read(ctx) as i8;
        let (res, flags) = offset_addr(ctx.cpu().regs.sp, offset);
        // This instruction takes two more cycles after loading the offset.
        ctx.yield1m();
        ctx.yield1m();
        ctx.cpu_mut().regs.sp = res;
        ctx.cpu_mut().regs.flags = flags;
    }

    /// Loads the result of offsetting the stack pointer by an immediate value into HL.
    pub(super) fn address_of_offset_sp(ctx: &mut impl CpuContext) {
        let offset = Operand8::Immediate.read(ctx) as i8;
        let (res, flags) = offset_addr(ctx.cpu().regs.sp, offset);
        // Interestingly, this instruction is actually faster than `ADD SP,i8`.
        ctx.yield1m();
        ctx.cpu_mut().regs.set_hl(res);
        ctx.cpu_mut().regs.flags = flags;
    }

    // Similar to unconditional jump, but using HL as the target address.
    pub(super) fn jump_hl(ctx: &mut impl CpuContext) {
        let regs = &mut ctx.cpu_mut().regs;
        regs.pc = regs.hl();
    }

    /// Executes the reset instruction. Similar to call with a fixed destination.
    pub(super) fn reset(ctx: &mut impl CpuContext, dest: u8) {
        // There's an extra delay at the start of an RST instruction.
        ctx.yield1m();
        push_helper(ctx, ctx.cpu().regs.pc);
        ctx.cpu_mut().regs.pc = dest as u16;
    }
}

/// Implementations of Opcodes in microcode.
mod ucode_ops {
    use crate::gbz80core::microcode::{
        BinaryOp, GbStack16, Immediate16, Immediate8, Microcode, MicrocodeBuilder, Reg16,
    };
    use crate::gbz80core::{ConditionCode, Flags, Operand16, Operand8};

    /// Build microcode for the relative jump instruction.
    pub(super) fn jump_relative(cond: ConditionCode) -> MicrocodeBuilder {
        // stack: ...|
        // Read the jump offset from the next immediate.
        // Loading the offset also moves the program counter over the next instruction, which
        // is good because the jump is relative to the following instruction.
        // stack: ...|i8  |
        MicrocodeBuilder::read(Immediate8)
            // Read the PC address of the instruction after this one onto the stack.
            // stack: ...|i8  |pcl |pch |
            .then_read(Reg16::Pc)
            // Apply the PC offset.
            // Read the PC address of the instruction after this one onto the stack.
            // stack: ...|dstl|dsth|flag|
            .then(Microcode::OffsetAddr)
            // Discard the flags since JR doesn't use them.
            // stack: ...|dstl|dsth|
            .then(Microcode::Discard(1))
            // Apply the jump only if the condition matches.
            // First yield, because there's an extra dely in JR instructions when branching,
            // then pop the destination off the stack and into the PC register.
            .then(cond.if_true(MicrocodeBuilder::first(Microcode::Yield).then_write(Reg16::Pc)))
    }

    /// Provides microcode for an 8 bit increment instruction.
    pub(super) fn inc8(operand: Operand8) -> MicrocodeBuilder {
        // Inc doesn't set the carry flag.
        const MASK: Flags = Flags::all().difference(Flags::CARRY);

        // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
        // binary ops operate with the top of the stack being the LHS.
        // stack: ...|1|
        MicrocodeBuilder::first(Microcode::Append(1))
            // Then fetch the operand.
            // stack: ...|1|v|
            .then_read(operand)
            // Apply the operation
            // stack: ...|r|f|
            .then(BinaryOp::Add)
            // Write out the flags which are modified
            // stack: ...|r|
            .then_write(MASK)
            // Write the result back to the same operand.
            // stack: ...|
            .then_write(operand)
    }

    /// Provides microcode for an 8 bit decrement instruction.
    pub(super) fn dec8(operand: Operand8) -> MicrocodeBuilder {
        // Dec doesn't set the carry flag.
        const MASK: Flags = Flags::all().difference(Flags::CARRY);

        // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
        // binary ops operate with the top of the stack being the LHS.
        // stack: ...|1|
        MicrocodeBuilder::first(Microcode::Append(1))
            // Then fetch the operand.
            // stack: ...|1|v|
            .then_read(operand)
            // Apply the operation
            // stack: ...|r|f|
            .then(BinaryOp::Sub)
            // Write out the flags which are modified
            // stack: ...|r|
            .then_write(MASK)
            // Write the result back to the same operand.
            // stack: ...|
            .then_write(operand)
    }

    /// Provides microcode for a 16 bit increment instruction.
    pub(super) fn inc16(operand: Operand16) -> MicrocodeBuilder {
        MicrocodeBuilder::read(operand)
            .then(Microcode::Inc16)
            // 16 bit inc doesn't set any flags, and all actual operands are always registers,
            // but it does delay by 1 additional M cycle, probably because it has to operate
            // on two bytes.
            .then(Microcode::Yield)
            .then_write(operand)
    }

    /// Provides microcode for a 16 bit decrement instruction.
    pub(super) fn dec16(operand: Operand16) -> MicrocodeBuilder {
        MicrocodeBuilder::read(operand)
            .then(Microcode::Dec16)
            // 16 bit dec doesn't set any flags, and all actual operands are always registers,
            // but it does delay by 1 additional M cycle, probably because it has to operate
            // on two bytes.
            .then(Microcode::Yield)
            .then_write(operand)
    }

    /// Provides microcode for 16 bit load operations.
    pub(super) fn load16(dest: Operand16, source: Operand16) -> MicrocodeBuilder {
        MicrocodeBuilder::read(source)
            .then(if (dest, source) == (Operand16::Sp, Operand16::HL) {
                // Most of the 16 bit loads are <Pair>,<Immediate> and take time based on
                // number of memory accesses. There are two exceptions. LD (u16),SP, which is
                // also just timed based on the number of memory accesses, and LD SP,HL, which
                // is all registers but still takes an extra 1m cycle, which isn't
                // automatically provided by Operand16 register interactions, so we insert it
                // here.
                Some(Microcode::Yield)
            } else {
                None
            })
            .then_write(dest)
    }

    /// Generates microcode for a 16 bit register add into HL. Never sets the zero flag and
    /// clears the subtract flag, but does set carry and half-carry based on the upper byte of
    /// the operation (as if it was performed by running the pseudo-instructions `add
    /// l,<arg-low>; adc h,<arg-high>`.
    pub(super) fn add16(arg: Operand16) -> MicrocodeBuilder {
        // 16 bit add never modifies the zero flag.
        const MASK: Flags = Flags::all().difference(Flags::ZERO);

        // RHS goes on the stack first.
        MicrocodeBuilder::read(arg)
            // Then LHS goes on the stack.
            .then_read(Reg16::HL)
            // This produces the unmasked flags on top of the stack with the result
            // underneath.
            .then(Microcode::Add16)
            // Add an extra delay cycle, since this op is an 8t not 4t, probably because it is
            // operating on two bytes, and that extra delay doesn't come from memory accesses.
            .then_yield()
            .then_write(MASK)
            .then_write(Reg16::HL)
    }

    /// Build the microcode for a conditional or unconditional call.
    pub(super) fn call(cond: ConditionCode) -> MicrocodeBuilder {
        // Conveniently, unconditional call behaves exactly the same as a conditional call
        // with a true value, down to the timing.
        // First, load the destination address onto the microcode stack.
        // stack: ...|dstl|dsth|
        MicrocodeBuilder::read(Immediate16)
            // Evaluate conditionally:
            // stack: ...|dstl|dsth|
            .then(
                cond.cond(
                    // If true:
                    // Read the PC onto the microcode stack so we can push it to the gameboy
                    // stack.
                    // stack: ...|dstl|dsth|pcl |pch |
                    MicrocodeBuilder::read(Reg16::Pc)
                        // Conditional jump has an extra internal delay if the condition is
                        // true before pushing the return address to the GB stack.
                        .then_yield()
                        // Write the return address to the Gameboy Stack.
                        // stack: ...|dstl|dsth|
                        .then_write(GbStack16)
                        // Write the destination of the jump to the PC.
                        // stack: ...|
                        .then_write(Reg16::Pc),
                    // If false:
                    // Discard the destination address from the microcode stack.
                    Microcode::Discard(2),
                ),
            )
    }

    /// Builds microcode for a conditional absolute jump.
    pub(super) fn jump(cond: ConditionCode) -> MicrocodeBuilder {
        // Conveniently, unconditional jump behaves exactly the same as a conditional jump
        // with a true value, down to the timing.

        // Fetch the destination of the jump from the immediate.
        // stack: ...|dstl|dsth|
        MicrocodeBuilder::read(Immediate16)
            // Evaluate conditionally:
            .then(
                cond.cond(
                    // If true:
                    // Delay by one extra cycle because branching adds an extra cycle despite not
                    // accessing memory.
                    MicrocodeBuilder::r#yield()
                        // Write the desintation address to the PC.
                        // stack: ...|
                        .then_write(Reg16::Pc),
                    // If false:
                    // Discard the destination address.
                    Microcode::Discard(2),
                ),
            )
    }

    /// Performs a conditional return.
    pub(super) fn ret(cond: ConditionCode) -> MicrocodeBuilder {
        // First do a yield if this is a conditional return. The conditional returns have an
        // extra delay at the beginning which isn't part of the unconditional return.
        MicrocodeBuilder::first(match cond {
            ConditionCode::Unconditional => None,
            _ => Some(Microcode::Yield),
        })
        // Apply the actual return only if the condition is true.
        .then(
            cond.if_true(
                // Pop the return address off the Gameboy stack and onto the microcode stack.
                // This takes two m cycles.
                // stack: ...|retl|reth|
                MicrocodeBuilder::read(GbStack16)
                    // Apply the additional yield that happens after the GB stack pop and
                    // before the return is applied to the program counter.
                    .then_yield()
                    // Pop the return address off the microcode stack and into the pc.
                    .then_write(Reg16::Pc),
            ),
        )
    }

    /// Build microcode to enable interrupts and return.
    pub(super) fn interrupt_return() -> MicrocodeBuilder {
        // Pop the return address off the GB stack and onto the microcode stack.
        MicrocodeBuilder::read(GbStack16)
            // Delay by one additional cycle since there's one extra delay in RETI.
            .then_yield()
            // Write the return address to the PC.
            .then_write(Reg16::Pc)
            // Enable interrupts immediately.
            .then(Microcode::EnableInterrupts { immediate: true })
    }

    /// Get microcode to offset the stack pointer by an immediate value.
    pub(super) fn offset_sp() -> MicrocodeBuilder {
        // stack: ...|off|
        MicrocodeBuilder::read(Immediate8)
            // stack: ...|off|spl|sph|
            .then_read(Reg16::Sp)
            // stack: ...|SPL|SPH|flg|
            .then(Microcode::OffsetAddr)
            // This instruction takes two more cycles after loading the offset.
            .then_yield()
            .then_yield()
            // stack: ...|SPL|SPH|
            .then_write(Flags::all())
            .then_write(Reg16::Sp)
    }

    /// Create microcode to load the result of offsetting the stack pointer by an immediate
    /// value into HL.
    pub(super) fn address_of_offset_sp() -> MicrocodeBuilder {
        // stack: ...|off|
        MicrocodeBuilder::read(Immediate8)
            // stack: ...|off|spl|sph|
            .then_read(Reg16::Sp)
            // stack: ...|SPL|SPH|flg|
            .then(Microcode::OffsetAddr)
            // This instruction takes one more cycle after loading the offset.
            // Interestingly, this instruction is actually faster than `ADD SP,i8`.
            .then_yield()
            // stack: ...|SPL|SPH|
            .then_write(Flags::all())
            .then_write(Reg16::HL)
    }

    /// Builds microcode for the reset instruction. Similar to call with a fixed destination.
    pub(super) fn reset(dest: u8) -> MicrocodeBuilder {
        // There's an extra delay at the start of an RST instruction.
        MicrocodeBuilder::r#yield()
            // Push the PC onto the gameboy stack.
            .then_read(Reg16::Pc)
            .then_write(GbStack16)
            // Set the dest address into the PC.
            // initial stack: ...|
            // Push the low order bytes of the dest address.
            // stack: ...|destl|
            .then(Microcode::Append(dest))
            // Push the high order bytes of the dest address.
            // stack: ...|destl|desth|
            .then(Microcode::Append(dest))
            // Set the PC to the specified address.
            .then_write(Reg16::Pc)
    }
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

    /// Get the [`Instr`] for a particular CB Opcode.
    #[cfg(feature = "microcode")]
    pub fn get_instruction(opcode: u8) -> Instr {
        /// Lookup table for Instrs for particular opcodes.
        static CB_OPCODE_TABLE: Lazy<[InstrDef; 256]> = Lazy::new(|| {
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<InstrDef>; 256] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for i in 0..256 {
                data[i].write(CBOpcode::decode(i as u8).into());
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [InstrDef; 256]>(data) }
        });

        Instr::new(&CB_OPCODE_TABLE[opcode as usize])
    }

    /// Load and execute a single CB-prefixed opcode from the given context.
    #[cfg(not(feature = "microcode"))]
    fn load_and_execute(ctx: &mut impl CpuContext) {
        let pc = ctx.cpu().regs.pc;
        trace!("Loading CB-opcode at {:#6X}", pc);
        let opcode = Operand8::Immediate.read(ctx);
        let opcode = Self::decode(opcode);
        debug!("Executing CB @ {:#6X}: {}", pc, opcode);
        opcode.execute(ctx);
    }

    /// Execute this opcode on the given context.
    #[cfg(not(feature = "microcode"))]
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

impl From<CBOpcode> for InstrDef {
    fn from(value: CBOpcode) -> Self {
        // Builds microcode for the CB Opcodes that takes a value and affects flags.
        fn cb_unary_op(operand: Operand8, operator: UnaryOp) -> MicrocodeBuilder {
            // First, read the operand onto the microcode stack.
            // stack: ...|val|
            MicrocodeBuilder::read(operand)
                // Apply the operator, generating the result and flags.
                // stack: ...|res|flags|
                .then(operator)
                // Apply the flags, overwriting all flags in the register.
                .then_write(Flags::all())
                .then_write(operand)
        }
        let builder = match value.op {
            CBOperation::RotateLeft8 => cb_unary_op(value.operand, UnaryOp::RotateLeft8),
            CBOperation::RotateLeft9 => cb_unary_op(value.operand, UnaryOp::RotateLeft9),
            CBOperation::RotateRight8 => cb_unary_op(value.operand, UnaryOp::RotateRight8),
            CBOperation::RotateRight9 => cb_unary_op(value.operand, UnaryOp::RotateRight9),
            CBOperation::ShiftLeft => cb_unary_op(value.operand, UnaryOp::ShiftLeft),
            CBOperation::ShiftRight => cb_unary_op(value.operand, UnaryOp::ShiftRight),
            CBOperation::ShiftRightSignExt => {
                cb_unary_op(value.operand, UnaryOp::ShiftRightSignExt)
            }
            CBOperation::Swap => cb_unary_op(value.operand, UnaryOp::Swap),
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                MicrocodeBuilder::read(value.operand)
                    .then(BitOp::TestBit(bit))
                    .then_write(MASK)
            }
            CBOperation::SetBit(bit) => MicrocodeBuilder::read(value.operand)
                .then(BitOp::SetBit(bit))
                .then_write(value.operand),
            CBOperation::ResetBit(bit) => MicrocodeBuilder::read(value.operand)
                .then(BitOp::ResetBit(bit))
                .then_write(value.operand),
        };
        builder.build(value.to_string())
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
