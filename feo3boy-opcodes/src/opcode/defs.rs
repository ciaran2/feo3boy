//! Provides the definitions of the opcodes in terms of microcode.

use crate::compiler::instr::builder::{InstrBuilder, MicrocodeReadable, MicrocodeWritable};
use crate::compiler::instr::{InstrDef, InstrId};
use crate::gbz80types::Flags;
use crate::microcode::args::{Reg16, Reg8};
use crate::microcode::combocodes::{
    GbStack16, HandleHalt, HlDec, HlInc, Immediate16, Immediate8, LoadAndExecute, Mem,
    ServiceInterrupt,
};
use crate::microcode::Microcode;
use crate::opcode::args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};
use crate::opcode::{CBOpcode, CBOperation, InternalFetch, Opcode};

impl From<InternalFetch> for InstrDef {
    fn from(_: InternalFetch) -> Self {
        // Check if halted (which may just yield and return).
        InstrBuilder::first(HandleHalt)
            // Service interrupts (which may jump to an interrupt and return).
            .then(ServiceInterrupt)
            // Load and execute the next instruction.
            .then(LoadAndExecute)
            .build(InstrId::InternalFetch)
    }
}

impl From<Opcode> for InstrDef {
    fn from(value: Opcode) -> Self {
        let builder = match value {
            Opcode::Nop => InstrBuilder::new(),
            Opcode::Stop => Microcode::Stop.into(),
            Opcode::JumpRelative(cond) => jump_relative(cond),
            Opcode::Inc8(operand) => inc8(operand),
            Opcode::Dec8(operand) => dec8(operand),
            Opcode::Load8 { dest, source } => InstrBuilder::read(source).then_write(dest),
            Opcode::Inc16(operand) => inc16(operand),
            Opcode::Dec16(operand) => dec16(operand),
            Opcode::Load16 { dest, source } => load16(dest, source),
            Opcode::Add16(operand) => add16(operand),
            Opcode::Halt => Microcode::Halt.into(),
            Opcode::AluOp { operand, op } => InstrBuilder::read(operand).then(op),
            Opcode::AluUnary(op) => op.into(),
            Opcode::Call(cond) => call(cond),
            Opcode::Jump(cond) => jump(cond),
            Opcode::Ret(cond) => ret(cond),
            Opcode::Push(operand) => InstrBuilder::read(operand)
                // Push has an extra delay before writing.
                .then_yield()
                .then_write(GbStack16),
            Opcode::Pop(operand) => InstrBuilder::read(GbStack16).then_write(operand),
            Opcode::PrefixCB => InstrBuilder::read(Immediate8).then(Microcode::ParseCBOpcode),
            Opcode::DisableInterrupts => Microcode::DisableInterrupts.into(),
            Opcode::EnableInterrupts => Microcode::EnableInterrupts { immediate: false }.into(),
            Opcode::RetInterrupt => interrupt_return(),
            Opcode::OffsetSp => offset_sp(),
            Opcode::AddressOfOffsetSp => address_of_offset_sp(),
            Opcode::JumpHL => InstrBuilder::read(Reg16::HL).then_write(Reg16::Pc),
            Opcode::Reset(dest) => reset(dest),
            // A brief note on this doc page:
            // https://gbdev.io/pandocs/CPU_Comparison_with_Z80.html
            // says that the unused opcodes will lock up the CPU, rather than behave as a
            // no-op.
            // However, we just build them as an empty instruction, which is a no-op.
            Opcode::MissingInstruction(_) => InstrBuilder::new(),
        };
        builder.build(InstrId::Opcode(value))
    }
}

/// Build microcode for the relative jump instruction.
fn jump_relative(cond: ConditionCode) -> InstrBuilder {
    // stack: ...|
    // Read the jump offset from the next immediate.
    // Loading the offset also moves the program counter over the next instruction, which
    // is good because the jump is relative to the following instruction.
    // stack: ...|i8  |
    InstrBuilder::read(Immediate8)
        // Read the PC address of the instruction after this one onto the stack.
        // stack: ...|i8  |pcl |pch |
        .then_read(Reg16::Pc)
        // Apply the PC offset.
        // Read the PC address of the instruction after this one onto the stack.
        // stack: ...|dstl|dsth|flag|
        .then(Microcode::OffsetAddr)
        // Discard the flags since JR doesn't use them.
        // stack: ...|dstl|dsth|
        .then(Microcode::Discard8)
        // Apply the jump only if the condition matches.
        // First yield, because there's an extra dely in JR instructions when branching,
        // then pop the destination off the stack and into the PC register.
        .then(cond.cond(
            // If the condition matches, apply it to the PC.
            InstrBuilder::r#yield().then_write(Reg16::Pc),
            // If it doesn't match, discard the computed new PC value.
            Microcode::Discard16,
        ))
}

/// Provides microcode for an 8 bit increment instruction.
fn inc8(operand: Operand8) -> InstrBuilder {
    // Inc doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
    // binary ops operate with the top of the stack being the LHS.
    // stack: ...|1|
    InstrBuilder::first(Microcode::Append { val: 1 })
        // Then fetch the operand.
        // stack: ...|1|v|
        .then_read(operand)
        // Apply the operation
        // stack: ...|r|f|
        .then(Microcode::Add)
        // Write out the flags which are modified
        // stack: ...|r|
        .then_write(MASK)
        // Write the result back to the same operand.
        // stack: ...|
        .then_write(operand)
}

/// Provides microcode for an 8 bit decrement instruction.
fn dec8(operand: Operand8) -> InstrBuilder {
    // Dec doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
    // binary ops operate with the top of the stack being the LHS.
    // stack: ...|1|
    InstrBuilder::first(Microcode::Append { val: 1 })
        // Then fetch the operand.
        // stack: ...|1|v|
        .then_read(operand)
        // Apply the operation
        // stack: ...|r|f|
        .then(Microcode::Sub)
        // Write out the flags which are modified
        // stack: ...|r|
        .then_write(MASK)
        // Write the result back to the same operand.
        // stack: ...|
        .then_write(operand)
}

/// Provides microcode for a 16 bit increment instruction.
fn inc16(operand: Operand16) -> InstrBuilder {
    InstrBuilder::read(operand)
        .then(Microcode::Inc16)
        // 16 bit inc doesn't set any flags, and all actual operands are always registers,
        // but it does delay by 1 additional M cycle, probably because it has to operate
        // on two bytes.
        .then(Microcode::Yield)
        .then_write(operand)
}

/// Provides microcode for a 16 bit decrement instruction.
fn dec16(operand: Operand16) -> InstrBuilder {
    InstrBuilder::read(operand)
        .then(Microcode::Dec16)
        // 16 bit dec doesn't set any flags, and all actual operands are always registers,
        // but it does delay by 1 additional M cycle, probably because it has to operate
        // on two bytes.
        .then(Microcode::Yield)
        .then_write(operand)
}

/// Provides microcode for 16 bit load operations.
fn load16(dest: Operand16, source: Operand16) -> InstrBuilder {
    InstrBuilder::read(source)
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
fn add16(arg: Operand16) -> InstrBuilder {
    // 16 bit add never modifies the zero flag.
    const MASK: Flags = Flags::all().difference(Flags::ZERO);

    // RHS goes on the stack first.
    InstrBuilder::read(arg)
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
fn call(cond: ConditionCode) -> InstrBuilder {
    // Conveniently, unconditional call behaves exactly the same as a conditional call
    // with a true value, down to the timing.
    // First, load the destination address onto the microcode stack.
    // stack: ...|dstl|dsth|
    InstrBuilder::read(Immediate16)
        // Evaluate conditionally:
        // stack: ...|dstl|dsth|
        .then(
            cond.cond(
                // If true:
                // Read the PC onto the microcode stack so we can push it to the gameboy
                // stack.
                // stack: ...|dstl|dsth|pcl |pch |
                InstrBuilder::read(Reg16::Pc)
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
                Microcode::Discard16,
            ),
        )
}

/// Builds microcode for a conditional absolute jump.
fn jump(cond: ConditionCode) -> InstrBuilder {
    // Conveniently, unconditional jump behaves exactly the same as a conditional jump
    // with a true value, down to the timing.

    // Fetch the destination of the jump from the immediate.
    // stack: ...|dstl|dsth|
    InstrBuilder::read(Immediate16)
        // Evaluate conditionally:
        .then(
            cond.cond(
                // If true:
                // Delay by one extra cycle because branching adds an extra cycle despite not
                // accessing memory.
                InstrBuilder::r#yield()
                    // Write the desintation address to the PC.
                    // stack: ...|
                    .then_write(Reg16::Pc),
                // If false:
                // Discard the destination address.
                Microcode::Discard16,
            ),
        )
}

/// Performs a conditional return.
fn ret(cond: ConditionCode) -> InstrBuilder {
    // First do a yield if this is a conditional return. The conditional returns have an
    // extra delay at the beginning which isn't part of the unconditional return.
    InstrBuilder::first(match cond {
        ConditionCode::Unconditional => None,
        _ => Some(Microcode::Yield),
    })
    // Apply the actual return only if the condition is true.
    .then(
        cond.if_true(
            // Pop the return address off the Gameboy stack and onto the microcode stack.
            // This takes two m cycles.
            // stack: ...|retl|reth|
            InstrBuilder::read(GbStack16)
                // Apply the additional yield that happens after the GB stack pop and
                // before the return is applied to the program counter.
                .then_yield()
                // Pop the return address off the microcode stack and into the pc.
                .then_write(Reg16::Pc),
        ),
    )
}

/// Build microcode to enable interrupts and return.
fn interrupt_return() -> InstrBuilder {
    // Pop the return address off the GB stack and onto the microcode stack.
    InstrBuilder::read(GbStack16)
        // Delay by one additional cycle since there's one extra delay in RETI.
        .then_yield()
        // Write the return address to the PC.
        .then_write(Reg16::Pc)
        // Enable interrupts immediately.
        .then(Microcode::EnableInterrupts { immediate: true })
}

/// Get microcode to offset the stack pointer by an immediate value.
fn offset_sp() -> InstrBuilder {
    // stack: ...|off|
    InstrBuilder::read(Immediate8)
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
fn address_of_offset_sp() -> InstrBuilder {
    // stack: ...|off|
    InstrBuilder::read(Immediate8)
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
fn reset(dest: u8) -> InstrBuilder {
    // There's an extra delay at the start of an RST instruction.
    InstrBuilder::r#yield()
        // Push the PC onto the gameboy stack.
        .then_read(Reg16::Pc)
        .then_write(GbStack16)
        // Push the reset destination low byte onto the stack.
        // stack: ...|addrl|
        .then(Microcode::Append { val: dest })
        // Push the high order bytes of the dest address (always 0).
        // stack: ...|addrl|addrh|
        .then(Microcode::Append { val: 0 })
        // Set the PC to the specified address.
        .then_write(Reg16::Pc)
}

impl From<CBOpcode> for InstrDef {
    fn from(value: CBOpcode) -> Self {
        // Builds microcode for the CB Opcodes that takes a value and affects flags.
        fn cb_unary_op(operand: Operand8, operator: Microcode) -> InstrBuilder {
            // First, read the operand onto the microcode stack.
            // stack: ...|val|
            InstrBuilder::read(operand)
                // Rotate left/right 9 require the current flags value in addition to the
                // operand.
                .then(
                    if matches!(operator, Microcode::RotateLeft9 | Microcode::RotateRight9) {
                        Some(Microcode::GetFlagsMasked { mask: Flags::all() })
                    } else {
                        None
                    },
                )
                // Apply the operator, generating the result and flags.
                // stack: ...|res|flags|
                .then(operator)
                // Apply the flags, overwriting all flags in the register.
                .then_write(Flags::all())
                .then_write(operand)
        }
        let builder = match value.op {
            CBOperation::RotateLeft8 => cb_unary_op(value.operand, Microcode::RotateLeft8),
            CBOperation::RotateLeft9 => cb_unary_op(value.operand, Microcode::RotateLeft9),
            CBOperation::RotateRight8 => cb_unary_op(value.operand, Microcode::RotateRight8),
            CBOperation::RotateRight9 => cb_unary_op(value.operand, Microcode::RotateRight9),
            CBOperation::ShiftLeft => cb_unary_op(value.operand, Microcode::ShiftLeft),
            CBOperation::ShiftRight => cb_unary_op(value.operand, Microcode::ShiftRight),
            CBOperation::ShiftRightSignExt => {
                cb_unary_op(value.operand, Microcode::ShiftRightSignExt)
            }
            CBOperation::Swap => cb_unary_op(value.operand, Microcode::Swap),
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                InstrBuilder::read(value.operand)
                    .then(Microcode::TestBit { bit })
                    .then_write(MASK)
            }
            CBOperation::SetBit(bit) => InstrBuilder::read(value.operand)
                .then(Microcode::SetBit { bit })
                .then_write(value.operand),
            CBOperation::ResetBit(bit) => InstrBuilder::read(value.operand)
                .then(Microcode::ResetBit { bit })
                .then_write(value.operand),
        };
        builder.build(InstrId::CBOpcode(value))
    }
}

/// Builds the microcode for a single ALU operation. This assumes that the ALU operation's
/// single u8 arg is already on the stack and handles fetching the accumulator and flags
/// (if needed) and applying the results.
impl From<AluOp> for InstrBuilder {
    fn from(op: AluOp) -> Self {
        // stack: ...|val|
        // Fetch the accumulator.
        // stack: ...|val|acc|
        InstrBuilder::read(Reg8::Acc)
            // If the operation requires flags, fetch the flags register.
            .then(if matches!(op, AluOp::AddCarry | AluOp::SubCarry) {
                Some(Microcode::GetFlagsMasked { mask: Flags::all() })
            } else {
                None
            })
            // Apply the appropriate BinaryOp.
            // stack: ...|res|flg|
            .then(match op {
                AluOp::Add => Microcode::Add,
                AluOp::AddCarry => Microcode::Adc,
                AluOp::Sub => Microcode::Sub,
                AluOp::SubCarry => Microcode::Sbc,
                AluOp::And => Microcode::And,
                AluOp::Or => Microcode::Or,
                AluOp::Xor => Microcode::Xor,
                AluOp::Compare => Microcode::Sub,
            })
            // Write the resulting flags (overwriting all flags)
            // stack: ...|res|
            .then_write(Flags::all())
            // Either put the result into the Acc or discard it (in case of cmp).
            // stack: ...|
            .then(match op {
                AluOp::Compare => Microcode::Discard8,
                _ => Microcode::WriteReg { reg: Reg8::Acc },
            })
    }
}

/// Builds the microcode for a single ALU unary operation. This handles reading the
/// accumulator and flags as needed, and handles applying the results.
impl From<AluUnaryOp> for InstrBuilder {
    fn from(value: AluUnaryOp) -> Self {
        match value {
            AluUnaryOp::RotateLeft8 => InstrBuilder::read(Reg8::Acc)
                .then(Microcode::RotateLeft8)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append { val: 0 })
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateLeft9 => InstrBuilder::read(Reg8::Acc)
                .then_read(Flags::all())
                .then(Microcode::RotateLeft9)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append { val: 0 })
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateRight8 => InstrBuilder::read(Reg8::Acc)
                .then(Microcode::RotateRight8)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append { val: 0 })
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateRight9 => InstrBuilder::read(Reg8::Acc)
                .then_read(Flags::all())
                .then(Microcode::RotateRight9)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append { val: 0 })
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::DecimalAdjust => {
                // Does not modify SUB.
                const MASK: Flags = Flags::all().difference(Flags::SUB);
                InstrBuilder::read(Reg8::Acc)
                    .then_read(Flags::all())
                    .then(Microcode::DecimalAdjust)
                    .then_write(MASK)
                    .then_write(Reg8::Acc)
            }
            AluUnaryOp::Compliment => {
                // Always sets only SUB and HALFCARRY to 1.
                const MASK: Flags = Flags::SUB.union(Flags::HALFCARRY);
                InstrBuilder::read(Reg8::Acc)
                    .then(Microcode::Compliment)
                    .then_write(MASK)
                    .then_write(Reg8::Acc)
            }
            AluUnaryOp::SetCarryFlag => {
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                InstrBuilder::first(Microcode::Append {
                    val: Flags::CARRY.bits(),
                })
                .then_write(MASK)
            }
            AluUnaryOp::ComplimentCarryFlag => {
                // stack: ...|
                // Fetch the carry flag from the flags register.
                // stack: ...|000C|
                InstrBuilder::read(Flags::CARRY)
                    // Compliment the carry flags.
                    // stack: ...|111c|flgs|
                    .then(Microcode::Compliment)
                    // Discard the flags from the complement operation.
                    // stack: ...|111c|
                    .then(Microcode::Discard8)
                    // Overwrite the carry flag:
                    // stack: ...|
                    .then_write(Flags::CARRY)
                    // Add a 0 to use to clear the SUB and HALFCARRY flags, as CCF always
                    // sets those to 0.
                    // stack: ...|0000|
                    .then(Microcode::Append { val: 0 })
                    // Overwrite the SUB and HALFCARRY flags.
                    // stack: ...|
                    .then_write(Flags::SUB.union(Flags::HALFCARRY))
            }
        }
    }
}

/// As a MicrocodeReadable, `Operand8` will result in a u8 being pushed onto the microcode
/// stack from the appropriate source.
impl MicrocodeReadable for Operand8 {
    fn to_read(self) -> InstrBuilder {
        match self {
            Self::A => InstrBuilder::read(Reg8::Acc),
            Self::B => InstrBuilder::read(Reg8::B),
            Self::C => InstrBuilder::read(Reg8::C),
            Self::D => InstrBuilder::read(Reg8::D),
            Self::E => InstrBuilder::read(Reg8::E),
            Self::H => InstrBuilder::read(Reg8::H),
            Self::L => InstrBuilder::read(Reg8::L),
            Self::AddrHL => InstrBuilder::read(Reg16::HL).then_read(Mem),
            Self::AddrBC => InstrBuilder::read(Reg16::BC).then_read(Mem),
            Self::AddrDE => InstrBuilder::read(Reg16::DE).then_read(Mem),
            Self::AddrHLInc => {
                // stack: ...|
                // Retrieve HL and increment it.
                // stack: ...|h|l|
                InstrBuilder::read(HlInc)
                    // Use hl to read the value from memory.
                    // stack: ...|v|
                    .then_read(Mem)
            }
            Self::AddrHLDec => {
                // stack: ...|
                // Retrieve HL and decrement it.
                // stack: ...|h|l|
                InstrBuilder::read(HlDec)
                    // Use the original hl value to read the value from memory.
                    // stack: ...|v|
                    .then_read(Mem)
            }
            Self::Immediate => InstrBuilder::read(Immediate8),
            Self::AddrImmediate => InstrBuilder::read(Immediate16).then_read(Mem),
            Self::AddrRelC => InstrBuilder::read(Reg8::C)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // read from, so after reading C as the low byte of the address, we just
                // push the 0xff as the high byte.
                .then(Microcode::Append { val: 0xff })
                .then_read(Mem),
            Self::AddrRelImmediate => InstrBuilder::read(Immediate8)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // read from, so after reading an immediate as the low byte of the
                // address, we just push the 0xff as the high byte.
                .then(Microcode::Append { val: 0xff })
                .then_read(Mem),
        }
    }
}

/// As a MicrocodeWritable, `Operand8` will result in a u8 popped off of the stack and
/// written to the appropriate destination.
impl MicrocodeWritable for Operand8 {
    fn to_write(self) -> InstrBuilder {
        match self {
            Self::A => InstrBuilder::write(Reg8::Acc),
            Self::B => InstrBuilder::write(Reg8::B),
            Self::C => InstrBuilder::write(Reg8::C),
            Self::D => InstrBuilder::write(Reg8::D),
            Self::E => InstrBuilder::write(Reg8::E),
            Self::H => InstrBuilder::write(Reg8::H),
            Self::L => InstrBuilder::write(Reg8::L),
            Self::AddrHL => InstrBuilder::read(Reg16::HL).then_write(Mem),
            Self::AddrBC => InstrBuilder::read(Reg16::BC).then_write(Mem),
            Self::AddrDE => InstrBuilder::read(Reg16::DE).then_write(Mem),
            Self::AddrHLInc => {
                // stack: ...|v|
                // Retrieve HL and increment it.
                // stack: ...|v|h|l|
                InstrBuilder::read(HlInc)
                    // Use hl to write the value to memory.
                    // stack: ...|
                    .then_write(Mem)
            }
            Self::AddrHLDec => {
                // stack: ...|v|
                // Retrieve HL and decrement it.
                // stack: ...|v|h|l|
                InstrBuilder::read(HlDec)
                    // Use hl to write the value to memory.
                    // stack: ...|
                    .then_write(Mem)
            }
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            Self::AddrImmediate => InstrBuilder::read(Immediate16).then_write(Mem),
            Self::AddrRelC => InstrBuilder::read(Reg8::C)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // write to, so after reading C as the low byte of the address, we just
                // push the 0xff as the high byte.
                .then(Microcode::Append { val: 0xff })
                .then_write(Mem),
            Self::AddrRelImmediate => InstrBuilder::read(Immediate8)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // write to, so after reading an immediate as the low byte of the address,
                // we just push the 0xff as the high byte.
                .then(Microcode::Append { val: 0xff })
                .then_write(Mem),
        }
    }
}

/// As a MicrocodeReadable, `Operand16` will result in a u16 being pushed onto the
/// microcode stack from the appropriate source.
impl MicrocodeReadable for Operand16 {
    fn to_read(self) -> InstrBuilder {
        match self {
            Self::BC => InstrBuilder::read(Reg16::BC),
            Self::DE => InstrBuilder::read(Reg16::DE),
            Self::HL => InstrBuilder::read(Reg16::HL),
            Self::AF => InstrBuilder::read(Reg16::AF),
            Self::Sp => InstrBuilder::read(Reg16::Sp),
            Self::Immediate => InstrBuilder::read(Immediate16),
            Self::AddrImmediate => {
                panic!("No actual operation uses (u16) as the source for a 16 bit load")
            }
        }
    }
}

/// As a MicrocodeWritable, `Operand16` will result in a u16 popped off of the stack and
/// written to the appropriate destination.
impl MicrocodeWritable for Operand16 {
    fn to_write(self) -> InstrBuilder {
        match self {
            Self::BC => InstrBuilder::write(Reg16::BC),
            Self::DE => InstrBuilder::write(Reg16::DE),
            Self::HL => InstrBuilder::write(Reg16::HL),
            Self::AF => InstrBuilder::write(Reg16::AF),
            Self::Sp => InstrBuilder::write(Reg16::Sp),
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            // First get the address to write to.
            // stack: ...|vall|valh|destl|desth|
            Self::AddrImmediate => InstrBuilder::read(Immediate16)
                // Intersperse inverts the order of the value bytes and puts copies of the
                // address between them, which sets us up perfectly for the needed writes.
                // stack: ...|valh|destl|desth|vall|destl|desth|
                .then(Microcode::Intersperse)
                // Write the low byte to memory at the specified address.
                // stack: ...|valh|destl|desth|
                .then_write(Mem)
                // Increment the destination address to get the address for the second
                // byte.
                // stack: ...|valh|DESTL|DESTH|
                .then(Microcode::Inc16)
                // Write the high byte to memory at the specified address.
                // stack: ...|
                .then_write(Mem),
        }
    }
}

impl ConditionCode {
    /// Wrap the provided microcode in a conditional handling. If unconditional, just
    /// returns the `code_if_condition_true`. If conditional, fetches the appropriate flag
    /// and applies a micorcode conditional to run the provided code only when the
    /// condition matches.
    fn cond(
        self,
        code_if_condition_true: impl Into<InstrBuilder>,
        code_if_condition_false: impl Into<InstrBuilder>,
    ) -> InstrBuilder {
        match self {
            ConditionCode::Unconditional => code_if_condition_true.into(),
            // To implement the inverted flags (NonZero and NoCarry), we execute the
            // "code_if_condition_true" in the "false" case of the InstrBuilder::cond.
            ConditionCode::NonZero => InstrBuilder::read(Flags::ZERO).then(InstrBuilder::cond(
                code_if_condition_false,
                code_if_condition_true,
            )),
            ConditionCode::Zero => InstrBuilder::read(Flags::ZERO).then(InstrBuilder::cond(
                code_if_condition_true,
                code_if_condition_false,
            )),
            ConditionCode::NoCarry => InstrBuilder::read(Flags::CARRY).then(InstrBuilder::cond(
                code_if_condition_false,
                code_if_condition_true,
            )),
            ConditionCode::Carry => InstrBuilder::read(Flags::CARRY).then(InstrBuilder::cond(
                code_if_condition_true,
                code_if_condition_false,
            )),
        }
    }

    /// Wrap the given code to run only if this conditional evaluates to true. If
    /// unconditional, returns the microcode unchanged, otherwise runs the specified code
    /// only if the appropriate flag matches.
    fn if_true(self, code_if_condition_true: impl Into<InstrBuilder>) -> InstrBuilder {
        self.cond(code_if_condition_true, InstrBuilder::new())
    }
}
