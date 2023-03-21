//! Contains the microcode implementations of opcode args.

use crate::gbz80core::microcode::{
    BinaryOp, Immediate16, Immediate8, Mem16, Mem8, Microcode, MicrocodeBuilder, MicrocodeReadable,
    MicrocodeWritable, Reg16, Reg8, UnaryOp,
};
use crate::gbz80core::opcode::args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};
use crate::gbz80core::Flags;

/// Builds the microcode for a single ALU operation. This assumes that the ALU operation's
/// single u8 arg is already on the stack and handles fetching the accumulator and
/// applying the results.
impl From<AluOp> for MicrocodeBuilder {
    fn from(op: AluOp) -> Self {
        // stack: ...|val|
        // Fetch the accumulator.
        // stack: ...|val|acc|
        MicrocodeBuilder::read(Reg8::Acc)
            // Apply the appropriate BinaryOp.
            // stack: ...|res|flg|
            .then(match op {
                AluOp::Add => BinaryOp::Add,
                AluOp::AddCarry => BinaryOp::AddCarry,
                AluOp::Sub => BinaryOp::Sub,
                AluOp::SubCarry => BinaryOp::SubCarry,
                AluOp::And => BinaryOp::And,
                AluOp::Or => BinaryOp::Or,
                AluOp::Xor => BinaryOp::Xor,
                AluOp::Compare => BinaryOp::Sub,
            })
            // Write the resulting flags (overwriting all flags)
            // stack: ...|res|
            .then_write(Flags::all())
            // Either put the result into the Acc or discard it (in case of cmp).
            // stack: ...|
            .then(match op {
                AluOp::Compare => Microcode::Discard(1),
                _ => Microcode::WriteReg8(Reg8::Acc),
            })
    }
}

/// Builds the microcode for a single ALU unary operation. This handles reading the
/// accumulator as needed, and handles applying the results.
impl From<AluUnaryOp> for MicrocodeBuilder {
    fn from(value: AluUnaryOp) -> Self {
        match value {
            AluUnaryOp::RotateLeft8 => MicrocodeBuilder::read(Reg8::Acc)
                .then(UnaryOp::RotateLeft8)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append(0))
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateLeft9 => MicrocodeBuilder::read(Reg8::Acc)
                .then(UnaryOp::RotateLeft9)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append(0))
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateRight8 => MicrocodeBuilder::read(Reg8::Acc)
                .then(UnaryOp::RotateRight8)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append(0))
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::RotateRight9 => MicrocodeBuilder::read(Reg8::Acc)
                .then(UnaryOp::RotateRight9)
                .then_write(Flags::all())
                // Unlike CB Opcodes, this Rotate always clears the zero flag, so we need to
                // clear that here. To do that, we load an all-zero byte and apply it masked
                // to just the zero flag.
                .then(Microcode::Append(0))
                .then_write(Flags::ZERO)
                .then_write(Reg8::Acc),
            AluUnaryOp::DecimalAdjust => {
                // Does not modify SUB.
                const MASK: Flags = Flags::all().difference(Flags::SUB);
                MicrocodeBuilder::read(Reg8::Acc)
                    .then(UnaryOp::DecimalAdjust)
                    .then_write(MASK)
                    .then_write(Reg8::Acc)
            }
            AluUnaryOp::Compliment => {
                // Always sets only SUB and HALFCARRY to 1.
                const MASK: Flags = Flags::SUB.union(Flags::HALFCARRY);
                MicrocodeBuilder::read(Reg8::Acc)
                    .then(UnaryOp::Compliment)
                    .then_write(MASK)
                    .then_write(Reg8::Acc)
            }
            AluUnaryOp::SetCarryFlag => {
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                MicrocodeBuilder::first(Microcode::Append(Flags::CARRY.bits())).then_write(MASK)
            }
            AluUnaryOp::ComplimentCarryFlag => {
                // stack: ...|
                // Fetch the carry flag from the flags register.
                // stack: ...|000C|
                MicrocodeBuilder::read(Flags::CARRY)
                    // Compliment the carry flags.
                    // stack: ...|111c|flgs|
                    .then(UnaryOp::Compliment)
                    // Discard the flags from the complement operation.
                    // stack: ...|111c|
                    .then(Microcode::Discard(1))
                    // Overwrite the carry flag:
                    // stack: ...|
                    .then_write(Flags::CARRY)
                    // Add a 0 to use to clear the SUB and HALFCARRY flags, as CCF always
                    // sets those to 0.
                    // stack: ...|0000|
                    .then(Microcode::Append(0))
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
    fn to_read(self) -> MicrocodeBuilder {
        match self {
            Self::A => MicrocodeBuilder::read(Reg8::Acc),
            Self::B => MicrocodeBuilder::read(Reg8::B),
            Self::C => MicrocodeBuilder::read(Reg8::C),
            Self::D => MicrocodeBuilder::read(Reg8::D),
            Self::E => MicrocodeBuilder::read(Reg8::E),
            Self::H => MicrocodeBuilder::read(Reg8::H),
            Self::L => MicrocodeBuilder::read(Reg8::L),
            Self::AddrHL => MicrocodeBuilder::read(Reg16::HL).then_read(Mem8),
            Self::AddrBC => MicrocodeBuilder::read(Reg16::BC).then_read(Mem8),
            Self::AddrDE => MicrocodeBuilder::read(Reg16::DE).then_read(Mem8),
            Self::AddrHLInc => {
                // stack: ...|
                // Grab the current value of HL.
                // stack: ...|l|h|
                MicrocodeBuilder::read(Reg16::HL)
                    // Copy the value so we can increment it.
                    // stack: ...|l|h|l|h|
                    .then(Microcode::Dup(2))
                    // Increment the value
                    // stack: ...|l|h|L|H|
                    .then(Microcode::Inc16)
                    // Write the new value to HL
                    // stack: ...|l|h|
                    .then_write(Reg16::HL)
                    // Use the original hl value to read the value from memory.
                    // stack: ...|v|
                    .then_write(Mem8)
            }
            Self::AddrHLDec => {
                // stack: ...|
                // Grab the current value of HL.
                // stack: ...|L|H|
                MicrocodeBuilder::read(Reg16::HL)
                    // Copy the value so we can increment it.
                    // stack: ...|L|H|L|H|
                    .then(Microcode::Dup(2))
                    // Decrement the value
                    // stack: ...|L|H|l|h|
                    .then(Microcode::Dec16)
                    // Write the new value to HL
                    // stack: ...|L|H|
                    .then_write(Reg16::HL)
                    // Use the original hl value to read the value from memory.
                    // stack: ...|v|
                    .then_write(Mem8)
            }
            Self::Immediate => MicrocodeBuilder::read(Immediate8),
            Self::AddrImmediate => MicrocodeBuilder::read(Immediate16).then_read(Mem8),
            Self::AddrRelC => MicrocodeBuilder::read(Reg8::C)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // read from, so after reading C as the low byte of the address, we just
                // push the 0xff as the high byte.
                .then(Microcode::Append(0xff))
                .then_read(Mem8),
            Self::AddrRelImmediate => MicrocodeBuilder::read(Immediate8)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // read from, so after reading an immediate as the low byte of the
                // address, we just push the 0xff as the high byte.
                .then(Microcode::Append(0xff))
                .then_read(Mem8),
        }
    }
}

/// As a MicrocodeWritable, `Operand8` will result in a u8 popped off of the stack and
/// written to the appropriate destination.
impl MicrocodeWritable for Operand8 {
    fn to_write(self) -> MicrocodeBuilder {
        match self {
            Self::A => MicrocodeBuilder::write(Reg8::Acc),
            Self::B => MicrocodeBuilder::write(Reg8::B),
            Self::C => MicrocodeBuilder::write(Reg8::C),
            Self::D => MicrocodeBuilder::write(Reg8::D),
            Self::E => MicrocodeBuilder::write(Reg8::E),
            Self::H => MicrocodeBuilder::write(Reg8::H),
            Self::L => MicrocodeBuilder::write(Reg8::L),
            Self::AddrHL => MicrocodeBuilder::read(Reg16::HL).then_write(Mem8),
            Self::AddrBC => MicrocodeBuilder::read(Reg16::BC).then_write(Mem8),
            Self::AddrDE => MicrocodeBuilder::read(Reg16::DE).then_write(Mem8),
            Self::AddrHLInc => {
                // stack: ...|v|
                // Grab the current value of HL.
                // stack: ...|v|l|h|
                MicrocodeBuilder::read(Reg16::HL)
                    // Copy the value so we can increment it.
                    // stack: ...|v|l|h|l|h|
                    .then(Microcode::Dup(2))
                    // Increment the value
                    // stack: ...|v|l|h|L|H|
                    .then(Microcode::Inc16)
                    // Write the new value to HL
                    // stack: ...|v|l|h|
                    .then_write(Reg16::HL)
                    // Use the original hl value to write the value to memory.
                    // stack: ...|
                    .then_write(Mem8)
            }
            Self::AddrHLDec => {
                // stack: ...|v|
                // Grab the current value of HL.
                // stack: ...|v|L|H|
                MicrocodeBuilder::read(Reg16::HL)
                    // Copy the value so we can increment it.
                    // stack: ...|v|L|H|L|H|
                    .then(Microcode::Dup(2))
                    // Decrement the value
                    // stack: ...|v|L|H|l|h|
                    .then(Microcode::Dec16)
                    // Write the new value to HL
                    // stack: ...|v|L|H|
                    .then_write(Reg16::HL)
                    // Use the original hl value to write the value to memory.
                    // stack: ...|
                    .then_write(Mem8)
            }
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            Self::AddrImmediate => MicrocodeBuilder::read(Immediate16).then_write(Mem8),
            Self::AddrRelC => MicrocodeBuilder::read(Reg8::C)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // write to, so after reading C as the low byte of the address, we just
                // push the 0xff as the high byte.
                .then(Microcode::Append(0xff))
                .then_write(Mem8),
            Self::AddrRelImmediate => MicrocodeBuilder::read(Immediate8)
                // We want to build a pair |l|h| on the top of the stack as the address to
                // write to, so after reading an immediate as the low byte of the address,
                // we just push the 0xff as the high byte.
                .then(Microcode::Append(0xff))
                .then_write(Mem8),
        }
    }
}

/// As a MicrocodeReadable, `Operand16` will result in a u16 being pushed onto the
/// microcode stack from the appropriate source.
impl MicrocodeReadable for Operand16 {
    fn to_read(self) -> MicrocodeBuilder {
        match self {
            Self::BC => MicrocodeBuilder::read(Reg16::BC),
            Self::DE => MicrocodeBuilder::read(Reg16::DE),
            Self::HL => MicrocodeBuilder::read(Reg16::HL),
            Self::AF => MicrocodeBuilder::read(Reg16::AF),
            Self::Sp => MicrocodeBuilder::read(Reg16::Sp),
            Self::Immediate => MicrocodeBuilder::read(Immediate16),
            Self::AddrImmediate => {
                panic!("No actual operation uses (u16) as the source for a 16 bit load")
            }
        }
    }
}

/// As a MicrocodeWritable, `Operand16` will result in a u16 popped off of the stack and
/// written to the appropriate destination.
impl MicrocodeWritable for Operand16 {
    fn to_write(self) -> MicrocodeBuilder {
        match self {
            Self::BC => MicrocodeBuilder::write(Reg16::BC),
            Self::DE => MicrocodeBuilder::write(Reg16::DE),
            Self::HL => MicrocodeBuilder::write(Reg16::HL),
            Self::AF => MicrocodeBuilder::write(Reg16::AF),
            Self::Sp => MicrocodeBuilder::write(Reg16::Sp),
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            Self::AddrImmediate => MicrocodeBuilder::read(Immediate16).then_write(Mem16),
        }
    }
}

impl ConditionCode {
    /// Wrap the provided microcode in a conditional handling. If unconditional, just
    /// returns the `code_if_condition_true`. If conditional, fetches the appropriate flag
    /// and applies a micorcode conditional to run the provided code only when the
    /// condition matches.
    pub(in crate::gbz80core::opcode) fn cond(
        self,
        code_if_condition_true: impl Into<MicrocodeBuilder>,
        code_if_condition_false: impl Into<MicrocodeBuilder>,
    ) -> MicrocodeBuilder {
        match self {
            ConditionCode::Unconditional => code_if_condition_true.into(),
            // To implement the inverted flags (NonZero and NoCarry), we execute the
            // "code_if_condition_true" in the "false" case of the MicrocodeBuilder::cond.
            ConditionCode::NonZero => MicrocodeBuilder::read(Flags::ZERO).then(
                MicrocodeBuilder::cond(code_if_condition_false, code_if_condition_true),
            ),
            ConditionCode::Zero => MicrocodeBuilder::read(Flags::ZERO).then(
                MicrocodeBuilder::cond(code_if_condition_true, code_if_condition_false),
            ),
            ConditionCode::NoCarry => MicrocodeBuilder::read(Flags::CARRY).then(
                MicrocodeBuilder::cond(code_if_condition_false, code_if_condition_true),
            ),
            ConditionCode::Carry => MicrocodeBuilder::read(Flags::CARRY).then(
                MicrocodeBuilder::cond(code_if_condition_true, code_if_condition_false),
            ),
        }
    }

    /// Wrap the given code to run only if this conditional evaluates to true. If
    /// unconditional, returns the microcode unchanged, otherwise runs the specified code
    /// only if the appropriate flag matches.
    pub(in crate::gbz80core::opcode) fn if_true(
        self,
        code_if_condition_true: impl Into<MicrocodeBuilder>,
    ) -> MicrocodeBuilder {
        self.cond(code_if_condition_true, MicrocodeBuilder::new())
    }
}
