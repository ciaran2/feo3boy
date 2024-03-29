//! Provides the [`Opcode`] and [`CBOpcode`] types, which define the opcodes available on
//! the gbz80 CPU.

use std::fmt;

use crate::opcode::args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};

pub mod args;
pub mod defs;

/// Represents the internal-fetch opcode which is used to begin every instruction cycle.
pub struct InternalFetch;

impl fmt::Display for InternalFetch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[Internal Fetch]")
    }
}

// Opcode References:
// - Decoding: www.z80.info/decoding.htm
// - GB Z80 Opcode Table: https://izik1.github.io/gbops/
// - Regular Z80 Opcode Table: http://z80-heaven.wikidot.com/opcode-reference-chart
// - Regular Z80 Flag Reference: http://www.z80.info/z80sflag.htm
// - GB Z80 Instruction Reference: https://rgbds.gbdev.io/docs/v0.4.1/gbz80.7

/// Parsed Opcode, not including any arguments that may be loaded from immediates, nor any follow-up
/// ops if the operation is a prefixed op.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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

//////////////////////
// CB Prefixed Opcodes
//////////////////////

/// Opcodes that come after a CB prefix opcode.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
