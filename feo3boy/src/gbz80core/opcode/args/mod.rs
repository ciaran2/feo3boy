use std::fmt;

mod r#impl;
mod microcode;

/// ALU Operation type.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AluOp {
    Add,
    AddCarry,
    Sub,
    SubCarry,
    And,
    Xor,
    Or,
    Compare,
}

impl AluOp {
    /// Get the ALU operation type for the given opcode. Panics if the code is greater than 7.
    pub(super) fn from_ycode(code: u8) -> Self {
        match code {
            0 => Self::Add,
            1 => Self::AddCarry,
            2 => Self::Sub,
            3 => Self::SubCarry,
            4 => Self::And,
            5 => Self::Xor,
            6 => Self::Or,
            7 => Self::Compare,
            _ => panic!("Unrecognized ALU operation type (y code) {}", code),
        }
    }
}

impl fmt::Display for AluOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Add => f.write_str("ADD"),
            Self::AddCarry => f.write_str("ADC"),
            Self::Sub => f.write_str("SUB"),
            Self::SubCarry => f.write_str("SBC"),
            Self::And => f.write_str("AND"),
            Self::Xor => f.write_str("XOR"),
            Self::Or => f.write_str("OR"),
            Self::Compare => f.write_str("CP"),
        }
    }
}

/// Type of unary ALU operation. All ops here apply only to A and Flags.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AluUnaryOp {
    /// 8-bit left rotate. Bit 7 goes to both the carry and bit 0.
    RotateLeft8,
    /// 9-bit left rotate. Bit 7 goes to carry and carry goes to bit 0.
    RotateLeft9,
    /// 8-bit right rotate. Bit 0 goes to both the carry and bit 7.
    RotateRight8,
    /// 9-bit left rotate. Bit 0 goes to carry and carry goes to bit 7.
    RotateRight9,
    /// Helper for doing binary-coded-decimal. Adjusts the hex didgits to keep both nybbles in range
    /// 0..=9 by adding 0x06 and/or 0x60 to push the digit to the next nybble.
    DecimalAdjust,
    /// Sets the carry flag.
    SetCarryFlag,
    /// Inverts the Accumulator.
    Compliment,
    /// Inverts the carry flag.
    ComplimentCarryFlag,
}

impl AluUnaryOp {
    /// Get the ALU unary operation type for the given opcode. Panics if the code is greater than 7.
    pub(super) fn from_ycode(code: u8) -> Self {
        match code {
            0 => Self::RotateLeft8,
            1 => Self::RotateRight8,
            2 => Self::RotateLeft9,
            3 => Self::RotateRight9,
            4 => Self::DecimalAdjust,
            5 => Self::Compliment,
            6 => Self::SetCarryFlag,
            7 => Self::ComplimentCarryFlag,
            _ => panic!("Unrecognized ALU unary operation type (y code) {}", code),
        }
    }
}

impl fmt::Display for AluUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::RotateLeft8 => f.write_str("RLCA"),
            Self::RotateLeft9 => f.write_str("RLA"),
            Self::RotateRight8 => f.write_str("RRCA"),
            Self::RotateRight9 => f.write_str("RRA"),
            Self::DecimalAdjust => f.write_str("DAA"),
            Self::SetCarryFlag => f.write_str("SCF"),
            Self::Compliment => f.write_str("CPL"),
            Self::ComplimentCarryFlag => f.write_str("CCF"),
        }
    }
}

/// 8 bit operand. Either the source or destination of an 8 bit operation.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operand8 {
    // 8 bit registers.
    /// Normal register B.
    B,
    /// Normal register C.
    C,
    /// Normal register D.
    D,
    /// Normal register E.
    E,
    /// Normal register High.
    H,
    /// Normal register Low.
    L,
    /// Accumulator register.
    A,

    // Indirections of 16 bit register pairs.
    /// Dereference HL.
    AddrHL,
    /// Dereference BC.
    AddrBC,
    /// Dereference DE.
    AddrDE,
    /// Dereference HL then increment HL.
    AddrHLInc,
    /// Dereference HL then decrement HL.
    AddrHLDec,
    // Immediates
    /// Load value from immediate (and advance program counter). Cannot be used to store. Will
    /// panic if used as the destination operand.
    Immediate,
    /// Dereference a 16 bit immediate value.
    AddrImmediate,

    // Offsets from 0xff00.
    /// Dereference 0xff00 + register C.
    AddrRelC,
    /// Dereference 0xff00 + 8 bit immediate.
    AddrRelImmediate,
}

impl Operand8 {
    /// Get the 8 bit operand for the given register code. Panics if the code is greater than 7.
    /// Note that register codes are mostly 8 bit registers but also include `(HL)`.
    pub(super) fn from_regcode(code: u8) -> Self {
        match code {
            0 => Self::B,
            1 => Self::C,
            2 => Self::D,
            3 => Self::E,
            4 => Self::H,
            5 => Self::L,
            6 => Self::AddrHL,
            7 => Self::A,
            _ => panic!("Unrecognized Operand type {}", code),
        }
    }

    /// Get the 8 bit operand from the given `p` code for an indirection.
    pub(super) fn from_indirect(code: u8) -> Self {
        match code {
            0 => Self::AddrBC,
            1 => Self::AddrDE,
            2 => Self::AddrHLInc,
            3 => Self::AddrHLDec,
            _ => panic!("Unrecognized indirection code {}", code),
        }
    }
}

impl fmt::Display for Operand8 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::A => f.write_str("A"),
            Self::B => f.write_str("B"),
            Self::C => f.write_str("C"),
            Self::D => f.write_str("D"),
            Self::E => f.write_str("E"),
            Self::H => f.write_str("H"),
            Self::L => f.write_str("L"),
            Self::AddrHL => f.write_str("(HL)"),
            Self::AddrBC => f.write_str("(BC)"),
            Self::AddrDE => f.write_str("(DE)"),
            Self::AddrHLInc => f.write_str("(HL+)"),
            Self::AddrHLDec => f.write_str("(HL-)"),
            Self::Immediate => f.write_str("u8"),
            Self::AddrImmediate => f.write_str("(u16)"),
            Self::AddrRelC => f.write_str("(FF00+C)"),
            Self::AddrRelImmediate => f.write_str("(FF00+u8)"),
        }
    }
}

/// 16 bit operand.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Operand16 {
    /// Register pair BC.
    BC,
    /// Register pair DE.
    DE,
    /// Register pair HL.
    HL,
    /// Register pair of the accumulator + flags register.
    AF,
    /// Stack pointer.
    Sp,
    /// Load value from immediate (and advance program counter). Cannot be used to store. Will
    /// panic if used as the destination operand.
    Immediate,
    /// Load a 16 bit immediate (and advance program counter), then dereference that address. Note:
    /// since no actual opcode uses `(u16)` as the source for a 16 bit read, this will panic if used
    /// to load.
    AddrImmediate,
}

impl Operand16 {
    /// Get a 16 bit operand from a register pair code, using the table of register pairs that
    /// includes the stack pointer. Panics if the code is greater than 3.
    pub(super) fn from_pair_code_sp(code: u8) -> Operand16 {
        match code {
            0 => Operand16::BC,
            1 => Operand16::DE,
            2 => Operand16::HL,
            3 => Operand16::Sp,
            _ => panic!("Unrecognized register pair code {}", code),
        }
    }

    /// Get a 16 bit operand from a register pair code, using the table of register pairs that
    /// includes the accumulator-flags pair. Panics if the code is greater than 3.
    pub(super) fn from_pair_code_af(code: u8) -> Operand16 {
        match code {
            0 => Operand16::BC,
            1 => Operand16::DE,
            2 => Operand16::HL,
            3 => Operand16::AF,
            _ => panic!("Unrecognized register pair code {}", code),
        }
    }
}

impl fmt::Display for Operand16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand16::BC => f.write_str("BC"),
            Operand16::DE => f.write_str("DE"),
            Operand16::HL => f.write_str("HL"),
            Operand16::AF => f.write_str("AF"),
            Operand16::Sp => f.write_str("SP"),
            Operand16::Immediate => f.write_str("u16"),
            Operand16::AddrImmediate => f.write_str("(u16)"),
        }
    }
}

/// Conditional for conditional jump/conditional ret.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ConditionCode {
    Unconditional,
    NonZero,
    Zero,
    NoCarry,
    Carry,
}

impl ConditionCode {
    /// Get the condition code for the given relative-jump condition code. Relative jump condition
    /// codes are for x = 0, z = 0, y = 3..=7 (the value given should be y). Panics if the given
    /// value is not in range 3..=7.
    pub(super) fn from_relative_cond_code(code: u8) -> Self {
        match code {
            3 => Self::Unconditional,
            4..=7 => Self::from_absolute_cond_code(code - 4),
            _ => panic!("Unrecognized Relative-Jump condtion code {}", code),
        }
    }

    /// Get the condition code for the given jump, return, or call condition code. This never
    /// returns `Unconditional`. The value must be in range 0..=3, otherwise this will panic.
    pub(super) fn from_absolute_cond_code(code: u8) -> Self {
        match code {
            0 => Self::NonZero,
            1 => Self::Zero,
            2 => Self::NoCarry,
            3 => Self::Carry,
            _ => panic!("Unrecognized Absolute-Jump condtion code {}", code),
        }
    }
}

impl fmt::Display for ConditionCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Unconditional => Ok(()),
            Self::NonZero => f.write_str("NZ"),
            Self::Zero => f.write_str("Z"),
            Self::NoCarry => f.write_str("NC"),
            Self::Carry => f.write_str("C"),
        }
    }
}
