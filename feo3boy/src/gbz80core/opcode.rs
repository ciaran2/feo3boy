use std::fmt;

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
    /// Halt instruction. Pauses but still accepts interrupts I think?
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
    fn from_ycode(code: u8) -> Self {
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
    fn from_ycode(code: u8) -> Self {
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
    fn from_regcode(code: u8) -> Self {
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
    fn from_indirect(code: u8) -> Self {
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
    /// Load a 16 bit immediate (and advance program counter), then dereference that address.
    AddrImmediate,
}

impl Operand16 {
    /// Get a 16 bit operand from a register pair code, using the table of register pairs that
    /// includes the stack pointer. Panics if the code is greater than 3.
    fn from_pair_code_sp(code: u8) -> Operand16 {
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
    fn from_pair_code_af(code: u8) -> Operand16 {
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
    fn from_relative_cond_code(code: u8) -> Self {
        match code {
            3 => Self::Unconditional,
            4..=7 => Self::from_absolute_cond_code(code - 4),
            _ => panic!("Unrecognized Relative-Jump condtion code {}", code),
        }
    }

    /// Get the condition code for the given jump, return, or call condition code. This never
    /// returns `Unconditional`. The value must be in range 0..=3, otherwise this will panic.
    fn from_absolute_cond_code(code: u8) -> Self {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_decode_any_opcode() {
        for op in 0u8..=0xff {
            Opcode::decode(op);
        }
    }
}
