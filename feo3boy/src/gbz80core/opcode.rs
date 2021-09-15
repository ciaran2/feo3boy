use std::fmt;

/// Parsed Opcode, not including any arguments that may be loaded from immediates, nor any follow-up
/// ops if the operation is a prefixed op.
#[derive(Copy, Clone, Debug)]
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
    /// Conditional call. Load unsigned 16 bit immediate, check condtion, then if condition matches
    /// push current PC and jump to the loaded address.
    Call(ConditionCode),
    /// Loads and executes a prefixed instruction code.
    PrefixCB,
    /// The instruction decoded to an opcode the processor doesn't actually have. This functions
    /// equivalently to Nop (I think????). Contained value is the raw opcode.
    MissingInstruction(u8),
}

impl Opcode {
    /// Parse an opcode.
    pub fn parse(opcode: u8) -> Self {
        // Based on www.z80.info/decoding.htm, but adjusted based on codes which don't exist on the
        // GB Z80.
        let x = (opcode & 0b11000000) >> 6;
        let p = (opcode & 0b00110000) >> 4;
        let y = (opcode & 0b00111000) >> 3;
        let q = (opcode & 0b00001000) != 0;
        let z = opcode & 0b00000111;
        match x {
            0 => match z {
                0 => match y {
                    0 => Self::Nop,
                    1 => unimplemented!(),
                    2 => Self::Stop,
                    code @ 3..=7 => {
                        Self::JumpRelative(ConditionCode::from_relative_cond_code(code))
                    }
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
                7 => unimplemented!(),
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
                3 => match y {
                    1 => Opcode::PrefixCB,
                    // In, Out and EX insturctions are missing on GB Z80.
                    2 | 3 | 4 | 5 => Opcode::MissingInstruction(opcode),
                    0 | 6 | 7 => unimplemented!(),
                    _ => unreachable!(),
                },
                4 => match y {
                    0..=3 => Opcode::Call(ConditionCode::from_absolute_cond_code(y)),
                    // These are conditional calls for conditions the GB Z80 doesn't have.
                    4..=7 => Opcode::MissingInstruction(opcode),
                    _ => unreachable!(),
                },
                5 => match q {
                    false => unimplemented!(),
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
                0 | 1 | 2 | 7 => unimplemented!(),
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
            Self::PrefixCB => f.write_str("PREFIX CB"),
            Self::Call(ConditionCode::Unconditional) => f.write_str("CALL u16"),
            Self::Call(code) => write!(f, "CALL {},u16", code),
            Self::MissingInstruction(opcode) => write!(f, "<Missing Instruction {:2X}>", opcode),
        }
    }
}

/// ALU Operation type.
#[derive(Copy, Clone, Debug)]
pub enum AluOp {
    Add,
    Adc,
    Sub,
    Sbc,
    And,
    Xor,
    Or,
    Cp,
}

impl AluOp {
    /// Get the ALU operation type for the given opcode. Panics if the code is greater than 7.
    fn from_ycode(code: u8) -> Self {
        match code {
            0 => Self::Add,
            1 => Self::Adc,
            2 => Self::Sub,
            3 => Self::Sbc,
            4 => Self::And,
            5 => Self::Xor,
            6 => Self::Or,
            7 => Self::Cp,
            _ => panic!("Unrecognized ALU operation type (y code) {}", code),
        }
    }
}

impl fmt::Display for AluOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::Add => f.write_str("ADD"),
            Self::Adc => f.write_str("ADC"),
            Self::Sub => f.write_str("SUB"),
            Self::Sbc => f.write_str("SBC"),
            Self::And => f.write_str("AND"),
            Self::Xor => f.write_str("XOR"),
            Self::Or => f.write_str("OR"),
            Self::Cp => f.write_str("CP"),
        }
    }
}

/// 8 bit operand. Either the source or destination of an 8 bit operation.
#[derive(Copy, Clone, Debug)]
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
    /// Load value from immediate (and advance program counter). Cannot be used to store. Will
    /// panic if used as the destination operand.
    Immediate,
    /// Dereference a 16 bit immediate value.
    AddrImmediate,
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
        }
    }
}

/// 16 bit operand.
#[derive(Copy, Clone, Debug)]
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
        }
    }
}

/// Conditional for conditional jump/conditional ret.
#[derive(Copy, Clone, Debug)]
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
