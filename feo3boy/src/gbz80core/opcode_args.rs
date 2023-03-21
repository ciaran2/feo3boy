use std::fmt;
#[cfg(not(feature = "microcode"))]
use std::num::Wrapping;

#[cfg(not(feature = "microcode"))]
use log::trace;

#[cfg(not(feature = "microcode"))]
use super::oputils::{add8_flags, rotate_left9, rotate_right9, sub8_flags};
#[cfg(not(feature = "microcode"))]
use super::CpuContext;
use super::Flags;
use crate::gbz80core::microcode::{
    BinaryOp, Immediate16, Immediate8, Mem16, Mem8, Microcode, MicrocodeBuilder, MicrocodeReadable,
    MicrocodeWritable, Reg16, Reg8, UnaryOp,
};
#[cfg(not(feature = "microcode"))]
use crate::memdev::MemDevice;

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

    /// Evaluate this ALU operation, updating the accumulator and flags. The argument to the
    /// operation should already have been loaded by the caller and passed as `arg`.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn exec(self, ctx: &mut impl CpuContext, arg: u8) {
        trace!("Evaluating {} A,{}", self, arg);
        match self {
            Self::Add => {
                let (res, flags) = add8_flags(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            Self::AddCarry => {
                let (mut res, mut flags) = add8_flags(ctx.cpu().regs.acc, arg);
                if ctx.cpu().regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = add8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second add had a result of zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            Self::Sub => {
                let (res, flags) = sub8_flags(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            Self::SubCarry => {
                let (mut res, mut flags) = sub8_flags(ctx.cpu().regs.acc, arg);
                if ctx.cpu().regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = sub8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second subtract had a result of zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            Self::And => {
                let res = ctx.cpu().regs.acc & arg;
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = Flags::HALFCARRY | Flags::check_zero(res);
            }
            Self::Or => {
                let res = ctx.cpu().regs.acc | arg;
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = Flags::check_zero(res);
            }
            Self::Xor => {
                let res = ctx.cpu().regs.acc ^ arg;
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = Flags::check_zero(res);
            }
            Self::Compare => {
                let (_, flags) = sub8_flags(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.flags = flags;
            }
        }
    }
}

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

    /// Evaluate this ALU unary op.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn exec(self, ctx: &mut impl CpuContext) {
        let regs = &mut ctx.cpu_mut().regs;
        match self {
            Self::RotateLeft8 => {
                let res = regs.acc.rotate_left(1);
                regs.flags = Flags::check_carry(res & 1 != 0);
                regs.acc = res;
            }
            Self::RotateLeft9 => {
                let (res, flags) = rotate_left9(regs.acc, regs.flags);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            Self::RotateRight8 => {
                let res = regs.acc.rotate_right(1);
                regs.flags = Flags::check_carry(res & 0x80 != 0);
                regs.acc = res;
            }
            Self::RotateRight9 => {
                let (res, flags) = rotate_right9(regs.acc, regs.flags);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            Self::DecimalAdjust => {
                // Does not modify SUB.
                const MASK: Flags = Flags::all().difference(Flags::SUB);

                // Always clears HALFCARRY.
                let mut resflags = Flags::empty();
                if regs.flags.contains(Flags::SUB) {
                    if regs.flags.contains(Flags::CARRY) {
                        resflags |= Flags::CARRY;
                        regs.acc = regs.acc.wrapping_sub(0x60);
                    }
                    if regs.flags.contains(Flags::HALFCARRY) {
                        regs.acc = regs.acc.wrapping_sub(0x06);
                    }
                } else {
                    if regs.flags.contains(Flags::CARRY) || regs.acc > 0x99 {
                        resflags |= Flags::CARRY;
                        regs.acc = regs.acc.wrapping_add(0x60);
                    }
                    if regs.flags.contains(Flags::HALFCARRY) || regs.acc & 0xf > 9 {
                        regs.acc = regs.acc.wrapping_add(0x06);
                    }
                }
                resflags |= Flags::check_zero(regs.acc);
                regs.flags.merge(resflags, MASK);
            }
            Self::Compliment => {
                // Always sets only SUB and HALFCARRY to 1.
                const FLAGS: Flags = Flags::SUB.union(Flags::HALFCARRY);
                regs.acc = !regs.acc;
                regs.flags.merge(FLAGS, FLAGS);
            }
            Self::SetCarryFlag => {
                // Set all but zero.
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                regs.flags.merge(Flags::CARRY, MASK);
            }
            Self::ComplimentCarryFlag => {
                // Set all but zero.
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                regs.flags
                    .merge(Flags::check_carry(!regs.flags.contains(Flags::CARRY)), MASK);
            }
        }
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

    /// Read this operand from the CPU context, yielding for memory access if needed.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn read(self, ctx: &mut impl CpuContext) -> u8 {
        trace!("Operand8::read {}", self);
        match self {
            Self::A => ctx.cpu().regs.acc,
            Self::B => ctx.cpu().regs.b,
            Self::C => ctx.cpu().regs.c,
            Self::D => ctx.cpu().regs.d,
            Self::E => ctx.cpu().regs.e,
            Self::H => ctx.cpu().regs.h,
            Self::L => ctx.cpu().regs.l,
            Self::AddrHL => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrBC => {
                let addr = ctx.cpu().regs.bc();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrDE => {
                let addr = ctx.cpu().regs.de();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrHLInc => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_add(1));
                ctx.mem().read(addr.into())
            }
            Self::AddrHLDec => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_sub(1));
                ctx.mem().read(addr.into())
            }
            Self::Immediate => {
                let addr = ctx.cpu_mut().regs.inc_pc();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrImmediate => {
                let addr = Operand16::Immediate.read(ctx);
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrRelC => {
                let addr = 0xFF00 + ctx.cpu().regs.c as u16;
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Self::AddrRelImmediate => {
                let addr = 0xFF00 + Self::Immediate.read(ctx) as u16;
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
        }
    }

    /// Write this operand to the CPU context, yielding for memory access if needed.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn write(self, ctx: &mut impl CpuContext, val: u8) {
        trace!("Operand8::write {} -> {}", val, self);
        match self {
            Self::A => ctx.cpu_mut().regs.acc = val,
            Self::B => ctx.cpu_mut().regs.b = val,
            Self::C => ctx.cpu_mut().regs.c = val,
            Self::D => ctx.cpu_mut().regs.d = val,
            Self::E => ctx.cpu_mut().regs.e = val,
            Self::H => ctx.cpu_mut().regs.h = val,
            Self::L => ctx.cpu_mut().regs.l = val,
            Self::AddrHL => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrBC => {
                let addr = ctx.cpu().regs.bc();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrDE => {
                let addr = ctx.cpu().regs.de();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrHLInc => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_add(1));
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrHLDec => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_sub(1));
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            Self::AddrImmediate => {
                let addr = Operand16::Immediate.read(ctx);
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrRelC => {
                let addr = 0xFF00 + ctx.cpu().regs.c as u16;
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Self::AddrRelImmediate => {
                let addr = 0xFF00 + Self::Immediate.read(ctx) as u16;
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
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

    /// Read this operand from the CPU context, yielding for memory access if needed.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn read(self, ctx: &mut impl CpuContext) -> u16 {
        trace!("Operand16::read {}", self);
        match self {
            Self::BC => ctx.cpu().regs.bc(),
            Self::DE => ctx.cpu().regs.de(),
            Self::HL => ctx.cpu().regs.hl(),
            Self::AF => ctx.cpu().regs.af(),
            Self::Sp => ctx.cpu().regs.sp,
            Self::Immediate => {
                let low = Operand8::Immediate.read(ctx);
                let high = Operand8::Immediate.read(ctx);
                u16::from_le_bytes([low, high])
            }
            Self::AddrImmediate => {
                panic!("No actual operation uses (u16) as the source for a 16 bit load")
            }
        }
    }

    /// Write this operand to the CPU context, yielding for memory access if needed.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn write(self, ctx: &mut impl CpuContext, val: u16) {
        trace!("Operand16::write {} -> {}", val, self);
        match self {
            Self::BC => ctx.cpu_mut().regs.set_bc(val),
            Self::DE => ctx.cpu_mut().regs.set_de(val),
            Self::HL => ctx.cpu_mut().regs.set_hl(val),
            Self::AF => ctx.cpu_mut().regs.set_af(val),
            Self::Sp => ctx.cpu_mut().regs.sp = val,
            Self::Immediate => panic!("Immediates cannot be used as store destinations"),
            Self::AddrImmediate => {
                let [low, high] = val.to_le_bytes();
                let mut addr = Wrapping(Self::Immediate.read(ctx));
                ctx.yield1m();
                ctx.mem_mut().write(addr.0.into(), low);
                addr += Wrapping(1u16);
                ctx.yield1m();
                ctx.mem_mut().write(addr.0.into(), high);
            }
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

    /// Returns true if the jump condition is satisfied, that is, the jump *should* happen.
    #[cfg(not(feature = "microcode"))]
    pub(super) fn evaluate(self, ctx: &mut impl CpuContext) -> bool {
        let flags = ctx.cpu().regs.flags;
        match self {
            Self::Unconditional => true,
            Self::NonZero => !flags.contains(Flags::ZERO),
            Self::Zero => flags.contains(Flags::ZERO),
            Self::NoCarry => !flags.contains(Flags::CARRY),
            Self::Carry => flags.contains(Flags::CARRY),
        }
    }

    /// Wrap the provided microcode in a conditional handling. If unconditional, just
    /// returns the `code_if_condition_true`. If conditional, fetches the appropriate flag
    /// and applies a micorcode conditional to run the provided code only when the
    /// condition matches.
    pub(super) fn cond(
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
    pub(super) fn if_true(
        self,
        code_if_condition_true: impl Into<MicrocodeBuilder>,
    ) -> MicrocodeBuilder {
        self.cond(code_if_condition_true, MicrocodeBuilder::new())
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
