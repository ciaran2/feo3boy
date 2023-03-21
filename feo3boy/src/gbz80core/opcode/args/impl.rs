//! Contains the direct-execution implementations of opcode args.
use std::num::Wrapping;

use log::trace;

use crate::gbz80core::opcode::args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};
use crate::gbz80core::oputils::{add8_flags, rotate_left9, rotate_right9, sub8_flags};
use crate::gbz80core::{CpuContext, Flags};
use crate::memdev::MemDevice;

impl AluOp {
    /// Evaluate this ALU operation, updating the accumulator and flags. The argument to the
    /// operation should already have been loaded by the caller and passed as `arg`.
    pub(in crate::gbz80core::opcode) fn exec(self, ctx: &mut impl CpuContext, arg: u8) {
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

impl AluUnaryOp {
    /// Evaluate this ALU unary op.
    pub(in crate::gbz80core::opcode) fn exec(self, ctx: &mut impl CpuContext) {
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

impl Operand8 {
    /// Read this operand from the CPU context, yielding for memory access if needed.
    pub(in crate::gbz80core::opcode) fn read(self, ctx: &mut impl CpuContext) -> u8 {
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
    pub(in crate::gbz80core::opcode) fn write(self, ctx: &mut impl CpuContext, val: u8) {
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

impl Operand16 {
    /// Read this operand from the CPU context, yielding for memory access if needed.
    pub(in crate::gbz80core::opcode) fn read(self, ctx: &mut impl CpuContext) -> u16 {
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
    pub(in crate::gbz80core::opcode) fn write(self, ctx: &mut impl CpuContext, val: u16) {
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

impl ConditionCode {
    /// Returns true if the jump condition is satisfied, that is, the jump *should* happen.
    pub(in crate::gbz80core::opcode) fn evaluate(self, ctx: &mut impl CpuContext) -> bool {
        let flags = ctx.cpu().regs.flags;
        match self {
            Self::Unconditional => true,
            Self::NonZero => !flags.contains(Flags::ZERO),
            Self::Zero => flags.contains(Flags::ZERO),
            Self::NoCarry => !flags.contains(Flags::CARRY),
            Self::Carry => flags.contains(Flags::CARRY),
        }
    }
}
