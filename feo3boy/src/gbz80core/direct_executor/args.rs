//! Provides evaluators for opcode args.

use std::num::Wrapping;

use feo3boy_opcodes::gbz80types::Flags;
use feo3boy_opcodes::microcode;
use feo3boy_opcodes::opcode::args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};
use log::trace;

use crate::gbz80core::direct_executor::{Ctx, Run, Runner};
use crate::memdev::MemDevice;

impl Runner for AluOp {}
impl Runner for AluUnaryOp {}
impl Runner for ConditionCode {}
impl Runner for Operand16 {}
impl Runner for Operand8 {}

impl Run<Operand8> {
    /// Read this operand from the CPU context, yielding for memory access if needed.
    pub(super) fn read(self, ctx: &mut impl Ctx) -> u8 {
        trace!("Operand8::read {}", self);
        match *self {
            Operand8::A => ctx.cpu().regs.acc,
            Operand8::B => ctx.cpu().regs.b,
            Operand8::C => ctx.cpu().regs.c,
            Operand8::D => ctx.cpu().regs.d,
            Operand8::E => ctx.cpu().regs.e,
            Operand8::H => ctx.cpu().regs.h,
            Operand8::L => ctx.cpu().regs.l,
            Operand8::AddrHL => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrBC => {
                let addr = ctx.cpu().regs.bc();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrDE => {
                let addr = ctx.cpu().regs.de();
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrHLInc => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_add(1));
                ctx.mem().read(addr.into())
            }
            Operand8::AddrHLDec => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_sub(1));
                ctx.mem().read(addr.into())
            }
            Operand8::Immediate => {
                let addr = ctx.cpu_mut().regs.pc;
                ctx.cpu_mut().regs.pc = addr.wrapping_add(1);
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrImmediate => {
                let addr = Operand16::Immediate.runner().read(ctx);
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrRelC => {
                let addr = 0xFF00 + ctx.cpu().regs.c as u16;
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
            Operand8::AddrRelImmediate => {
                let addr = 0xFF00 + Operand8::Immediate.runner().read(ctx) as u16;
                ctx.yield1m();
                ctx.mem().read(addr.into())
            }
        }
    }

    /// Write this operand to the CPU context, yielding for memory access if needed.
    pub(super) fn write(self, ctx: &mut impl Ctx, val: u8) {
        trace!("Operand8::write {} -> {}", val, self);
        match *self {
            Operand8::A => ctx.cpu_mut().regs.acc = val,
            Operand8::B => ctx.cpu_mut().regs.b = val,
            Operand8::C => ctx.cpu_mut().regs.c = val,
            Operand8::D => ctx.cpu_mut().regs.d = val,
            Operand8::E => ctx.cpu_mut().regs.e = val,
            Operand8::H => ctx.cpu_mut().regs.h = val,
            Operand8::L => ctx.cpu_mut().regs.l = val,
            Operand8::AddrHL => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrBC => {
                let addr = ctx.cpu().regs.bc();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrDE => {
                let addr = ctx.cpu().regs.de();
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrHLInc => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_add(1));
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrHLDec => {
                let addr = ctx.cpu().regs.hl();
                ctx.yield1m();
                ctx.cpu_mut().regs.set_hl(addr.wrapping_sub(1));
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::Immediate => panic!("Immediates cannot be used as store destinations"),
            Operand8::AddrImmediate => {
                let addr = Operand16::Immediate.runner().read(ctx);
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrRelC => {
                let addr = 0xFF00 + ctx.cpu().regs.c as u16;
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
            Operand8::AddrRelImmediate => {
                let addr = 0xFF00 + Operand8::Immediate.runner().read(ctx) as u16;
                ctx.yield1m();
                ctx.mem_mut().write(addr.into(), val)
            }
        }
    }
}

impl Run<Operand16> {
    /// Read this operand from the CPU context, yielding for memory access if needed.
    pub(super) fn read(self, ctx: &mut impl Ctx) -> u16 {
        trace!("Operand16::read {}", self);
        match *self {
            Operand16::BC => ctx.cpu().regs.bc(),
            Operand16::DE => ctx.cpu().regs.de(),
            Operand16::HL => ctx.cpu().regs.hl(),
            Operand16::AF => ctx.cpu().regs.af(),
            Operand16::Sp => ctx.cpu().regs.sp,
            Operand16::Immediate => {
                let low = Operand8::Immediate.runner().read(ctx);
                let high = Operand8::Immediate.runner().read(ctx);
                u16::from_le_bytes([low, high])
            }
            Operand16::AddrImmediate => {
                panic!("No actual operation uses (u16) as the source for a 16 bit load")
            }
        }
    }

    /// Write this operand to the CPU context, yielding for memory access if needed.
    pub(super) fn write(self, ctx: &mut impl Ctx, val: u16) {
        trace!("Operand16::write {} -> {}", val, self);
        match *self {
            Operand16::BC => ctx.cpu_mut().regs.set_bc(val),
            Operand16::DE => ctx.cpu_mut().regs.set_de(val),
            Operand16::HL => ctx.cpu_mut().regs.set_hl(val),
            Operand16::AF => ctx.cpu_mut().regs.set_af(val),
            Operand16::Sp => ctx.cpu_mut().regs.sp = val,
            Operand16::Immediate => panic!("Immediates cannot be used as store destinations"),
            Operand16::AddrImmediate => {
                let [low, high] = val.to_le_bytes();
                let mut addr = Wrapping(Operand16::Immediate.runner().read(ctx));
                ctx.yield1m();
                ctx.mem_mut().write(addr.0.into(), low);
                addr += Wrapping(1u16);
                ctx.yield1m();
                ctx.mem_mut().write(addr.0.into(), high);
            }
        }
    }
}

impl Run<AluOp> {
    /// Evaluate this ALU operation, updating the accumulator and flags. The argument to
    /// the operation should already have been loaded by the caller and passed as `arg`.
    pub(super) fn run(self, ctx: &mut impl Ctx, arg: u8) {
        trace!("Evaluating {} A,{}", self, arg);
        match *self {
            AluOp::Add => {
                let (res, flags) = microcode::defs::add(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            AluOp::AddCarry => {
                let (res, flags) =
                    microcode::defs::adc(ctx.cpu().regs.flags, ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            AluOp::Sub => {
                let (res, flags) = microcode::defs::sub(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            AluOp::SubCarry => {
                let (res, flags) =
                    microcode::defs::sbc(ctx.cpu().regs.flags, ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.acc = res;
                ctx.cpu_mut().regs.flags = flags;
            }
            AluOp::And => {
                let (res, flags) = microcode::defs::and(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.flags = flags;
                ctx.cpu_mut().regs.acc = res;
            }
            AluOp::Or => {
                let (res, flags) = microcode::defs::or(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.flags = flags;
                ctx.cpu_mut().regs.acc = res;
            }
            AluOp::Xor => {
                let (res, flags) = microcode::defs::xor(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.flags = flags;
                ctx.cpu_mut().regs.acc = res;
            }
            AluOp::Compare => {
                let (_, flags) = microcode::defs::sub(ctx.cpu().regs.acc, arg);
                ctx.cpu_mut().regs.flags = flags;
            }
        }
    }
}

impl Run<AluUnaryOp> {
    /// Evaluate this ALU unary op.
    pub(super) fn run(self, ctx: &mut impl Ctx) {
        let regs = &mut ctx.cpu_mut().regs;
        match *self {
            AluUnaryOp::RotateLeft8 => {
                let (res, flags) = microcode::defs::rotate_left8(regs.acc);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            AluUnaryOp::RotateLeft9 => {
                let (res, flags) = microcode::defs::rotate_left9(regs.flags, regs.acc);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            AluUnaryOp::RotateRight8 => {
                let (res, flags) = microcode::defs::rotate_right8(regs.acc);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            AluUnaryOp::RotateRight9 => {
                let (res, flags) = microcode::defs::rotate_right9(regs.flags, regs.acc);
                // Clear the zero flag.
                regs.flags = flags - Flags::ZERO;
                regs.acc = res;
            }
            AluUnaryOp::DecimalAdjust => {
                // Does not modify SUB.
                const MASK: Flags = Flags::all().difference(Flags::SUB);
                let (res, flags) = microcode::defs::decmial_adjust(regs.flags, regs.acc);
                regs.flags.merge(flags, MASK);
                regs.acc = res;
            }
            AluUnaryOp::Compliment => {
                // Always sets only SUB and HALFCARRY to 1.
                const MASK: Flags = Flags::SUB.union(Flags::HALFCARRY);
                let (res, flags) = microcode::defs::compliment(regs.acc);
                regs.acc = res;
                regs.flags.merge(flags, MASK);
            }
            AluUnaryOp::SetCarryFlag => {
                // Set all but zero.
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                regs.flags.merge(Flags::CARRY, MASK);
            }
            AluUnaryOp::ComplimentCarryFlag => {
                // Set all but zero.
                const MASK: Flags = Flags::all().difference(Flags::ZERO);
                regs.flags
                    .merge(Flags::check_carry(!regs.flags.contains(Flags::CARRY)), MASK);
            }
        }
    }
}

impl Run<ConditionCode> {
    /// Returns true if the jump condition is satisfied, that is, the jump *should* happen.
    pub(super) fn check(self, ctx: &mut impl Ctx) -> bool {
        let flags = ctx.cpu().regs.flags;
        match *self {
            ConditionCode::Unconditional => true,
            ConditionCode::NonZero => !flags.contains(Flags::ZERO),
            ConditionCode::Zero => flags.contains(Flags::ZERO),
            ConditionCode::NoCarry => !flags.contains(Flags::CARRY),
            ConditionCode::Carry => flags.contains(Flags::CARRY),
        }
    }
}
