//! Contains implementations of microcode operations. Only needed when in microcode mode.

use std::mem;

use crate::gbz80core::microcode::{BinaryOp, BitOp, Instr, Microcode, Reg16, Reg8, UnaryOp};
use crate::gbz80core::opcode::{CBOpcode, Opcode};
use crate::gbz80core::oputils::{add8_flags, halt, rotate_left9, rotate_right9, sub8_flags};
use crate::gbz80core::{CpuContext, Flags};
use crate::interrupts::Interrupts;
use crate::memdev::{Addr, MemDevice};

/// Result of executing a microcode instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MicrocodeFlow {
    /// Microcode instruction requires a yield. CPU should stop executing microcode until
    /// 4 ticks have elapsed.
    Yield1m,
    /// Continue executing microcode.
    Continue,
}

/// Stack used to implement microcode operations.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MicrocodeStack {
    bytes: Vec<u8>,
}

impl MicrocodeStack {
    /// Check if the microcode stack is empty.
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Push a u8 onto the microcode stack.
    pub fn pushu8(&mut self, val: u8) {
        self.bytes.push(val);
    }

    /// Pop a u8 from the microcode stack.
    pub fn popu8(&mut self) -> u8 {
        self.bytes
            .pop()
            .expect("Cannot pop u8, microcode stack empty")
    }

    /// Read the top u8 without popping.
    pub fn peeku8(&self) -> u8 {
        *self
            .bytes
            .last()
            .expect("Cannot peek u8, microcode stack empty")
    }

    /// Push a u16 onto the microcode stack. The low byte is pushed first followed by the
    /// high byte on top.
    pub fn pushu16(&mut self, val: u16) {
        let val = val.to_le_bytes();
        self.bytes.extend_from_slice(&val);
    }

    /// Pop a u16 from the microcode stack. The high byte is popped first followed by the
    /// low byte below it.
    pub fn popu16(&mut self) -> u16 {
        debug_assert!(
            self.bytes.len() >= 2,
            "Not enough bytes to pop u16, need 2, have {}",
            self.bytes.len()
        );
        let mut val = [0u8; 2];
        val.copy_from_slice(&self.bytes[self.bytes.len() - 2..]);
        self.bytes.truncate(self.bytes.len() - 2);
        u16::from_le_bytes(val)
    }

    /// Peek the top u16 without popping. The top byte is treated as the high byte and the
    /// second byte is treated as the low byte.
    pub fn peeku16(&self) -> u16 {
        debug_assert!(
            self.bytes.len() >= 2,
            "Not enough bytes to peek u16, need 2, have {}",
            self.bytes.len()
        );
        let mut val = [0u8; 2];
        val.copy_from_slice(&self.bytes[self.bytes.len() - 2..]);
        u16::from_le_bytes(val)
    }

    /// Duplicates the specified number of bytes on the top of the stack.
    pub fn dup(&mut self, count: usize) {
        debug_assert!(
            self.bytes.len() >= count,
            "Not enough bytes on the microcode stack to duplicated {} bytes, only have {}",
            count,
            self.bytes.len()
        );
        self.bytes.extend_from_within(self.bytes.len() - count..);
    }

    /// Swaps `top_count` bytes on the top of the stack with `second_count` bytes below
    /// it.
    pub fn swap(&mut self, top_count: usize, second_count: usize) {
        debug_assert!(
            self.bytes.len() >= top_count + second_count,
            "Not enough bytes on the microcode stack to swap {} bytes with {} bytes, only have {}",
            top_count,
            second_count,
            self.bytes.len()
        );
        let second_start = self.bytes.len() - (top_count + second_count);
        let second_end = self.bytes.len() - top_count;
        self.bytes.extend_from_within(second_start..second_end);
        self.bytes.drain(second_start..second_end);
    }

    /// Discards the specified number of bytes from the top of the stack.
    pub fn discard(&mut self, count: usize) {
        debug_assert!(
            self.bytes.len() >= count,
            "Not enough bytes on the microcode stack to discard {} bytes, only have {}",
            count,
            self.bytes.len()
        );
        self.bytes.drain(self.bytes.len() - count..);
    }
}

impl Microcode {
    pub(in crate::gbz80core) fn eval(self, ctx: &mut impl CpuContext) -> MicrocodeFlow {
        match self {
            Self::Yield => return MicrocodeFlow::Yield1m,
            #[cfg(feature = "combo-code")]
            Self::ComboCode(code) => return code.eval(ctx),
            Self::ReadReg8(reg) => reg.read(ctx),
            Self::WriteReg8(reg) => reg.write(ctx),
            Self::ReadMem8 => read_mem8(ctx),
            Self::WriteMem8 => write_mem8(ctx),
            Self::Append(val) => ctx.cpu_mut().microcode.stack.pushu8(val),
            Self::GetFlagsMasked { mask } => get_flags_masked(ctx, mask),
            Self::SetFlagsMasked { mask } => set_flags_masked(ctx, mask),
            Self::BinaryOp(op) => op.eval(ctx),
            Self::UnaryOp(op) => op.eval(ctx),
            Self::BitOp(op) => op.eval(ctx),
            Self::ReadReg16(reg) => reg.read(ctx),
            Self::WriteReg16(reg) => reg.write(ctx),
            Self::Inc16 => inc16(ctx),
            Self::Dec16 => dec16(ctx),
            Self::Add16 => add16(ctx),
            Self::OffsetAddr => offset_addr(ctx),
            Self::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Self::Halt => halt(ctx),
            Self::EnableInterrupts { immediate } => enable_interrupts(ctx, immediate),
            Self::DisableInterrupts => ctx.cpu_mut().interrupt_master_enable.clear(),
            Self::CheckHalt => check_halt(ctx),
            Self::ClearHalt => ctx.cpu_mut().halted = false,
            Self::CheckIme => check_ime(ctx),
            Self::GetActiveInterrupts => get_active_interrupts(ctx),
            Self::PopHaltBug => pop_halt_bug(ctx),
            Self::PopInterrupt => pop_interrupt(ctx),
            Self::TickImeOnEnd => tick_ime_on_end(ctx),
            Self::Skip { steps } => skip(ctx, steps),
            Self::SkipIf { steps } => skip_if(ctx, steps),
            Self::SkipIfNot { steps } => skip_if_not(ctx, steps),
            Self::FetchNextInstruction => fetch_next_instruction(ctx),
            Self::ParseOpcode => parse_opcode(ctx),
            Self::ParseCBOpcode => parse_cb_opcode(ctx),
            Self::Dup(count) => ctx.cpu_mut().microcode.stack.dup(count),
            Self::Swap { top, second } => ctx.cpu_mut().microcode.stack.swap(top, second),
            Self::Discard(count) => ctx.cpu_mut().microcode.stack.discard(count),
        }
        MicrocodeFlow::Continue
    }
}

impl Reg8 {
    /// Read this register into the microcode stack.
    fn read(self, ctx: &mut impl CpuContext) {
        let val = match self {
            Self::Acc => ctx.cpu().regs.acc,
            Self::B => ctx.cpu().regs.b,
            Self::C => ctx.cpu().regs.c,
            Self::D => ctx.cpu().regs.d,
            Self::E => ctx.cpu().regs.e,
            Self::H => ctx.cpu().regs.h,
            Self::L => ctx.cpu().regs.l,
        };
        ctx.cpu_mut().microcode.stack.pushu8(val);
    }

    /// Write this register from the microcode stack.
    fn write(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode.stack.popu8();
        match self {
            Self::Acc => ctx.cpu_mut().regs.acc = val,
            Self::B => ctx.cpu_mut().regs.b = val,
            Self::C => ctx.cpu_mut().regs.c = val,
            Self::D => ctx.cpu_mut().regs.d = val,
            Self::E => ctx.cpu_mut().regs.e = val,
            Self::H => ctx.cpu_mut().regs.h = val,
            Self::L => ctx.cpu_mut().regs.l = val,
        }
    }
}

impl Reg16 {
    fn read(self, ctx: &mut impl CpuContext) {
        let val = match self {
            Self::AF => ctx.cpu().regs.af(),
            Self::BC => ctx.cpu().regs.bc(),
            Self::DE => ctx.cpu().regs.de(),
            Self::HL => ctx.cpu().regs.hl(),
            Self::Sp => ctx.cpu().regs.sp,
            Self::Pc => ctx.cpu().regs.pc,
        };
        ctx.cpu_mut().microcode.stack.pushu16(val);
    }

    fn write(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode.stack.popu16();
        match self {
            Self::AF => ctx.cpu_mut().regs.set_af(val),
            Self::BC => ctx.cpu_mut().regs.set_bc(val),
            Self::DE => ctx.cpu_mut().regs.set_de(val),
            Self::HL => ctx.cpu_mut().regs.set_hl(val),
            Self::Sp => ctx.cpu_mut().regs.sp = val,
            Self::Pc => ctx.cpu_mut().regs.pc = val,
        }
    }
}

/// Pop a 16 bit address and use it to read an 8 bit value from memory onto the stack.
fn read_mem8(ctx: &mut impl CpuContext) {
    let addr = Addr::from(ctx.cpu_mut().microcode.stack.popu16());
    let val = ctx.mem().read(addr);
    ctx.cpu_mut().microcode.stack.pushu8(val);
}

/// Pop a 16 bit address and an 8 bit value and write the value to the address.
fn write_mem8(ctx: &mut impl CpuContext) {
    let addr = Addr::from(ctx.cpu_mut().microcode.stack.popu16());
    let val = ctx.cpu_mut().microcode.stack.popu8();
    ctx.mem_mut().write(addr, val);
}

/// Internal implementation of microcode to fetch masked flags to the output.
fn get_flags_masked(ctx: &mut impl CpuContext, mask: Flags) {
    let flags = ctx.cpu().regs.flags.intersection(mask);
    ctx.cpu_mut().microcode.stack.pushu8(flags.bits());
}

/// Internal implementation of microcode to apply masked flags to the output.
fn set_flags_masked(ctx: &mut impl CpuContext, mask: Flags) {
    let flags = Flags::from_bits_truncate(ctx.cpu_mut().microcode.stack.popu8());
    ctx.cpu_mut().regs.flags.merge(flags, mask);
}

/// Internal implementation of microcode u16 increment.
fn inc16(ctx: &mut impl CpuContext) {
    let val = ctx.cpu_mut().microcode.stack.popu16();
    ctx.cpu_mut().microcode.stack.pushu16(val.wrapping_add(1));
}

/// Internal implementation of microcode u16 decrement.
fn dec16(ctx: &mut impl CpuContext) {
    let val = ctx.cpu_mut().microcode.stack.popu16();
    ctx.cpu_mut().microcode.stack.pushu16(val.wrapping_sub(1));
}

/// Internal implementation of microcode u16 add.
fn add16(ctx: &mut impl CpuContext) {
    let lhs = ctx.cpu_mut().microcode.stack.popu16();
    let rhs = ctx.cpu_mut().microcode.stack.popu16();

    let mut flags = Flags::empty();
    if (lhs & 0x7ff) + (rhs & 0x7ff) > 0x7ff {
        flags |= Flags::HALFCARRY;
    }
    let (res, carry) = lhs.overflowing_add(rhs);
    flags |= Flags::check_carry(carry);

    ctx.cpu_mut().microcode.stack.pushu16(res);
    ctx.cpu_mut().microcode.stack.pushu8(flags.bits());
}

fn offset_addr(ctx: &mut impl CpuContext) {
    use crate::gbz80core::oputils::offset_addr as offset_internal;
    let cpu = ctx.cpu_mut();

    let addr = cpu.microcode.stack.popu16();
    let offset = cpu.microcode.stack.popu8() as i8;

    let (res, flags) = offset_internal(addr, offset);
    cpu.microcode.stack.pushu16(res);
    cpu.microcode.stack.pushu8(flags.bits());
}

/// Internal implementation of the microcode enable interrupts instruction. Runs either
/// set or set_next_instruction, depending on whether the interrupt is to be set
/// immeidately or not.
fn enable_interrupts(ctx: &mut impl CpuContext, immediate: bool) {
    if immediate {
        ctx.cpu_mut().interrupt_master_enable.set();
    } else {
        ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
    }
}

/// Pushes the value of whether the CPU is halted onto the microcode stack as a u8.
fn check_halt(ctx: &mut impl CpuContext) {
    let halted = ctx.cpu().halted as u8;
    ctx.cpu_mut().microcode.stack.pushu8(halted);
}

/// Pushes the value of whether the CPU IME is enabled onto the microcode stack as a
/// u8.
fn check_ime(ctx: &mut impl CpuContext) {
    let ime = ctx.cpu().interrupt_master_enable.enabled() as u8;
    ctx.cpu_mut().microcode.stack.pushu8(ime);
}

/// Pushes the set of interrupts which are both active and enabled onto the microcode
/// stack.
fn get_active_interrupts(ctx: &mut impl CpuContext) {
    let active = ctx.interrupts().active().bits();
    ctx.cpu_mut().microcode.stack.pushu8(active);
}

/// Pushes the value of the halt_bug flag onto the microcode stack, clearing the value.
fn pop_halt_bug(ctx: &mut impl CpuContext) {
    let halt_bug = mem::replace(&mut ctx.cpu_mut().halt_bug, false) as u8;
    ctx.cpu_mut().microcode.stack.pushu8(halt_bug);
}

/// Pushes the destination address of the next active and enabled interrupt onto the
/// microcode stack and clears that interrupt. Panics if no interrupts are active!.
fn pop_interrupt(ctx: &mut impl CpuContext) {
    match ctx.interrupts().active().iter().next() {
            Some(interrupt) => {
                ctx.interrupts_mut().clear(interrupt);
                ctx.cpu_mut().microcode.stack.pushu16(interrupt.handler_addr());
            }
            None => panic!("Must not use the PopInterrupt microcode instruction if there are no active interrupts."),
        }
}

/// Enables IME ticking on the next `FetchNextInstruction`.
pub(super) fn tick_ime_on_end(ctx: &mut impl CpuContext) {
    let cpu = ctx.cpu_mut();
    debug_assert!(
        cpu.microcode.prev_ime.is_none(),
        "prev_ime is already set to {:?}, trying to set {:?}",
        cpu.microcode.prev_ime.unwrap(),
        cpu.interrupt_master_enable
    );
    cpu.microcode.prev_ime = Some(cpu.interrupt_master_enable);
}

/// Skip the CPU forward by this number of microcode instruction steps.
fn skip(ctx: &mut impl CpuContext, steps: usize) {
    // Only checked for overflow in debug.
    ctx.cpu_mut().microcode.pc += steps;
    debug_assert!(ctx.cpu().microcode.pc <= ctx.cpu().microcode.instruction.len());
}

/// Skip the CPU forward by this number of microcode instruction steps if the u8 value on
/// top of the microcode stack is non-zero.
fn skip_if(ctx: &mut impl CpuContext, steps: usize) {
    let cond = ctx.cpu_mut().microcode.stack.popu8();
    if cond != 0 {
        // Only checked for overflow in debug.
        ctx.cpu_mut().microcode.pc += steps;
        debug_assert!(ctx.cpu().microcode.pc <= ctx.cpu().microcode.instruction.len());
    }
}

/// Skip the CPU forward by this number of microcode instruction steps if the u8 value on
/// top of the microcode stack is zero.
fn skip_if_not(ctx: &mut impl CpuContext, steps: usize) {
    let cond = ctx.cpu_mut().microcode.stack.popu8();
    if cond == 0 {
        // Only checked for overflow in debug.
        ctx.cpu_mut().microcode.pc += steps;
        debug_assert!(ctx.cpu().microcode.pc <= ctx.cpu().microcode.instruction.len());
    }
}

/// Resets to the CPU Internal instruction.
// Shared with combo-codes.
pub(super) fn fetch_next_instruction(ctx: &mut impl CpuContext) {
    let cpu = ctx.cpu_mut();
    debug_assert!(
        cpu.microcode.stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        cpu.microcode.instruction.label()
    );
    if let Some(prev_ime) = cpu.microcode.prev_ime.take() {
        cpu.interrupt_master_enable.tick(prev_ime);
    }
    cpu.microcode.pc = 0;
    cpu.microcode.instruction = Instr::internal_fetch();
    cpu.microcode.is_fetch_start = true;
}

/// Parses a regular opcode from the microcode stack and replaces the current
/// instruction with it.
pub(super) fn parse_opcode(ctx: &mut impl CpuContext) {
    let cpu = ctx.cpu_mut();
    let opcode = cpu.microcode.stack.popu8();
    debug_assert!(
        cpu.microcode.stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        cpu.microcode.instruction.label()
    );
    cpu.microcode.pc = 0;
    cpu.microcode.instruction = Opcode::get_instruction(opcode);
}

/// Parses a cb opcode from the microcode stack and replaces the current
/// instruction with it.
fn parse_cb_opcode(ctx: &mut impl CpuContext) {
    let cpu = ctx.cpu_mut();
    let opcode = cpu.microcode.stack.popu8();
    debug_assert!(
        cpu.microcode.stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        cpu.microcode.instruction.label()
    );
    cpu.microcode.pc = 0;
    cpu.microcode.instruction = CBOpcode::get_instruction(opcode);
}

impl BinaryOp {
    fn eval(self, ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        let lhs = cpu.microcode.stack.popu8();
        let rhs = cpu.microcode.stack.popu8();
        let (res, flags) = match self {
            Self::Add => add8_flags(lhs, rhs),
            Self::AddCarry => {
                let (mut res, mut flags) = add8_flags(lhs, rhs);
                if cpu.regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = add8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second add had a result of zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                (res, flags)
            }
            Self::Sub => sub8_flags(lhs, rhs),
            Self::SubCarry => {
                let (mut res, mut flags) = sub8_flags(lhs, rhs);
                if cpu.regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = sub8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second subtract had a result of
                    // zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                (res, flags)
            }
            Self::And => {
                let res = lhs & rhs;
                let flags = Flags::HALFCARRY | Flags::check_zero(res);
                (res, flags)
            }
            Self::Or => {
                let res = lhs | rhs;
                let flags = Flags::check_zero(res);
                (res, flags)
            }
            Self::Xor => {
                let res = lhs ^ rhs;
                let flags = Flags::check_zero(res);
                (res, flags)
            }
        };
        cpu.microcode.stack.pushu8(res);
        cpu.microcode.stack.pushu8(flags.bits);
    }
}

impl UnaryOp {
    fn eval(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode.stack.popu8();
        let (res, flags) = match self {
            Self::RotateLeft8 => {
                let res = val.rotate_left(1);
                let flags = Flags::check_zero(res) | Flags::check_carry(res & 1 != 0);
                (res, flags)
            }
            Self::RotateLeft9 => rotate_left9(val, ctx.cpu().regs.flags),
            Self::RotateRight8 => {
                let res = val.rotate_right(1);
                let flags = Flags::check_zero(res) | Flags::check_carry(res & 0x80 != 0);
                (res, flags)
            }
            Self::RotateRight9 => rotate_right9(val, ctx.cpu().regs.flags),
            Self::DecimalAdjust => {
                let inflags = ctx.cpu().regs.flags;
                // Always clears HALFCARRY.
                let mut flags = Flags::empty();
                let mut res = val;
                if inflags.contains(Flags::SUB) {
                    if inflags.contains(Flags::CARRY) {
                        flags |= Flags::CARRY;
                        res = res.wrapping_sub(0x60);
                    }
                    if inflags.contains(Flags::HALFCARRY) {
                        res = res.wrapping_sub(0x06);
                    }
                } else {
                    if inflags.contains(Flags::CARRY) || val > 0x99 {
                        flags |= Flags::CARRY;
                        res = res.wrapping_add(0x60);
                    }
                    if inflags.contains(Flags::HALFCARRY) || res & 0xf > 9 {
                        res = res.wrapping_add(0x06);
                    }
                }
                flags |= Flags::check_zero(res);
                (res, flags)
            }
            Self::Compliment => {
                const FLAGS: Flags = Flags::SUB.union(Flags::HALFCARRY);
                let res = !val;
                (res, FLAGS)
            }
            Self::ShiftLeft => {
                let res = val << 1;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 0x80 != 0);
                (res, flags)
            }
            Self::ShiftRight => {
                let res = val >> 1;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
                (res, flags)
            }
            Self::ShiftRightSignExt => {
                let res = ((val as i8) >> 1) as u8;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
                (res, flags)
            }
            Self::Swap => {
                let res = ((val & 0x0f) << 4) | ((val & 0xf0) >> 4);
                let flags = Flags::check_zero(res);
                (res, flags)
            }
        };
        ctx.cpu_mut().microcode.stack.pushu8(res);
        ctx.cpu_mut().microcode.stack.pushu8(flags.bits());
    }
}

impl BitOp {
    fn eval(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode.stack.popu8();
        let res = match self {
            Self::TestBit(bit) => {
                let flags = Flags::check_zero(val & (1 << bit)) | Flags::HALFCARRY;
                flags.bits()
            }
            Self::SetBit(bit) => val | (1 << bit),
            Self::ResetBit(bit) => val & !(1 << bit),
        };
        ctx.cpu_mut().microcode.stack.pushu8(res);
    }
}
