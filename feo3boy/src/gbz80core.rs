use bitflags::bitflags;

use crate::interrupts::{InterruptContext, Interrupts, MemInterrupts};
use crate::memdev::{MemContext, MemDevice};
pub use opcode::{CBOpcode, CBOperation, Opcode};
pub use opcode_args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};

mod opcode;
mod opcode_args;
mod oputils;

bitflags! {
    /// Flags set after various operations.
    #[derive(Default)]
    pub struct Flags: u8 {
        /// result was zero.
        const ZERO = 0x80;
        /// operation was a subtraction.
        const SUB = 0x40;
        /// there was a carry in the middle of the number (bit 4 -> 5 for u8, uncertain which bits
        /// this is for in u16).
        const HALFCARRY = 0x20;
        /// there was a carry out of the top of the number (bit 7 -> carry for u8, presumably bit 15
        /// -> carry for u16, though not sure).
        const CARRY = 0x10;
    }
}

impl Flags {
    /// Merge the given flags into the current flags by applying the given mask to set only flags in
    /// that mask.
    pub fn merge(&mut self, flags: Flags, mask: Flags) {
        *self = (*self & !mask) | (flags & mask);
    }

    /// If the value is zero, returns `Flags::ZERO`, otherwise returns `Flags::empty()`.
    pub fn check_zero(val: u8) -> Flags {
        if val == 0 {
            Flags::ZERO
        } else {
            Flags::empty()
        }
    }

    /// If carry is true, returns `Flags::CARRY` otherwise returns `Flags::empty()`.
    pub fn check_carry(carry: bool) -> Flags {
        if carry {
            Flags::CARRY
        } else {
            Flags::empty()
        }
    }
}

/// CPU registers on the GB Z80 processor.
///
/// Note that there are a few other registers on a GameBoy, but those are memory mapped.
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Regs {
    // Registers are paired in little-endian order (though we aren't using any specific #[repr], so
    // compiler is free to reorder them).
    /// Register F.
    pub flags: Flags,
    /// Register A.
    pub acc: u8,
    /// Register C.
    pub c: u8,
    /// Register B.
    pub b: u8,
    /// Register E.
    pub e: u8,
    /// Register D.
    pub d: u8,
    /// Register L.
    pub l: u8,
    /// Register H.
    pub h: u8,
    /// Stack pointer.
    pub sp: u16,
    /// Program counter.
    pub pc: u16,
}

macro_rules! reg_pair_access {
    ($name:ident, $get:ident, $set:ident, $h:ident, $l:ident) => {
        /// Gets the value of register pair $name.
        pub fn $get(&self) -> u16 {
            u16::from_le_bytes([self.$l, self.$h])
        }

        /// Sets the value of register pair $name.
        pub fn $set(&mut self, val: u16) {
            let [low, high] = val.to_le_bytes();
            self.$l = low;
            self.$h = high;
        }
    };
}

impl Regs {
    /// Gets the value of register pair AF.
    pub fn af(&self) -> u16 {
        u16::from_le_bytes([self.flags.bits, self.acc])
    }

    /// Sets the value of register pair AF. Any low-order bits will be truncated from F.
    pub fn set_af(&mut self, val: u16) {
        let [f, a] = val.to_le_bytes();
        self.flags = Flags::from_bits_truncate(f);
        self.acc = a;
    }

    reg_pair_access!(BC, bc, set_bc, b, c);
    reg_pair_access!(DE, de, set_de, d, e);
    reg_pair_access!(HL, hl, set_hl, h, l);

    /// Returns the current program counter, and increments the value.
    pub fn inc_pc(&mut self) -> u16 {
        let pc = self.pc;
        self.pc = pc.wrapping_add(1);
        pc
    }

    /// Returns the current stack pointer and increments the value.
    pub fn inc_sp(&mut self) -> u16 {
        let sp = self.sp;
        self.sp = sp.wrapping_add(1);
        sp
    }

    /// Returns the current stack pointer and decrements the value.
    pub fn dec_sp(&mut self) -> u16 {
        self.sp = self.sp.wrapping_sub(1);
        self.sp
    }
}

/// State of interrupts on the CPU.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InterruptMasterState {
    /// Interrupts are disabled.
    Disabled,
    /// EI was just run, but the effect is delayed until after the next instruction.
    Pending,
    /// Interrupts are enabled.
    Enabled,
}

impl InterruptMasterState {
    /// Sets interrupts immediately.
    pub fn set(&mut self) {
        *self = Self::Enabled;
    }

    /// Disables interrupts immediately.
    pub fn clear(&mut self) {
        *self = Self::Disabled;
    }

    /// Sets interrupts after the next instruction. If interrupts are already enabled, does nothing.
    pub fn set_next_instruction(&mut self) {
        if *self != Self::Enabled {
            *self = Self::Pending;
        }
    }

    /// Returns true if IME is enabled.
    pub fn enabled(self) -> bool {
        self == Self::Enabled
    }

    /// Ticks the interrupt master state after an instruction has executed.
    fn tick(&mut self, previous_state: Self) {
        // If it was pending, and was not disabled by the instruction run in the mean time, set to
        // enabled.
        if previous_state == Self::Pending && *self != Self::Disabled {
            *self = Self::Enabled;
        }
    }
}

impl Default for InterruptMasterState {
    fn default() -> Self {
        Self::Disabled
    }
}

/// Internal state of the CPU.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Gbz80State {
    /// Cpu registers.
    pub regs: Regs,
    /// Interrupt master enable flag, controlled by EI, DI, RETI, and interrupts.
    pub interrupt_master_enable: InterruptMasterState,
    /// Whether the CPU is halted.
    pub halted: bool,
    /// Set to true when the halt bug is tripped until the double-read of the program
    /// counter.
    pub halt_bug: bool,
}

impl Gbz80State {
    /// Create a new Gbz80State.
    pub fn new() -> Gbz80State {
        Default::default()
    }
}

/// Context trait which encapsulates everything that the CPU needs in order to execute.
///
/// The purpose of this trait is to encapsulate the components needed to run the GB Z80 CPU,
/// independently of any other component of the GameBoy system. That allows the CPU to be run for
/// other purposes, by swapping in a memory controller that behaves differently.
pub trait CpuContext: MemContext + InterruptContext {
    /// Gets the CPU state.
    fn cpu(&self) -> &Gbz80State;

    /// Gets a mutable reference to the CPU state.
    fn cpu_mut(&mut self) -> &mut Gbz80State;

    /// Yields from CPU execution for 1 M clock cycle (4 T). This callback should step the clock
    /// forward and perform any work that needs to happen faster than instructions execute.
    /// Warning: It is undefined behavior to call `tick` again during a context yield.
    fn yield1m(&mut self);
}

/// Runs a single instruction on the CPU.
pub fn tick(ctx: &mut impl CpuContext) {
    if ctx.cpu().halted {
        if ctx.interrupts().active().is_empty() {
            return;
        }
        ctx.cpu_mut().halted = false;
    }
    if opcode::service_interrupt(ctx) {
        return;
    }

    let previous_ime = ctx.cpu().interrupt_master_enable;
    Opcode::load_and_execute(ctx);
    ctx.cpu_mut().interrupt_master_enable.tick(previous_ime);
}

/////////////////////////////////////////
// Utility implementations of CpuContext.
/////////////////////////////////////////

/// Allows a tuple of Gbz80State and any MemDevice to be used as CpuContext.
impl<M: MemDevice> CpuContext for (Gbz80State, M) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        &self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        &mut self.0
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M: MemDevice> MemContext for (Gbz80State, M) {
    type Mem = M;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        &self.1
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        &mut self.1
    }
}

impl<M: MemDevice> InterruptContext for (Gbz80State, M) {
    type Interrupts = MemInterrupts<M>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}

/// Allows a tuple of references to Gbz80State and any MemDevice to be used as CpuContext.
impl<M: MemDevice> CpuContext for (&mut Gbz80State, &mut M) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        self.0
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M: MemDevice> MemContext for (&mut Gbz80State, &mut M) {
    type Mem = M;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        self.1
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        self.1
    }
}

impl<M: MemDevice> InterruptContext for (&mut Gbz80State, &mut M) {
    type Interrupts = MemInterrupts<M>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loads_and_alu() {
        let mut cpu = Gbz80State::new();
        let mut testmem = [0u8; 0x10000];

        testmem[0] = 0x3e;
        testmem[1] = 0x80;
        testmem[2] = 0x06;
        testmem[3] = 0x01;
        testmem[4] = 0x80;
        testmem[5] = 0x0e;
        testmem[6] = 0x85;
        testmem[7] = 0x81;

        tick(&mut (&mut cpu, &mut testmem));
        assert_eq!(cpu.regs.acc, 0x80, "{:?}", cpu.regs);

        tick(&mut (&mut cpu, &mut testmem));
        assert_eq!(cpu.regs.b, 0x01, "{:?}", cpu.regs);

        tick(&mut (&mut cpu, &mut testmem));
        assert_eq!(cpu.regs.acc, 0x81, "{:?}", cpu.regs);

        tick(&mut (&mut cpu, &mut testmem));
        assert_eq!(cpu.regs.c, 0x85, "{:?}", cpu.regs);

        tick(&mut (&mut cpu, &mut testmem));
        assert_eq!(cpu.regs.acc, 0x6, "{:?}", cpu.regs);
        assert!(cpu.regs.flags.contains(Flags::CARRY), "{:?}", cpu.regs);
    }

    #[test]
    fn load8bit() {
        fn set_dest(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match inst {
                0x40..=0x47 => ctx.cpu_mut().regs.b = val,
                0x48..=0x4f => ctx.cpu_mut().regs.c = val,
                0x50..=0x57 => ctx.cpu_mut().regs.d = val,
                0x58..=0x5f => ctx.cpu_mut().regs.e = val,
                0x60..=0x67 => ctx.cpu_mut().regs.h = val,
                0x68..=0x6f => ctx.cpu_mut().regs.l = val,
                0x70..=0x77 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                0x78..=0x7f => ctx.cpu_mut().regs.acc = val,
                _ => unreachable!(),
            }
        }
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                0x7 => ctx.cpu_mut().regs.acc = val,
                _ => unreachable!(),
            }
        }
        for inst in (0x40..=0x7f).filter(|&i| i != 0x76) {
            for input in 0..=0xff {
                let mut ctx = (Gbz80State::default(), [0u8; 0x10000]);
                // Start with HL at a high number so any single-byte change to it will never
                // collide with the instruction at 0x00.
                ctx.0.regs.set_hl(0xAA);
                // This step might overwrite H or L changing the destination address of LD (HL),H
                // or LD (HL),L, but that's ok because we use HL in set_dest.
                set_source(inst, &mut ctx, input);
                ctx.1[0] = inst;

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    set_dest(inst, &mut expected, input);
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn load8bit_immediate() {
        fn set_dest(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match inst {
                0x06 => ctx.cpu_mut().regs.b = val,
                0x0e => ctx.cpu_mut().regs.c = val,
                0x16 => ctx.cpu_mut().regs.d = val,
                0x1e => ctx.cpu_mut().regs.e = val,
                0x26 => ctx.cpu_mut().regs.h = val,
                0x2e => ctx.cpu_mut().regs.l = val,
                0x36 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                0x3e => ctx.cpu_mut().regs.acc = val,
                _ => unreachable!(),
            }
        }
        for inst in (0x00..=0x30)
            .step_by(0x10)
            .flat_map(|high| [high + 0x06, high + 0x0e])
        {
            for input in 0..=0xff {
                let mut ctx = (Gbz80State::default(), [0u8; 3]);
                // Start with HL at a high number so any single-byte change to it will never
                // collide with the instruction at 0x00.
                ctx.0.regs.set_hl(2);
                ctx.1[0] = inst;
                ctx.1[1] = input;

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 2;
                    set_dest(inst, &mut expected, input);
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn add8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0x80..=0x86 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("add({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let (sum, wrapped) = v1.overflowing_add(v2);
                            expected.0.regs.acc = sum;
                            expected.0.regs.flags = Flags::empty();
                            if sum == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            if wrapped {
                                expected.0.regs.flags |= Flags::CARRY;
                            }
                            if (v1 & 0xf) + (v2 & 0xf) > 0xf {
                                expected.0.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            // A,u8
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("add(c6) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xc6;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let (sum, wrapped) = v1.overflowing_add(v2);
                        expected.0.regs.acc = sum;
                        expected.0.regs.flags = Flags::empty();
                        if sum == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        if wrapped {
                            expected.0.regs.flags |= Flags::CARRY;
                        }
                        if (v1 & 0xf) + (v2 & 0xf) > 0xf {
                            expected.0.regs.flags |= Flags::HALFCARRY;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            // A,A
            for v in 0..=0xff {
                println!("add(87) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0x87;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    let (sum, wrapped) = v.overflowing_add(v);
                    expected.0.regs.acc = sum;
                    expected.0.regs.flags = Flags::empty();
                    if sum == 0 {
                        expected.0.regs.flags |= Flags::ZERO;
                    }
                    if wrapped {
                        expected.0.regs.flags |= Flags::CARRY;
                    }
                    if (v & 0xf) + (v & 0xf) > 0xf {
                        expected.0.regs.flags |= Flags::HALFCARRY;
                    }
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn adc8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            let carry = (existing_flags & 0x10) >> 4;
            for inst in 0x88..=0x8e {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("adc({:02x}) {},{} {}", inst, v1, v2, carry);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let (sum1, wrapped1) = v1.overflowing_add(v2);
                            let (sum2, wrapped2) = sum1.overflowing_add(carry);
                            expected.0.regs.acc = sum2;
                            expected.0.regs.flags = Flags::empty();
                            if sum2 == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            if wrapped1 || wrapped2 {
                                expected.0.regs.flags |= Flags::CARRY;
                            }
                            if (v1 & 0xf) + (v2 & 0xf) + carry > 0xf {
                                expected.0.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            // ADC A,u8
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("adc(ce) {},{} {}", v1, v2, carry);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xce;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let (sum1, wrapped1) = v1.overflowing_add(v2);
                        let (sum2, wrapped2) = sum1.overflowing_add(carry);
                        expected.0.regs.acc = sum2;
                        expected.0.regs.flags = Flags::empty();
                        if sum2 == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        if wrapped1 || wrapped2 {
                            expected.0.regs.flags |= Flags::CARRY;
                        }
                        if (v1 & 0xf) + (v2 & 0xf) + carry > 0xf {
                            expected.0.regs.flags |= Flags::HALFCARRY;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            // ADC A,A
            for v in 0..=0xff {
                println!("adc(8f) {},{} {}", v, v, carry);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0x8f;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                if carry == 1 {
                    ctx.0.regs.flags |= Flags::CARRY;
                }

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    let (sum1, wrapped1) = v.overflowing_add(v);
                    let (sum2, wrapped2) = sum1.overflowing_add(carry);
                    expected.0.regs.acc = sum2;
                    expected.0.regs.flags = Flags::empty();
                    if sum2 == 0 {
                        expected.0.regs.flags |= Flags::ZERO;
                    }
                    if wrapped1 || wrapped2 {
                        expected.0.regs.flags |= Flags::CARRY;
                    }
                    if (v & 0xf) + (v & 0xf) + carry > 0xf {
                        expected.0.regs.flags |= Flags::HALFCARRY;
                    }
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn sub8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0x90..=0x96 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("sub({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let diff = v1.wrapping_sub(v2);
                            expected.0.regs.acc = diff;
                            expected.0.regs.flags = Flags::SUB;
                            if diff == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            if v1 < v2 {
                                expected.0.regs.flags |= Flags::CARRY;
                            }
                            if (v1 & 0xf) < (v2 & 0xf) {
                                expected.0.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("sub(d6) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xd6;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let diff = v1.wrapping_sub(v2);
                        expected.0.regs.acc = diff;
                        expected.0.regs.flags = Flags::SUB;
                        if diff == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        if v1 < v2 {
                            expected.0.regs.flags |= Flags::CARRY;
                        }
                        if (v1 & 0xf) < (v2 & 0xf) {
                            expected.0.regs.flags |= Flags::HALFCARRY;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("sub(97) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0x97;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.acc = 0;
                    expected.0.regs.flags = Flags::ZERO | Flags::SUB;
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn sbc8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            let carry = (existing_flags & 0x10) >> 4;
            for inst in 0x98..=0x9e {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("sbc({:02x}) {},{} {}", inst, v1, v2, carry);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let diff = v1.wrapping_sub(v2).wrapping_sub(carry);
                            expected.0.regs.acc = diff;
                            expected.0.regs.flags = Flags::SUB;
                            if diff == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            if v1 < v2 || (v1 == v2 && carry != 0) {
                                expected.0.regs.flags |= Flags::CARRY;
                            }
                            if (v1 & 0xf) < (v2 & 0xf) || ((v1 & 0xf) == (v2 & 0xf) && carry != 0) {
                                expected.0.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("sbc(de) {},{} {}", v1, v2, carry);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xde;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let diff = v1.wrapping_sub(v2).wrapping_sub(carry);
                        expected.0.regs.acc = diff;
                        expected.0.regs.flags = Flags::SUB;
                        if diff == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        if v1 < v2 || (v1 == v2 && carry != 0) {
                            expected.0.regs.flags |= Flags::CARRY;
                        }
                        if (v1 & 0xf) < (v2 & 0xf) || ((v1 & 0xf) == (v2 & 0xf) && carry != 0) {
                            expected.0.regs.flags |= Flags::HALFCARRY;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("sbc(9f) {},{} {}", v, v, carry);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0x9f;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    if carry == 0 {
                        expected.0.regs.acc = 0;
                        expected.0.regs.flags = Flags::ZERO | Flags::SUB;
                    } else {
                        expected.0.regs.acc = 0xff;
                        expected.0.regs.flags = Flags::SUB | Flags::CARRY | Flags::HALFCARRY;
                    }
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn and8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xa0..=0xa6 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("and({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let res = v1 & v2;
                            expected.0.regs.acc = res;
                            expected.0.regs.flags = Flags::HALFCARRY;
                            if res == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("and(e6) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xe6;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let res = v1 & v2;
                        expected.0.regs.acc = res;
                        expected.0.regs.flags = Flags::HALFCARRY;
                        if res == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("and(a7) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0xa7;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.flags = Flags::HALFCARRY;
                    if v == 0 {
                        expected.0.regs.flags |= Flags::ZERO;
                    }
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn xor8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xa8..=0xae {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("xor({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let res = v1 ^ v2;
                            expected.0.regs.acc = res;
                            expected.0.regs.flags = Flags::empty();
                            if res == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("xor(ee) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xee;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let res = v1 ^ v2;
                        expected.0.regs.acc = res;
                        expected.0.regs.flags = Flags::empty();
                        if res == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("and(af) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0xaf;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.acc = 0;
                    expected.0.regs.flags = Flags::ZERO;
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn or8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xb0..=0xb6 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("or({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let res = v1 | v2;
                            expected.0.regs.acc = res;
                            expected.0.regs.flags = Flags::empty();
                            if res == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("or(f6) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xf6;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let res = v1 | v2;
                        expected.0.regs.acc = res;
                        expected.0.regs.flags = Flags::empty();
                        if res == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("or(b7) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0xb7;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.flags = Flags::empty();
                    if v == 0 {
                        expected.0.regs.flags |= Flags::ZERO;
                    }
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn cp8bit() {
        fn set_source(inst: u8, ctx: &mut impl CpuContext, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xb8..=0xbe {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("cp({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (Gbz80State::default(), [0u8; 2]);
                        ctx.1[0] = inst;
                        ctx.0.regs.acc = v1;
                        ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                        ctx.0.regs.set_hl(1);
                        set_source(inst, &mut ctx, v2);

                        let expected = {
                            let mut expected = ctx.clone();
                            expected.0.regs.pc = 1;
                            let diff = v1.wrapping_sub(v2);
                            expected.0.regs.flags = Flags::SUB;
                            if diff == 0 {
                                expected.0.regs.flags |= Flags::ZERO;
                            }
                            if v1 < v2 {
                                expected.0.regs.flags |= Flags::CARRY;
                            }
                            if (v1 & 0xf) < (v2 & 0xf) {
                                expected.0.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        tick(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("cp(fe) {},{}", v1, v2);
                    let mut ctx = (Gbz80State::default(), [0u8; 2]);
                    ctx.1[0] = 0xfe;
                    ctx.0.regs.acc = v1;
                    ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);
                    ctx.1[1] = v2;

                    let expected = {
                        let mut expected = ctx.clone();
                        expected.0.regs.pc = 2;
                        let diff = v1.wrapping_sub(v2);
                        expected.0.regs.flags = Flags::SUB;
                        if diff == 0 {
                            expected.0.regs.flags |= Flags::ZERO;
                        }
                        if v1 < v2 {
                            expected.0.regs.flags |= Flags::CARRY;
                        }
                        if (v1 & 0xf) < (v2 & 0xf) {
                            expected.0.regs.flags |= Flags::HALFCARRY;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("cp(bf) {},{}", v, v);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.1[0] = 0xbf;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.flags = Flags::ZERO | Flags::SUB;
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn relative_jumps() {
        for flags in (0x00..=0xf0).step_by(0x10) {
            for offset in -128i8..=127 {
                let jnz = (flags & 0x80) == 0;
                let jz = (flags & 0x80) != 0;
                let jnc = (flags & 0x10) == 0;
                let jc = (flags & 0x10) != 0;
                let ops = [
                    (0x18, true),
                    (0x20, jnz),
                    (0x28, jz),
                    (0x30, jnc),
                    (0x38, jc),
                ];

                for (opcode, should_jump) in ops {
                    println!("jr({:02x}) -> {} should: {}", opcode, offset, should_jump);
                    let mut ctx = (Gbz80State::default(), [0u8; 0x200]);
                    ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                    ctx.1[0x100] = opcode;
                    ctx.0.regs.pc = 0x100;
                    ctx.1[0x101] = offset as u8;

                    let expected = {
                        let mut expected = ctx.clone();
                        if should_jump {
                            expected.0.regs.pc = (0x102 + offset as i32) as u16;
                        } else {
                            expected.0.regs.pc = 0x102;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn absolute_immediate_jumps() {
        for flags in (0x00..=0xf0).step_by(0x10) {
            for dest in 0x0000u16..=0xffff {
                let jnz = (flags & 0x80) == 0;
                let jz = (flags & 0x80) != 0;
                let jnc = (flags & 0x10) == 0;
                let jc = (flags & 0x10) != 0;
                let ops = [
                    (0xc3, true),
                    (0xc2, jnz),
                    (0xca, jz),
                    (0xd2, jnc),
                    (0xda, jc),
                ];

                for (opcode, should_jump) in ops {
                    println!("jp({:02x}) -> {:04x} should: {}", opcode, dest, should_jump);
                    let mut ctx = (Gbz80State::default(), [0u8; 3]);
                    ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                    ctx.1[0] = opcode;
                    let [low, high] = dest.to_le_bytes();
                    ctx.1[1] = low;
                    ctx.1[2] = high;

                    let expected = {
                        let mut expected = ctx.clone();
                        if should_jump {
                            expected.0.regs.pc = dest;
                        } else {
                            expected.0.regs.pc = 3;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn jump_hl() {
        for flags in (0x00..=0xf0).step_by(0x10) {
            for dest in 0x0000u16..=0xffff {
                println!("jp(e9) -> {:04x}", dest);
                let mut ctx = (Gbz80State::default(), [0u8; 1]);
                ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                ctx.1[0] = 0xe9;
                ctx.0.regs.set_hl(dest);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = dest;
                    expected
                };

                tick(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn returns() {
        for flags in (0x00..=0xf0).step_by(0x10) {
            for dest in 0x0000u16..=0xffff {
                let jnz = (flags & 0x80) == 0;
                let jz = (flags & 0x80) != 0;
                let jnc = (flags & 0x10) == 0;
                let jc = (flags & 0x10) != 0;
                let ops = [
                    (0xc9, true),
                    (0xc0, jnz),
                    (0xc8, jz),
                    (0xd0, jnc),
                    (0xd8, jc),
                ];

                for (opcode, should_jump) in ops {
                    println!(
                        "ret({:02x}) -> {:04x} should: {}",
                        opcode, dest, should_jump
                    );
                    let mut ctx = (Gbz80State::default(), [0u8; 0x200]);
                    ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                    ctx.1[0x100] = opcode;
                    ctx.0.regs.pc = 0x100;
                    let [low, high] = dest.to_le_bytes();
                    ctx.1[0x1fe] = low;
                    ctx.1[0x1ff] = high;
                    ctx.0.regs.sp = 0x1fe;

                    let expected = {
                        let mut expected = ctx.clone();
                        if should_jump {
                            expected.0.regs.sp = 0x200;
                            expected.0.regs.pc = dest;
                        } else {
                            expected.0.regs.pc = 0x101;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn calls() {
        for flags in (0x00..=0xf0).step_by(0x10) {
            for dest in 0x0000u16..=0xffff {
                let jnz = (flags & 0x80) == 0;
                let jz = (flags & 0x80) != 0;
                let jnc = (flags & 0x10) == 0;
                let jc = (flags & 0x10) != 0;
                let ops = [
                    (0xcd, true),
                    (0xc4, jnz),
                    (0xcc, jz),
                    (0xd4, jnc),
                    (0xdc, jc),
                ];

                for (opcode, should_jump) in ops {
                    println!(
                        "call({:02x}) -> {:04x} should: {}",
                        opcode, dest, should_jump
                    );
                    let mut ctx = (Gbz80State::default(), [0u8; 0x200]);
                    ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                    ctx.1[0x100] = opcode;
                    ctx.0.regs.pc = 0x100;
                    let [low, high] = dest.to_le_bytes();
                    ctx.1[0x101] = low;
                    ctx.1[0x102] = high;
                    ctx.0.regs.sp = 0x200;

                    let expected = {
                        let mut expected = ctx.clone();
                        if should_jump {
                            expected.0.regs.sp = 0x1fe;
                            // Return to 0x0103.
                            expected.1[0x1fe] = 0x03;
                            expected.1[0x1ff] = 0x01;
                            expected.0.regs.pc = dest;
                        } else {
                            expected.0.regs.pc = 0x103;
                        }
                        expected
                    };

                    tick(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }
}
