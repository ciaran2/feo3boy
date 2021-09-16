use std::borrow::BorrowMut;

use bitflags::bitflags;

use crate::memdev::MemDevice;
pub use opcode::{CBOpcode, CBOperation, Opcode};
pub use opcode_args::{AluOp, AluUnaryOp, ConditionCode, Operand16, Operand8};

mod opcode;
mod opcode_args;
mod oputils;

bitflags! {
    /// operation flags set after various operations.
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

impl Default for Flags {
    fn default() -> Flags {
        Flags::empty()
    }
}

#[derive(Default, Debug)]
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
        let sp = self.sp;
        self.sp = sp.wrapping_sub(1);
        sp
    }
}

/// Internal state of the CPU.
#[derive(Default, Debug)]
pub struct Gbz80State {
    /// Cpu registers.
    pub regs: Regs,
    halted: bool,
}

impl Gbz80State {
    /// Create a new Gbz80State.
    pub fn new() -> Gbz80State {
        Default::default()
    }
}

/// Trait which encapsulates everything that the CPU needs in order to execute.
pub trait CpuContext {
    /// Type of MemDevice in this context.
    type Mem: MemDevice;

    /// Gets the CPU state.
    fn cpustate(&self) -> &Gbz80State;

    /// Gets a mutable reference to the CPU state.
    fn cpustate_mut(&mut self) -> &mut Gbz80State;

    /// Gets the memory.
    fn mem(&self) -> &Self::Mem;

    /// Get a mutable reference to the memory.
    fn mem_mut(&mut self) -> &mut Self::Mem;

    /// Yields from CPU execution for 1 M clock cycle (4 T). This callback should step the clock
    /// forward and perform any work that needs to happen faster than instructions execute.
    /// Warning: It is undefined behavior to call `tick` again during a context yield.
    fn yield1m(&mut self);
}

/// Runs a single instruction on the CPU.
pub fn tick<B, C>(mut ctx: B)
// Using BorrowMut here allows both `&mut (Gbz80State, M)` and `(&mut Gbz80State, &mut M)` to be
// passed as the argument.
where
    B: BorrowMut<C>,
    C: CpuContext,
{
    Opcode::load_and_execute(ctx.borrow_mut());
}

// fn push16(cpustate: &mut Gbz80State, mmu: &mut impl MemDevice, value: u16) {
//     cpustate.regs.sp -= Wrapping(1u16);
//     mmu.write(cpustate.regs.sp.0.into(), (value >> 8) as u8);
//     cpustate.regs.sp -= Wrapping(1u16);
//     mmu.write(cpustate.regs.sp.0.into(), (value & 0xFF) as u8);
// }
//
// fn pop16(cpustate: &mut Gbz80State, mmu: &mut impl MemDevice) -> u16 {
//     let mut ret = 0u16;
//     ret |= mmu.read(cpustate.regs.sp.0.into()) as u16;
//     cpustate.regs.sp += Wrapping(1u16);
//     ret |= (mmu.read(cpustate.regs.sp.0.into()) as u16) << 8;
//     cpustate.regs.sp += Wrapping(1u16);
//     ret
// }
// fn load16imm(cpustate: &mut Gbz80State, mmu: &mut impl MemDevice, p: u8) -> (u64, u8, u8) {
//     let toload = pcload(cpustate, mmu) as u16 + (pcload(cpustate, mmu) as u16) << 8;
//
//     match p {
//         0..=2 => cpustate.regs.regs816.write16(p, toload),
//         3 => cpustate.regs.sp = Wrapping(toload),
//         _ => panic!("Error in instruction decoding at load16imm"),
//     }
//
//     (12, FNONE, FNONE)
// }
//
// fn load8imm(cpustate: &mut Gbz80State, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
//     let toload = pcload(cpustate, mmu);
//
//     let cycle_offset = reg_write8(cpustate, mmu, regnum, toload);
//
//     (8 + cycle_offset, FNONE, FNONE)
// }
//
//
// fn rst(cpustate: &mut Gbz80State, mmu: &mut impl MemDevice, vec: u8) -> (u64, u8, u8) {
//     push16(cpustate, mmu, cpustate.regs.pc.0);
//     cpustate.regs.pc = Wrapping(vec as u16 * 8);
//     (16, FNONE, FNONE)
// }

/////////////////////////////////////////
// Utility implementations of CpuContext.
/////////////////////////////////////////

/// Allows a tuple of Gbz80State and any MemDevice to be used as CpuContext.
impl<M: MemDevice> CpuContext for (Gbz80State, M) {
    type Mem = M;

    fn cpustate(&self) -> &Gbz80State {
        &self.0
    }

    fn cpustate_mut(&mut self) -> &mut Gbz80State {
        &mut self.0
    }

    fn mem(&self) -> &Self::Mem {
        &self.1
    }

    fn mem_mut(&mut self) -> &mut Self::Mem {
        &mut self.1
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    fn yield1m(&mut self) {}
}

/// Allows a tuple of references to Gbz80State and any MemDevice to be used as CpuContext.
impl<M: MemDevice> CpuContext for (&mut Gbz80State, &mut M) {
    type Mem = M;

    fn cpustate(&self) -> &Gbz80State {
        self.0
    }

    fn cpustate_mut(&mut self) -> &mut Gbz80State {
        self.0
    }

    fn mem(&self) -> &Self::Mem {
        self.1
    }

    fn mem_mut(&mut self) -> &mut Self::Mem {
        self.1
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    fn yield1m(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    //     #[test]
    //     fn test_loads_and_alu() {
    //         let mut cpustate = Gbz80State::new();
    //         let mut testmem = [0u8; 0x10000];
    //
    //         testmem[0] = 0x3e;
    //         testmem[1] = 0x80;
    //         testmem[2] = 0x06;
    //         testmem[3] = 0x01;
    //         testmem[4] = 0x80;
    //         testmem[5] = 0x0e;
    //         testmem[6] = 0x85;
    //         testmem[7] = 0x81;
    //
    //         tick((&mut cpustate, &mut testmem));
    //
    //         assert_eq!(
    //             cpustate.regs.regs816.read8(0b111),
    //             0x80,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //
    //         tick(&mut cpustate, &mut testmem);
    //
    //         assert_eq!(
    //             cpustate.regs.regs816.read8(0x0),
    //             0x01,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //
    //         tick(&mut cpustate, &mut testmem);
    //
    //         assert_eq!(
    //             cpustate.regs.regs816.read8(0b111),
    //             0x81,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //
    //         tick(&mut cpustate, &mut testmem);
    //
    //         assert_eq!(
    //             cpustate.regs.regs816.read8(0b001),
    //             0x85,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //
    //         tick(&mut cpustate, &mut testmem);
    //
    //         assert_eq!(
    //             cpustate.regs.regs816.read8(0b111),
    //             0x6,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //         assert_eq!(
    //             check_flag(&cpustate, FCARRY),
    //             true,
    //             "{:?}",
    //             cpustate.regs.regs816
    //         );
    //     }
}
