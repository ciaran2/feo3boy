use crate::memdev::MemDevice;

use std::num::Wrapping;

pub mod opcode;

const FZERO: u8 = 0x80;
const FSUB: u8 = 0x40;
const FHALFCARRY: u8 = 0x20;
const FCARRY: u8 = 0x10;
const FNONE: u8 = 0x00;
const FALL: u8 = 0xf0;

/// CPU registers.
#[derive(Debug)]
struct Regs816 {
    /// 8bit registers.
    regs8: [u8; 8],
}
impl Regs816 {
    /// Read the 8-bit register with the specified register number.
    fn read8(&self, regnum: u8) -> u8 {
        self.regs8[regnum as usize]
    }

    /// Write the 8-bit register with the specified register number.
    fn write8(&mut self, regnum: u8, value: u8) {
        self.regs8[regnum as usize] = value
    }

    /// Read the 16-bit register with the specified register number.
    fn read16(&self, regnum: u8) -> u16 {
        let (mut highreg, mut lowreg) = (regnum << 1, regnum << 1);
        if regnum == 0b11 {
            highreg += 1;
        } else {
            lowreg += 1;
        }

        (self.regs8[highreg as usize] as u16) << 8 + self.regs8[lowreg as usize] as u16
    }

    fn write16(&mut self, regnum: u8, value: u16) {
        let (mut highreg, mut lowreg) = (regnum << 1, regnum << 1);
        if regnum == 0b11 {
            highreg += 1;
        } else {
            lowreg += 1;
        }

        self.regs8[highreg as usize] = (value >> 8) as u8;
        self.regs8[lowreg as usize] = (value & 0xff) as u8;
    }
}
impl Default for Regs816 {
    fn default() -> Regs816 {
        Regs816 { regs8: [0; 8] }
    }
}

#[derive(Default)]
struct Regs {
    regs816: Regs816,
    sp: Wrapping<u16>,
    pc: Wrapping<u16>,
}

#[derive(Default)]
pub struct Gbz80state {
    regs: Regs,
    halted: bool,
}
impl Gbz80state {
    pub fn new() -> Gbz80state {
        Gbz80state {
            halted: false,
            ..Default::default()
        }
    }
}

pub fn tick(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice) -> u64 {
    println!("tick: pc @ 0x{:X}", cpustate.regs.pc);
    let opcode = pcload(cpustate, mmu);
    match dispatch(cpustate, mmu, opcode) {
        None => {
            println!("Unknown opcode 0x{:X}. Skipping.", opcode);
            0
        }
        Some((cycles, flags, flagmask)) => {
            let mut wip_flags = cpustate.regs.regs816.read8(0b110);
            wip_flags &= !flagmask;
            wip_flags |= flags;
            cpustate.regs.regs816.write8(0b110, wip_flags);
            cycles
        }
    }
}

fn dispatch(
    cpustate: &mut Gbz80state,
    mmu: &mut impl MemDevice,
    opcode: u8,
) -> Option<(u64, u8, u8)> {
    let z = opcode & 7;
    let opcode = opcode >> 3;
    let y = opcode & 7;
    let q = opcode & 1;
    let opcode = opcode >> 1;
    let p = opcode & 3;
    let opcode = opcode >> 2;
    let x = opcode & 3;

    match x {
        0 => match z {
            0 => match y {
                0 => Some(nop()),
                _ => None,
            },
            1 => match q {
                0 => Some(load16imm(cpustate, mmu, p)),
                1 => None,
                _ => None,
            },
            6 => Some(load8imm(cpustate, mmu, y)),
            _ => None,
        },
        1 => {
            if 0o6 == y {
                None
            } else {
                Some(load8reg(cpustate, mmu, y, z))
            }
        }
        2 => match y {
            0o0 => Some(add(cpustate, mmu, z)),
            0o1 => Some(addc(cpustate, mmu, z)),
            0o2 => Some(sub(cpustate, mmu, z)),
            0o3 => Some(subc(cpustate, mmu, z)),
            0o4 => Some(and(cpustate, mmu, z)),
            0o5 => Some(xor(cpustate, mmu, z)),
            0o6 => Some(or(cpustate, mmu, z)),
            0o7 => Some(cp(cpustate, mmu, z)),
            _ => None,
        },
        3 => match z {
            0 => match y {
                _ => None,
            },
            7 => Some(rst(cpustate, mmu, y)),
            _ => None,
        },
        _ => None,
    }
}

/*
 * Utility functions
 */

/**
 ** Memory tasks
 **/
fn pcload(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice) -> u8 {
    let value = mmu.read(cpustate.regs.pc.0.into());
    cpustate.regs.pc += Wrapping(1u16);
    value
}

fn push16(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, value: u16) {
    cpustate.regs.sp -= Wrapping(1u16);
    mmu.write(cpustate.regs.sp.0.into(), (value >> 8) as u8);
    cpustate.regs.sp -= Wrapping(1u16);
    mmu.write(cpustate.regs.sp.0.into(), (value & 0xFF) as u8);
}

fn pop16(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice) -> u16 {
    let mut ret = 0u16;
    ret |= mmu.read(cpustate.regs.sp.0.into()) as u16;
    cpustate.regs.sp += Wrapping(1u16);
    ret |= (mmu.read(cpustate.regs.sp.0.into()) as u16) << 8;
    cpustate.regs.sp += Wrapping(1u16);
    ret
}

/**
 ** Register tasks
 **/
fn reg_fetch8(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u8, u64) {
    if regnum == 0o6 {
        (mmu.read(cpustate.regs.regs816.read16(0b10).into()), 4)
    } else {
        (cpustate.regs.regs816.read8(regnum), 0)
    }
}

fn reg_write8(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8, value: u8) -> u64 {
    if regnum == 0o6 {
        mmu.write(cpustate.regs.regs816.read16(0b10).into(), value);
        4
    } else {
        cpustate.regs.regs816.write8(regnum, value);
        0
    }
}

fn check_flag(cpustate: &Gbz80state, flag: u8) -> bool {
    0 != flag & cpustate.regs.regs816.read8(0b110)
}

/*
 * Instruction implementations
 */
fn nop() -> (u64, u8, u8) {
    (4, FNONE, FNONE)
}

fn load16imm(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, p: u8) -> (u64, u8, u8) {
    let toload = pcload(cpustate, mmu) as u16 + (pcload(cpustate, mmu) as u16) << 8;

    match p {
        0..=2 => cpustate.regs.regs816.write16(p, toload),
        3 => cpustate.regs.sp = Wrapping(toload),
        _ => panic!("Error in instruction decoding at load16imm"),
    }

    (12, FNONE, FNONE)
}

fn load8imm(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let toload = pcload(cpustate, mmu);

    let cycle_offset = reg_write8(cpustate, mmu, regnum, toload);

    (8 + cycle_offset, FNONE, FNONE)
}

fn load8reg(
    cpustate: &mut Gbz80state,
    mmu: &mut impl MemDevice,
    toregnum: u8,
    fromregnum: u8,
) -> (u64, u8, u8) {
    let (toload, cycle_offset1) = reg_fetch8(cpustate, mmu, fromregnum);
    let cycle_offset2 = reg_write8(cpustate, mmu, toregnum, toload);
    (4 + cycle_offset1 + cycle_offset2, FNONE, FNONE)
}

fn add(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = a as u16 + reg as u16;

    let mut flags = FNONE;

    if 0 == result {
        flags |= FZERO;
    }
    if 0 != (result & 0xff) {
        flags |= FCARRY;
    }
    if 0 != (((a & 0xf) + (reg & 0xf)) & 0x10) {
        flags |= FHALFCARRY;
    }

    cpustate.regs.regs816.write8(0o7, (result & 0xff) as u8);

    (4 + cycle_offset, flags, FALL)
}

fn addc(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = a as u16 + reg as u16 + if check_flag(cpustate, FCARRY) { 1 } else { 0 };

    let mut flags = FNONE;

    if 0 == result {
        flags |= FZERO;
    }
    if 0 != (result & 0xff) {
        flags |= FCARRY;
    }
    if 0 != (((a & 0xf) + (reg & 0xf)) & 0x10) {
        flags |= FHALFCARRY;
    }

    cpustate.regs.regs816.write8(0o7, (result & 0xff) as u8);

    (4 + cycle_offset, flags, FALL)
}

fn sub(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = (Wrapping(a) - Wrapping(reg)).0;

    let mut flags = FSUB;

    if 0 == result {
        flags |= FZERO;
    }
    if reg > a {
        flags |= FCARRY;
    }
    if (reg & 0xf) > (a & 0xf) {
        flags |= FHALFCARRY;
    }

    cpustate.regs.regs816.write8(0o7, result);

    (4 + cycle_offset, flags, FALL)
}

fn subc(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = (Wrapping(a)
        - Wrapping(reg)
        - Wrapping(if check_flag(cpustate, FCARRY) {
            1u8
        } else {
            0u8
        }))
    .0;

    let mut flags = FSUB;

    if 0 == result {
        flags |= FZERO;
    }
    if reg > a {
        flags |= FCARRY;
    }
    if (reg & 0xf) > (a & 0xf) {
        flags |= FHALFCARRY;
    }

    cpustate.regs.regs816.write8(0o7, result);

    (4 + cycle_offset, flags, FALL)
}

fn and(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = a & reg;
    let flags = if 0 == result {
        FZERO | FHALFCARRY
    } else {
        FHALFCARRY
    };
    cpustate.regs.regs816.write8(0o7, result);
    (4 + cycle_offset, flags, FALL)
}

fn xor(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = a ^ reg;
    let flags = if 0 == result { FZERO } else { FNONE };
    cpustate.regs.regs816.write8(0o7, result);
    (4 + cycle_offset, flags, FALL)
}

fn or(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let result = a | reg;
    let flags = if 0 == result { FZERO } else { FNONE };
    cpustate.regs.regs816.write8(0o7, result);
    (4 + cycle_offset, flags, FALL)
}

fn cp(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, regnum: u8) -> (u64, u8, u8) {
    let a = cpustate.regs.regs816.read8(0o7);
    let (reg, cycle_offset) = reg_fetch8(cpustate, mmu, regnum);

    let mut flags = FSUB;

    if a == reg {
        flags |= FZERO;
    }
    if reg > a {
        flags |= FCARRY;
    }
    if (reg & 0xf) > (a & 0xf) {
        flags |= FHALFCARRY;
    }

    (4 + cycle_offset, flags, FALL)
}

fn rst(cpustate: &mut Gbz80state, mmu: &mut impl MemDevice, vec: u8) -> (u64, u8, u8) {
    push16(cpustate, mmu, cpustate.regs.pc.0);
    cpustate.regs.pc = Wrapping(vec as u16 * 8);
    (16, FNONE, FNONE)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_loads_and_alu() {
        let mut cpustate: Gbz80state = Gbz80state::new();
        let mut testmem: [u8; 0x10000] = [0u8; 0x10000];

        testmem[0] = 0x3e;
        testmem[1] = 0x80;
        testmem[2] = 0x06;
        testmem[3] = 0x01;
        testmem[4] = 0x80;
        testmem[5] = 0x0e;
        testmem[6] = 0x85;
        testmem[7] = 0x81;

        tick(&mut cpustate, &mut testmem);

        assert_eq!(
            cpustate.regs.regs816.read8(0b111),
            0x80,
            "{:?}",
            cpustate.regs.regs816
        );

        tick(&mut cpustate, &mut testmem);

        assert_eq!(
            cpustate.regs.regs816.read8(0x0),
            0x01,
            "{:?}",
            cpustate.regs.regs816
        );

        tick(&mut cpustate, &mut testmem);

        assert_eq!(
            cpustate.regs.regs816.read8(0b111),
            0x81,
            "{:?}",
            cpustate.regs.regs816
        );

        tick(&mut cpustate, &mut testmem);

        assert_eq!(
            cpustate.regs.regs816.read8(0b001),
            0x85,
            "{:?}",
            cpustate.regs.regs816
        );

        tick(&mut cpustate, &mut testmem);

        assert_eq!(
            cpustate.regs.regs816.read8(0b111),
            0x6,
            "{:?}",
            cpustate.regs.regs816
        );
        assert_eq!(
            check_flag(&cpustate, FCARRY),
            true,
            "{:?}",
            cpustate.regs.regs816
        );
    }
}
