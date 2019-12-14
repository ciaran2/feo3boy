use crate::memdev::MemDevice;

use std::num::Wrapping;

struct Regs816 {
  regs8: [u8;8],
}
impl Regs816 {
  fn read8(&self, regnum: u8) -> u8 {
    self.regs8[regnum as usize]
  }
  fn write8(&mut self, regnum: u8, value: u8) {
    self.regs8[regnum as usize] = value
  }
  fn read16(&self, regnum: u8) -> u16 {
    let (mut highreg, mut lowreg) = (regnum << 1, regnum << 1);
    if regnum == 0b11 {
      highreg += 1;
    }
    else {
      lowreg += 1;
    }

    (self.regs8[highreg as usize] as u16) << 8 + self.regs8[lowreg as usize] as u16
  }
  fn write16(&mut self, regnum: u8, value: u16) {
    let (mut highreg, mut lowreg) = (regnum << 1, regnum << 1);
    if regnum == 0b11 {
      highreg += 1;
    }
    else {
      lowreg += 1;
    }

    self.regs8[highreg as usize] = (value >> 8) as u8;
    self.regs8[lowreg as usize] = (value & 0xff) as u8;
  }
}
impl Default for Regs816 {
    fn default() -> Regs816 {
        Regs816 {
            regs8: [0;8]
        }
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

pub fn tick(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice) -> u64 {
  println!("tick: pc @ 0x{:X}", cpustate.regs.pc);
  let opcode = pcload(cpustate, mmu);
  match dispatch(cpustate, mmu, opcode) {
    None => {println!("Unknown opcode 0x{:X}. Skipping.", opcode); 0},
    Some((cycles, flags, flagmask)) => cycles,
  }
}

fn dispatch(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice, opcode: u8) -> Option<(u64,u8,u8)> {
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
                }
           1 => match q {
                      0 => Some(load16imm(cpustate, mmu, p)),
                      1 => None,
                      _ => None,
                }
           _ => None,
         }
    1 => match z {
           0o000 => match y {
                      _ => None,
                    }
           _ => None,
         }
    2 => match z {
           0 => match y {
                  _ => None,
                }
           _ => None,
         }
    3 => match z {
           0 => match y {
                  _ => None,
                }
           7 => Some(rst(cpustate, mmu, y)),
           _ => None,
         }
    _ => None,
  }
}

/*
 * Utility functions
 */
fn pcload(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice) -> u8 {
  let value = mmu.read(cpustate.regs.pc.0);
  cpustate.regs.pc += Wrapping(1u16);
  value
}

fn push16(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice, value: u16) {
  cpustate.regs.sp -= Wrapping(1u16);
  mmu.write(cpustate.regs.sp.0, (value >> 8) as u8);
  cpustate.regs.sp -= Wrapping(1u16);
  mmu.write(cpustate.regs.sp.0, (value & 0xFF) as u8);
}

fn pop16(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice) -> u16 {
  let mut ret = 0u16;
  ret |= mmu.read(cpustate.regs.sp.0) as u16;
  cpustate.regs.sp += Wrapping(1u16);
  ret |= (mmu.read(cpustate.regs.sp.0) as u16) << 8;
  cpustate.regs.sp += Wrapping(1u16);
  ret
}

/*
 * Instruction implementations
 */
fn nop() -> (u64, u8, u8) {
  (4, 0x0, 0x0)
}

fn load16imm(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice, p: u8) -> (u64, u8, u8) {
  let toload = pcload(cpustate, mmu) as u16 + (pcload(cpustate, mmu) as u16) << 8;

  match p {
    0 ..= 2 => cpustate.regs.regs816.write16(p, toload),
    3 => cpustate.regs.sp = Wrapping(toload),
    _ => panic!("Error in instruction decoding at load16imm"),
  }

  (12, 0x0, 0x0)
}

fn rst(cpustate: &mut Gbz80state, mmu: &mut dyn MemDevice, vec: u8) -> (u64, u8, u8) {
  push16(cpustate, mmu, cpustate.regs.pc.0);
  cpustate.regs.pc = Wrapping(vec as u16 * 8);
  (16, 0x0, 0x0)
}
