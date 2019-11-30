use crate::memdev::MemDevice;

use std::num::Wrapping;

union Regs816 {
  regs8: [u8;8],
  regs16: [u16;4],
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

pub fn tick(cpustate: &mut Gbz80state, mmu: &mut MemDevice) -> u64 {
  println!("tick: pc @ 0x{:X}", cpustate.regs.pc);
  let opcode = mmu.read(cpustate.regs.pc.0);
  cpustate.regs.pc += Wrapping(1u16);
  match dispatch(cpustate, mmu, opcode) {
    None => {println!("Unknown opcode 0x{:X}. Skipping.", opcode); 0},
    Some(cycles) => cycles,
  }
}

fn dispatch(cpustate: &mut Gbz80state, mmu: &mut MemDevice, opcode: u8) -> Option<u64> {
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
           0o000 => match y {
                      0 => Some(nop()),
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

fn push16(cpustate: &mut Gbz80state, mmu: &mut MemDevice, value: u16) {
  cpustate.regs.sp -= Wrapping(1u16);
  mmu.write(cpustate.regs.sp.0, (value >> 8) as u8);
  cpustate.regs.sp -= Wrapping(1u16);
  mmu.write(cpustate.regs.sp.0, (value & 0xF) as u8);
}

fn nop() -> u64 {
  4
}

fn rst(cpustate: &mut Gbz80state, mmu: &mut MemDevice, vec: u8) -> u64 {
  push16(cpustate, mmu, cpustate.regs.pc.0);
  cpustate.regs.pc = Wrapping(vec as u16 * 8);
  16
}
