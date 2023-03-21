//! Prints out all CB-prefixed opcodes in a CSV table for comparison against published opcode
//! tables.

use feo3boy::gbz80core::opcode::CBOpcode;

fn main() {
    for l in 0u8..=0xF {
        print!(",{:#04X}", l);
    }
    println!();
    for h in (0u8..=0xFF).step_by(0x10) {
        print!("{:#04X}", h);
        for l in 0u8..=0xF {
            let opcode = CBOpcode::decode(h + l);
            print!(",\"{}\"", opcode);
        }
        println!();
    }
}
