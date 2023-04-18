use std::{iter, mem};

use feo3boy::gbz80core::direct_executor::DirectExecutor;
use feo3boy::gbz80core::executor::Executor;
use feo3boy::gbz80core::Gbz80State;
use feo3boy::memdev::{AllRam, RootMemDevice};

#[test]
fn fibonacci() {
    const OUTPUT: u16 = 0xC000;

    let mut mem = AllRam::from(include_bytes!("fibonacci.bin"));
    let mut cpu = Gbz80State::default();
    while !cpu.halted {
        DirectExecutor::run_single_instruction(&mut (&mut cpu, &mut mem));
    }

    let (mut f1, mut f2) = (0, 1);
    for (i, fib) in iter::from_fn(move || {
        let res = f1;
        f1 += f2;
        mem::swap(&mut f1, &mut f2);
        Some(res)
    })
    .enumerate()
    {
        if fib > u8::MAX as u32 {
            break;
        }
        assert_eq!(mem.read_byte(OUTPUT + i as u16), fib as u8);
    }
}

#[test]
fn fibonacci16() {
    const OUTPUT: u16 = 0xC000;

    let mut mem = AllRam::from(include_bytes!("fibonacci16.bin"));
    let mut cpu = Gbz80State::default();
    while !cpu.halted {
        DirectExecutor::run_single_instruction(&mut (&mut cpu, &mut mem));
    }

    let (mut f1, mut f2) = (0u32, 1);
    for (i, fib) in iter::from_fn(move || {
        let res = f1;
        f1 += f2;
        mem::swap(&mut f1, &mut f2);
        Some(res)
    })
    .enumerate()
    {
        if fib > u16::MAX as u32 {
            break;
        }
        let val: u16 = mem.read(OUTPUT + i as u16 * 2);
        assert_eq!(val, fib as u16);
    }
}

#[test]
fn squares() {
    const OUTPUT: u16 = 0xC000;

    let mut mem = AllRam::from(include_bytes!("squares.bin"));
    let mut cpu = Gbz80State::default();
    while !cpu.halted {
        DirectExecutor::run_single_instruction(&mut (&mut cpu, &mut mem));
    }

    for (i, square) in (1..).map(|x| x * x).enumerate() {
        if square > u8::MAX as u32 {
            break;
        }
        assert_eq!(mem.read_byte(OUTPUT + i as u16), square as u8);
    }
}

/// Tests which depend on the cpu_instrs.gb test rom. cpu_instrs is not included in the
/// repository for licensing reasons. Copy it to this folder before trying to run these
/// tests.
#[cfg(feature = "test-roms")]
mod test_roms {
    use std::ops::ControlFlow;

    use feo3boy::gb::Gb;
    use feo3boy::gbz80core::direct_executor::DirectExecutor;
    use feo3boy::gbz80core::executor::Executor;
    use feo3boy::gbz80core::microcode_executor::MicrocodeExecutor;
    use feo3boy::memdev::{BiosRom, Cartridge, RootMemDevice};
    use feo3boy_opcodes::opcode::Opcode;

    /// Checks if output matches the cpu_instrs output. Returns break when at the end of
    /// the passing output.
    fn check_cpu_instrs_output(output: &str) -> ControlFlow<()> {
        // There are two extra spaces on the end of the line of test outputs, which we
        // make clear by escaping them as \u{0020}
        const EXPECTED: &'static str = "cpu_instrs

01:ok  02:ok  03:ok  04:ok  05:ok  06:ok  07:ok  08:ok  09:ok  10:ok  11:ok\u{0020}\u{0020}

Passed all tests";
        if output.len() < EXPECTED.len() {
            assert!(
                EXPECTED.starts_with(output),
                "Output was not a prefix of expected.\nExpected:\n{}\n\nOutput:\n{}",
                EXPECTED,
                output
            );
        } else if output.len() == EXPECTED.len() {
            assert_eq!(EXPECTED, output);
            return ControlFlow::Break(());
        } else {
            panic!(
                "Output is longer than expected.\nExpected:\n{}\n\nOutput:\n{}",
                EXPECTED, output
            );
        }
        ControlFlow::Continue(())
    }

    /// Run the "cpu_instrs" cartridge.
    #[test]
    fn cpu_instrs_direct() {
        let cart = Cartridge::parse(&include_bytes!("cpu_instrs.gb")[..]).unwrap();
        let bios = BiosRom::new(*include_bytes!("bios.bin"));

        let mut gb = Box::new(Gb::new(bios.clone(), cart.clone()));
        let mut output = Vec::new();

        loop {
            output.extend(gb.serial.stream.receive_bytes());
            let output = String::from_utf8_lossy(&output);
            if let ControlFlow::Break(()) = check_cpu_instrs_output(output.as_ref()) {
                break;
            }
            DirectExecutor::run_single_instruction(&mut *gb);
        }
    }

    /// Run the "cpu_instrs" cartridge.
    #[test]
    fn cpu_instrs_microcode() {
        let cart = Cartridge::parse(&include_bytes!("cpu_instrs.gb")[..]).unwrap();
        let bios = BiosRom::new(*include_bytes!("bios.bin"));

        let mut gb = Box::new(Gb::new_microcode(bios.clone(), cart.clone()));
        let mut output = Vec::new();

        loop {
            output.extend(gb.serial.stream.receive_bytes());
            let output = String::from_utf8_lossy(&output);
            if let ControlFlow::Break(()) = check_cpu_instrs_output(output.as_ref()) {
                break;
            }
            MicrocodeExecutor::run_single_instruction(&mut *gb);
        }
    }

    /// Run the "cpu_instrs" comparing state between microcode and direct execution.
    #[test]
    fn cpu_instrs_comparison() {
        let cart = Cartridge::parse(&include_bytes!("cpu_instrs.gb")[..]).unwrap();
        let bios = BiosRom::new(*include_bytes!("bios.bin"));

        let mut gb_direct = Box::new(Gb::new(bios.clone(), cart.clone()));
        let mut gb_microcode = Box::new(Gb::new_microcode(bios.clone(), cart.clone()));
        let mut output_direct = Vec::new();
        let mut output_microcode = Vec::new();

        loop {
            output_direct.extend(gb_direct.serial.stream.receive_bytes());
            output_microcode.extend(gb_microcode.serial.stream.receive_bytes());

            let ex_pc = gb_direct.cpustate.regs.pc;
            let instr_direct = Opcode::decode(gb_direct.mmu.read_byte(ex_pc));
            let instr_microcode = Opcode::decode(gb_microcode.mmu.read_byte(ex_pc));
            DirectExecutor::run_single_instruction(&mut *gb_direct);
            MicrocodeExecutor::run_single_instruction(&mut *gb_microcode);
            assert!(
                gb_direct.cpustate == gb_microcode.cpustate,
                "Mismatch after PC {}: {}/{}\nDirect: {:?}\nMicrodode: {:?}",
                ex_pc,
                instr_direct,
                instr_microcode,
                gb_direct.cpustate,
                gb_microcode.cpustate,
            );

            let output_direct = String::from_utf8_lossy(&output_direct);
            let output_microcode = String::from_utf8_lossy(&output_microcode);
            assert_eq!(output_direct, output_microcode);
            if let ControlFlow::Break(()) = check_cpu_instrs_output(output_direct.as_ref()) {
                break;
            }
        }
    }
}
