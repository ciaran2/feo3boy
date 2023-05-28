use std::{iter, mem};

use feo3boy::gbz80core::direct_executor::DirectExecutor;
use feo3boy::gbz80core::direct_executor_v2::DirectExecutorV2;
use feo3boy::gbz80core::microcode_executor::MicrocodeExecutor;
use feo3boy::gbz80core::stepping_executor::SteppingExecutor;

macro_rules! executor_tests {
    ($executor:ty, $modname:ident) => {
        mod $modname {
            use feo3boy::gbz80core::executor::Executor;
            use feo3boy::gbz80core::TestGb;
            use feo3boy::memdev::AllRam;

            use super::*;

            #[test]
            fn fibonacci() {
                const OUTPUT: usize = 0xC000;

                let mem = AllRam::from(include_bytes!("fibonacci.bin"));
                let state = <$executor as Executor>::State::default();
                let mut gb = TestGb::new(mem, state);
                while !gb.cpu.halted {
                    <$executor>::run_single_instruction(&mut gb);
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
                    assert_eq!(gb.mem[OUTPUT + i], fib as u8);
                }
            }

            #[test]
            fn fibonacci16() {
                const OUTPUT: usize = 0xC000;

                let mem = AllRam::from(include_bytes!("fibonacci16.bin"));
                let state = <$executor as Executor>::State::default();
                let mut gb = TestGb::new(mem, state);
                while !gb.cpu.halted {
                    <$executor>::run_single_instruction(&mut gb);
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
                    let val: u16 =
                        u16::from_le_bytes([gb.mem[OUTPUT + i * 2], gb.mem[OUTPUT + i * 2 + 1]]);
                    assert_eq!(val, fib as u16);
                }
            }

            #[test]
            fn squares() {
                const OUTPUT: usize = 0xC000;

                let mem = AllRam::from(include_bytes!("squares.bin"));
                let state = <$executor as Executor>::State::default();
                let mut gb = TestGb::new(mem, state);
                while !gb.cpu.halted {
                    <$executor>::run_single_instruction(&mut gb);
                }

                for (i, square) in (1..).map(|x| x * x).enumerate() {
                    if square > u8::MAX as u32 {
                        break;
                    }
                    assert_eq!(gb.mem[OUTPUT + i], square as u8);
                }
            }

            #[cfg(feature = "test-roms")]
            mod test_roms {
                use std::ops::ControlFlow;

                use feo3boy::gb::Gb;
                use feo3boy::memdev::{BiosRom, Cartridge};

                use super::*;
                use crate::test_roms::check_cpu_instrs_output;

                /// Run the "cpu_instrs" cartridge.
                #[test]
                fn cpu_instrs() {
                    let cart = Cartridge::parse(&include_bytes!("cpu_instrs.gb")[..]).unwrap();
                    let bios = BiosRom::new(*include_bytes!("bios.bin"));

                    let mut gb =
                        Box::new(Gb::<$executor>::for_executor(bios.clone(), cart.clone()));
                    let mut output = Vec::new();

                    loop {
                        output.extend(gb.serial.stream.receive_bytes());
                        let output = String::from_utf8_lossy(&output);
                        if let ControlFlow::Break(()) = check_cpu_instrs_output(output.as_ref()) {
                            break;
                        }
                        <$executor>::run_single_instruction(&mut *gb);
                    }
                }
            }
        }
    };
}

executor_tests!(DirectExecutor, direct_executor);
executor_tests!(MicrocodeExecutor, microcode_executor);
executor_tests!(DirectExecutorV2, direct_executor_v2);
executor_tests!(SteppingExecutor, stepping_executor);

/// Tests which depend on the cpu_instrs.gb test rom. cpu_instrs is not included in the
/// repository for licensing reasons. Copy it to this folder before trying to run these
/// tests.
#[cfg(feature = "test-roms")]
mod test_roms {
    use std::ops::ControlFlow;

    use feo3boy::gb::Gb;
    use feo3boy::gbz80core::direct_executor::DirectExecutor;
    use feo3boy::gbz80core::direct_executor_v2::DirectExecutorV2;
    use feo3boy::gbz80core::executor::Executor;
    use feo3boy::gbz80core::microcode_executor::MicrocodeExecutor;
    use feo3boy::gbz80core::stepping_executor::SteppingExecutor;
    use feo3boy::memdev::{BiosRom, Cartridge, ReadCtx, RootMemDevice};
    use feo3boy_opcodes::opcode::Opcode;

    /// Checks if output matches the cpu_instrs output. Returns break when at the end of
    /// the passing output.
    pub fn check_cpu_instrs_output(output: &str) -> ControlFlow<()> {
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

    /// Run the "cpu_instrs" comparing state between microcode and direct execution.
    #[test]
    fn cpu_instrs_comparison() {
        let cart = Cartridge::parse(&include_bytes!("cpu_instrs.gb")[..]).unwrap();
        let bios = BiosRom::new(*include_bytes!("bios.bin"));

        let mut gb_direct = Box::new(Gb::new(bios.clone(), cart.clone()));
        let mut gb_microcode = Box::new(Gb::new_microcode(bios.clone(), cart.clone()));
        let mut gb_direct_v2 = Box::new(Gb::new_v2(bios.clone(), cart.clone()));
        let mut gb_stepping = Box::new(Gb::new_stepping(bios.clone(), cart.clone()));
        let mut output_direct = Vec::new();
        let mut output_microcode = Vec::new();
        let mut output_direct_v2 = Vec::new();
        let mut output_stepping = Vec::new();

        loop {
            output_direct.extend(gb_direct.serial.stream.receive_bytes());
            output_microcode.extend(gb_microcode.serial.stream.receive_bytes());
            output_direct_v2.extend(gb_direct_v2.serial.stream.receive_bytes());
            output_stepping.extend(gb_stepping.serial.stream.receive_bytes());

            let readctx = ReadCtx::new(gb_direct.clock().snapshot());
            let ex_pc = gb_direct.cpustate.regs.pc;
            let instr_direct = Opcode::decode(gb_direct.mmu.read_byte(&readctx, ex_pc));
            let instr_microcode = Opcode::decode(gb_microcode.mmu.read_byte(&readctx, ex_pc));
            let instr_direct_v2 = Opcode::decode(gb_direct_v2.mmu.read_byte(&readctx, ex_pc));
            let instr_stepping = Opcode::decode(gb_stepping.mmu.read_byte(&readctx, ex_pc));
            DirectExecutor::run_single_instruction(&mut *gb_direct);
            MicrocodeExecutor::run_single_instruction(&mut *gb_microcode);
            DirectExecutorV2::run_single_instruction(&mut *gb_direct_v2);
            SteppingExecutor::run_single_instruction(&mut *gb_stepping);
            assert!(
                gb_direct.cpustate == gb_microcode.cpustate
                    && gb_direct.cpustate == gb_direct_v2.cpustate
                    && gb_direct.cpustate == gb_stepping.cpustate,
                "Mismatch after PC {}: {}/{}/{}/{}\nDirect: {:?}\nMicrodode: {:?}\n\
                Direct V2: {:?}\nStepping: {:?}",
                ex_pc,
                instr_direct,
                instr_microcode,
                instr_direct_v2,
                instr_stepping,
                gb_direct.cpustate,
                gb_microcode.cpustate,
                gb_direct_v2.cpustate,
                gb_stepping.cpustate,
            );

            let output_direct = String::from_utf8_lossy(&output_direct);
            let output_microcode = String::from_utf8_lossy(&output_microcode);
            let output_direct_v2 = String::from_utf8_lossy(&output_direct_v2);
            let output_stepping = String::from_utf8_lossy(&output_stepping);
            assert_eq!(output_direct, output_microcode);
            assert_eq!(output_direct, output_direct_v2);
            assert_eq!(output_direct, output_stepping);
            if let ControlFlow::Break(()) = check_cpu_instrs_output(output_direct.as_ref()) {
                break;
            }
        }
    }
}
