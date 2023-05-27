use criterion::{criterion_group, criterion_main, BatchSize, Criterion};
use feo3boy::gbz80core::direct_executor::DirectExecutor;
use feo3boy::gbz80core::direct_executor_v2::DirectExecutorV2;
use feo3boy::gbz80core::executor::Executor;
use feo3boy::gbz80core::microcode_executor::{MicrocodeExecutor, MicrocodeState};
use feo3boy::gbz80core::stepping_executor::{SteppingExecutor, SteppingExecutorState};
use feo3boy::gbz80core::TestGb;
use feo3boy::memdev::AllRam;

fn run_until_halted<E: Executor>(gb: &mut TestGb<AllRam, E::State>) -> (u8, u8, u8, u8) {
    while !gb.cpu.halted {
        E::run_single_instruction(gb);
    }
    (
        gb.mem[0xC000],
        gb.mem[0xC001],
        gb.mem[0xC002],
        gb.mem[0xC003],
    )
}

criterion_group!(basic, opcodes_benchmark);

/// Benchmarks for opcodes.
fn opcodes_benchmark(c: &mut Criterion) {
    let fib = include_bytes!("../tests/fibonacci.bin");
    c.bench_function("fibonacci-direct", |b| {
        b.iter_batched_ref(
            || {
                let mem = AllRam::from(fib);
                TestGb::new(mem, ())
            },
            |gb| run_until_halted::<DirectExecutor>(gb),
            BatchSize::LargeInput,
        )
    });
    c.bench_function("fibonacci-microcode", |b| {
        b.iter_batched_ref(
            || {
                let mem = AllRam::from(fib);
                TestGb::new(mem, MicrocodeState::default())
            },
            |gb| run_until_halted::<MicrocodeExecutor>(gb),
            BatchSize::LargeInput,
        )
    });
    c.bench_function("fibonacci-direct-v2", |b| {
        b.iter_batched_ref(
            || {
                let mem = AllRam::from(fib);
                TestGb::new(mem, ())
            },
            |gb| run_until_halted::<DirectExecutorV2>(gb),
            BatchSize::LargeInput,
        )
    });
    c.bench_function("fibonacci-stepping", |b| {
        b.iter_batched_ref(
            || {
                let mem = AllRam::from(fib);
                TestGb::new(mem, SteppingExecutorState::default())
            },
            |gb| run_until_halted::<SteppingExecutor>(gb),
            BatchSize::LargeInput,
        )
    });
}

#[cfg(feature = "test-roms")]
criterion_group! {
    name = long_running;
    config = Criterion::default()
        //.measurement_time(std::time::Duration::from_secs(240))
        .sample_size(10);
    targets = cartridge_benchmark
}

/// Benchmarks running 1 million instructions in the GB cpu_instrs test rom.
///
/// The cpu_instrs.gb is not included in this repository. You must copy it to the tests
/// directory before uncommenting this benchmark. (Cargo features don't apply to
/// benches apparently).
#[cfg(feature = "test-roms")]
fn cartridge_benchmark(c: &mut Criterion) {
    use feo3boy::gb::Gb;
    use feo3boy::memdev::{BiosRom, Cartridge};
    use std::ops::ControlFlow;

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

    fn run_cpu_instrs<E: Executor>(gb: &mut Gb<E>, output: &mut Vec<u8>) -> String {
        loop {
            output.extend(gb.serial.stream.receive_bytes());
            let output = String::from_utf8_lossy(&output);
            if let ControlFlow::Break(()) = check_cpu_instrs_output(output.as_ref()) {
                break;
            }
            E::run_single_instruction(&mut *gb);
        }
        String::from_utf8_lossy(&output).into_owned()
    }

    let cart = Cartridge::parse(&include_bytes!("../tests/cpu_instrs.gb")[..]).unwrap();
    let bios = BiosRom::new(*include_bytes!("../tests/bios.bin"));

    let mut group = c.benchmark_group("CpuInstrs");
    group.sampling_mode(criterion::SamplingMode::Flat);
    group.bench_function("cpu_instrs-direct", |b| {
        b.iter_batched_ref(
            || (Box::new(Gb::new(bios.clone(), cart.clone())), Vec::new()),
            |(gb, output)| run_cpu_instrs(&mut **gb, output),
            BatchSize::LargeInput,
        );
    });
    group.bench_function("cpu_instrs-microcode", |b| {
        b.iter_batched_ref(
            || {
                (
                    Box::new(Gb::new_microcode(bios.clone(), cart.clone())),
                    Vec::new(),
                )
            },
            |(gb, output)| run_cpu_instrs(&mut **gb, output),
            BatchSize::LargeInput,
        );
    });
    group.bench_function("cpu_instrs-direct-v2", |b| {
        b.iter_batched_ref(
            || (Box::new(Gb::new_v2(bios.clone(), cart.clone())), Vec::new()),
            |(gb, output)| run_cpu_instrs(&mut **gb, output),
            BatchSize::LargeInput,
        );
    });
    group.bench_function("cpu_instrs-stepping", |b| {
        b.iter_batched_ref(
            || {
                (
                    Box::new(Gb::new_stepping(bios.clone(), cart.clone())),
                    Vec::new(),
                )
            },
            |(gb, output)| run_cpu_instrs(&mut **gb, output),
            BatchSize::LargeInput,
        );
    });
}

#[cfg(feature = "test-roms")]
criterion_main!(basic, long_running);

#[cfg(not(feature = "test-roms"))]
criterion_main!(basic);
