//! Contains the code-generated stepping executor.

use feo3boy_executor_generator::define_state_executor;

use crate::gbz80core::externdefs;

define_state_executor!(
    /// An executor which allows sub-stepping like the
    /// [MicrocodeExecutor][crate::gbz80core::microcode_executor::MicrocodeExecutor], but
    /// is code generated to use a minimum number of states and less interpretation, so is
    /// much more performant.
    ///
    /// In this executor, states only change whenever there is a
    /// [`Yield`][feo3boy_opcodes::microcode::Microcode::Yield] or
    /// [`FetchNextInstruction`][feo3boy_opcodes::microcode::Microcode::FetchNextInstruction].
    ///
    /// Where the `MicrocodeExecutor` executor uses a [`Vec`] as its stack to store bytes
    /// between microcodes, the microcode stack here exists only at compile time. All
    /// values stored on the microcode stack are instead placed either in rust variables
    /// or in fields of the executor's state.
    pub SteppingExecutor,
    externs = [
        ReadReg => externdefs::read_reg,
        WriteReg => externdefs::write_reg,
        ReadReg16 => externdefs::read_reg16,
        WriteReg16 => externdefs::write_reg16,
        ReadMem => externdefs::read_mem,
        WriteMem => externdefs::write_mem,
        GetFlagsMasked => externdefs::get_flags_masked,
        SetFlagsMasked => externdefs::set_flags_masked,
        Halt => externdefs::halt,
        EnableInterrupts => externdefs::enable_interrupts,
        DisableInterrupts => externdefs::disable_interrupts,
        CheckHalt => externdefs::check_halt,
        ClearHalt => externdefs::clear_halt,
        CheckIme => externdefs::check_ime,
        GetActiveInterrupts => externdefs::get_active_interrupts,
        PopInterrupt => externdefs::pop_interrupt,
        PopHaltBug => externdefs::pop_halt_bug,
    ],
);

/// Tests for the direct_executor
#[cfg(test)]
mod tests {

    use feo3boy_opcodes::gbz80types::Flags;

    use crate::gbz80core::executor::Executor;
    use crate::gbz80core::{ExecutorContext, Gbz80State};
    use crate::memdev::RootMemDevice;

    use super::*;

    /// Convenience type for an ExecutorContext with a unit state.
    trait Ctx: ExecutorContext<State = SteppingExecutorState> {}

    impl<E: ExecutorContext<State = SteppingExecutorState>> Ctx for E {}

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    fn run_single_instruction(ctx: &mut impl Ctx) {
        SteppingExecutor::run_single_instruction(ctx)
    }

    #[test]
    fn test_loads_and_alu() {
        init();

        let mut cpu = Gbz80State::new();
        let mut testmem = [0u8; 0x10000];
        let mut state = SteppingExecutorState::default();

        testmem[0] = 0x3e;
        testmem[1] = 0x80;
        testmem[2] = 0x06;
        testmem[3] = 0x01;
        testmem[4] = 0x80;
        testmem[5] = 0x0e;
        testmem[6] = 0x85;
        testmem[7] = 0x81;

        run_single_instruction(&mut (&mut cpu, &mut testmem, &mut state));
        assert_eq!(cpu.regs.acc, 0x80, "{:?}", cpu.regs);

        run_single_instruction(&mut (&mut cpu, &mut testmem, &mut state));
        assert_eq!(cpu.regs.b, 0x01, "{:?}", cpu.regs);

        run_single_instruction(&mut (&mut cpu, &mut testmem, &mut state));
        assert_eq!(cpu.regs.acc, 0x81, "{:?}", cpu.regs);

        run_single_instruction(&mut (&mut cpu, &mut testmem, &mut state));
        assert_eq!(cpu.regs.c, 0x85, "{:?}", cpu.regs);

        run_single_instruction(&mut (&mut cpu, &mut testmem, &mut state));
        assert_eq!(cpu.regs.acc, 0x6, "{:?}", cpu.regs);
        assert!(cpu.regs.flags.contains(Flags::CARRY), "{:?}", cpu.regs);
    }

    #[test]
    fn load8bit() {
        init();

        fn set_dest(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match inst {
                0x40..=0x47 => ctx.cpu_mut().regs.b = val,
                0x48..=0x4f => ctx.cpu_mut().regs.c = val,
                0x50..=0x57 => ctx.cpu_mut().regs.d = val,
                0x58..=0x5f => ctx.cpu_mut().regs.e = val,
                0x60..=0x67 => ctx.cpu_mut().regs.h = val,
                0x68..=0x6f => ctx.cpu_mut().regs.l = val,
                0x70..=0x77 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                0x78..=0x7f => ctx.cpu_mut().regs.acc = val,
                _ => unreachable!(),
            }
        }
        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                0x7 => ctx.cpu_mut().regs.acc = val,
                _ => unreachable!(),
            }
        }
        for inst in (0x40..=0x7f).filter(|&i| i != 0x76) {
            for input in 0..=0xff {
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 0x10000],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn load8bit_immediate() {
        init();

        fn set_dest(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match inst {
                0x06 => ctx.cpu_mut().regs.b = val,
                0x0e => ctx.cpu_mut().regs.c = val,
                0x16 => ctx.cpu_mut().regs.d = val,
                0x1e => ctx.cpu_mut().regs.e = val,
                0x26 => ctx.cpu_mut().regs.h = val,
                0x2e => ctx.cpu_mut().regs.l = val,
                0x36 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
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
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 3],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn add8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0x80..=0x86 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("add({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            // A,u8
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("add(c6) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            // A,A
            for v in 0..=0xff {
                println!("add(87) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn adc8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
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
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            // ADC A,u8
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("adc(ce) {},{} {}", v1, v2, carry);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            // ADC A,A
            for v in 0..=0xff {
                println!("adc(8f) {},{} {}", v, v, carry);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn sub8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0x90..=0x96 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("sub({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("sub(d6) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("sub(97) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn sbc8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
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
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("sbc(de) {},{} {}", v1, v2, carry);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("sbc(9f) {},{} {}", v, v, carry);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn and8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xa0..=0xa6 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("and({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("and(e6) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("and(a7) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn xor8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xa8..=0xae {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("xor({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("xor(ee) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("and(af) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn or8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xb0..=0xb6 {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("or({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("or(f6) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("or(b7) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
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

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn cp8bit() {
        init();

        fn set_source(inst: u8, ctx: &mut impl Ctx, val: u8) {
            match (inst & 0xf) % 0x8 {
                0x0 => ctx.cpu_mut().regs.b = val,
                0x1 => ctx.cpu_mut().regs.c = val,
                0x2 => ctx.cpu_mut().regs.d = val,
                0x3 => ctx.cpu_mut().regs.e = val,
                0x4 => ctx.cpu_mut().regs.h = val,
                0x5 => ctx.cpu_mut().regs.l = val,
                0x6 => {
                    let dest = ctx.cpu().regs.hl().into();
                    ctx.mem_mut().write_byte(dest, val)
                }
                _ => unreachable!(),
            }
        }
        for existing_flags in (0x00..=0xf0).step_by(0x10) {
            for inst in 0xb8..=0xbe {
                for v1 in 0..=0xff {
                    for v2 in 0..=0xff {
                        println!("cp({:02x}) {},{}", inst, v1, v2);
                        let mut ctx = (
                            Gbz80State::default(),
                            [0u8; 2],
                            SteppingExecutorState::default(),
                        );
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

                        run_single_instruction(&mut ctx);
                        assert_eq!(ctx, expected);
                    }
                }
            }
            for v1 in 0..=0xff {
                for v2 in 0..=0xff {
                    println!("cp(fe) {},{}", v1, v2);
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 2],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
            for v in 0..=0xff {
                println!("cp(bf) {},{}", v, v);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
                ctx.1[0] = 0xbf;
                ctx.0.regs.acc = v;
                ctx.0.regs.flags = Flags::from_bits_truncate(existing_flags);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = 1;
                    expected.0.regs.flags = Flags::ZERO | Flags::SUB;
                    expected
                };

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn relative_jumps() {
        init();

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
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 0x200],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn absolute_immediate_jumps() {
        init();

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
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 3],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn jump_hl() {
        init();

        for flags in (0x00..=0xf0).step_by(0x10) {
            for dest in 0x0000u16..=0xffff {
                println!("jp(e9) -> {:04x}", dest);
                let mut ctx = (
                    Gbz80State::default(),
                    [0u8; 1],
                    SteppingExecutorState::default(),
                );
                ctx.0.regs.flags = Flags::from_bits_truncate(flags);
                ctx.1[0] = 0xe9;
                ctx.0.regs.set_hl(dest);

                let expected = {
                    let mut expected = ctx.clone();
                    expected.0.regs.pc = dest;
                    expected
                };

                run_single_instruction(&mut ctx);
                assert_eq!(ctx, expected);
            }
        }
    }

    #[test]
    fn rst() {
        init();

        let rsts = [
            (0xc7, 0x00),
            (0xcf, 0x08),
            (0xd7, 0x10),
            (0xdf, 0x18),
            (0xe7, 0x20),
            (0xef, 0x28),
            (0xf7, 0x30),
            (0xff, 0x38),
        ];

        for (opcode, dest) in rsts {
            let mut ctx = (
                Gbz80State::default(),
                [0u8; 0x200],
                SteppingExecutorState::default(),
            );
            ctx.1[0x101] = opcode;
            ctx.0.regs.pc = 0x101;
            ctx.0.regs.sp = 0x200;

            let expected = {
                let mut expected = ctx.clone();
                expected.0.regs.pc = dest;
                expected.0.regs.sp = 0x1fe;
                // Pushed address should be 0x0102 (in little endian order), since that's
                // the address of the instruction after RST.
                expected.1[0x1fe] = 0x02;
                expected.1[0x1ff] = 0x01;
                expected
            };

            run_single_instruction(&mut ctx);
            assert_eq!(ctx, expected);
        }
    }

    #[test]
    fn returns() {
        init();

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
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 0x200],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }

    #[test]
    fn interrupt_return() {
        init();

        for dest in 0x0000u16..=0xffff {
            println!("reti(0xd9) -> {:04x}", dest);
            let mut ctx = (
                Gbz80State::default(),
                [0u8; 0x200],
                SteppingExecutorState::default(),
            );
            ctx.1[0x100] = 0xd9;
            ctx.0.regs.pc = 0x100;
            let [low, high] = dest.to_le_bytes();
            ctx.1[0x1fe] = low;
            ctx.1[0x1ff] = high;
            ctx.0.regs.sp = 0x1fe;

            let expected = {
                let mut expected = ctx.clone();
                expected.0.regs.sp = 0x200;
                expected.0.regs.pc = dest;
                expected.0.interrupt_master_enable.set();
                expected
            };

            run_single_instruction(&mut ctx);
            assert_eq!(ctx, expected);
        }
    }

    #[test]
    fn calls() {
        init();

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
                    let mut ctx = (
                        Gbz80State::default(),
                        [0u8; 0x200],
                        SteppingExecutorState::default(),
                    );
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

                    run_single_instruction(&mut ctx);
                    assert_eq!(ctx, expected);
                }
            }
        }
    }
}
