/// Defines the common set of tests for the executors.
#[macro_export]
macro_rules! executor_tests {
    ($name:ident, $exe:ty, $test_gb:ty) => {
        #[cfg(test)]
        mod $name {
            use feo3boy_opcodes::gbz80types::Flags;

            use crate::gbz80core::executor::Executor;

            fn init() {
                let _ = env_logger::builder().is_test(true).try_init();
            }

            fn run_single_instruction(gb: &mut $test_gb) {
                <$exe>::run_single_instruction(gb)
            }

            #[test]
            fn test_loads_and_alu() {
                init();

                let mut gb = <$test_gb>::default();

                gb.mem[0] = 0x3e;
                gb.mem[1] = 0x80;
                gb.mem[2] = 0x06;
                gb.mem[3] = 0x01;
                gb.mem[4] = 0x80;
                gb.mem[5] = 0x0e;
                gb.mem[6] = 0x85;
                gb.mem[7] = 0x81;

                run_single_instruction(&mut gb);
                assert_eq!(gb.cpu.regs.acc, 0x80, "{:?}", gb.cpu.regs);

                run_single_instruction(&mut gb);
                assert_eq!(gb.cpu.regs.b, 0x01, "{:?}", gb.cpu.regs);

                run_single_instruction(&mut gb);
                assert_eq!(gb.cpu.regs.acc, 0x81, "{:?}", gb.cpu.regs);

                run_single_instruction(&mut gb);
                assert_eq!(gb.cpu.regs.c, 0x85, "{:?}", gb.cpu.regs);

                run_single_instruction(&mut gb);
                assert_eq!(gb.cpu.regs.acc, 0x6, "{:?}", gb.cpu.regs);
                assert!(
                    gb.cpu.regs.flags.contains(Flags::CARRY),
                    "{:?}",
                    gb.cpu.regs
                );
            }

            #[test]
            fn load8bit() {
                init();

                fn set_dest(inst: u8, gb: &mut $test_gb, val: u8) {
                    match inst {
                        0x40..=0x47 => gb.cpu.regs.b = val,
                        0x48..=0x4f => gb.cpu.regs.c = val,
                        0x50..=0x57 => gb.cpu.regs.d = val,
                        0x58..=0x5f => gb.cpu.regs.e = val,
                        0x60..=0x67 => gb.cpu.regs.h = val,
                        0x68..=0x6f => gb.cpu.regs.l = val,
                        0x70..=0x77 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        0x78..=0x7f => gb.cpu.regs.acc = val,
                        _ => unreachable!(),
                    }
                }
                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        0x7 => gb.cpu.regs.acc = val,
                        _ => unreachable!(),
                    }
                }
                for inst in (0x40..=0x7f).filter(|&i| i != 0x76) {
                    for input in 0..=0xff {
                        let mut gb = <$test_gb>::default();
                        // Start with HL at a high number so any single-byte change to it will never
                        // collide with the instruction at 0x00.
                        gb.cpu.regs.set_hl(0xAA);
                        // This step might overwrite H or L changing the destination address of LD (HL),H
                        // or LD (HL),L, but that's ok because we use HL in set_dest.
                        set_source(inst, &mut gb, input);
                        gb.mem[0] = inst;

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            set_dest(inst, &mut expected, input);
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn load8bit_immediate() {
                init();

                fn set_dest(inst: u8, gb: &mut $test_gb, val: u8) {
                    match inst {
                        0x06 => gb.cpu.regs.b = val,
                        0x0e => gb.cpu.regs.c = val,
                        0x16 => gb.cpu.regs.d = val,
                        0x1e => gb.cpu.regs.e = val,
                        0x26 => gb.cpu.regs.h = val,
                        0x2e => gb.cpu.regs.l = val,
                        0x36 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        0x3e => gb.cpu.regs.acc = val,
                        _ => unreachable!(),
                    }
                }
                for inst in (0x00..=0x30)
                    .step_by(0x10)
                    .flat_map(|high| [high + 0x06, high + 0x0e])
                {
                    for input in 0..=0xff {
                        let mut gb = <$test_gb>::default();
                        // Start with HL at a high number so any single-byte change to it will never
                        // collide with the instruction at 0x00.
                        gb.cpu.regs.set_hl(2);
                        gb.mem[0] = inst;
                        gb.mem[1] = input;

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 2;
                            set_dest(inst, &mut expected, input);
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn add8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0x80..=0x86 {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("add({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let (sum, wrapped) = v1.overflowing_add(v2);
                                    expected.cpu.regs.acc = sum;
                                    expected.cpu.regs.flags = Flags::empty();
                                    if sum == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    if wrapped {
                                        expected.cpu.regs.flags |= Flags::CARRY;
                                    }
                                    if (v1 & 0xf) + (v2 & 0xf) > 0xf {
                                        expected.cpu.regs.flags |= Flags::HALFCARRY;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    // A,u8
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("add(c6) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xc6;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let (sum, wrapped) = v1.overflowing_add(v2);
                                expected.cpu.regs.acc = sum;
                                expected.cpu.regs.flags = Flags::empty();
                                if sum == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                if wrapped {
                                    expected.cpu.regs.flags |= Flags::CARRY;
                                }
                                if (v1 & 0xf) + (v2 & 0xf) > 0xf {
                                    expected.cpu.regs.flags |= Flags::HALFCARRY;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    // A,A
                    for v in 0..=0xff {
                        println!("add(87) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0x87;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            let (sum, wrapped) = v.overflowing_add(v);
                            expected.cpu.regs.acc = sum;
                            expected.cpu.regs.flags = Flags::empty();
                            if sum == 0 {
                                expected.cpu.regs.flags |= Flags::ZERO;
                            }
                            if wrapped {
                                expected.cpu.regs.flags |= Flags::CARRY;
                            }
                            if (v & 0xf) + (v & 0xf) > 0xf {
                                expected.cpu.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn adc8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
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
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let (sum1, wrapped1) = v1.overflowing_add(v2);
                                    let (sum2, wrapped2) = sum1.overflowing_add(carry);
                                    expected.cpu.regs.acc = sum2;
                                    expected.cpu.regs.flags = Flags::empty();
                                    if sum2 == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    if wrapped1 || wrapped2 {
                                        expected.cpu.regs.flags |= Flags::CARRY;
                                    }
                                    if (v1 & 0xf) + (v2 & 0xf) + carry > 0xf {
                                        expected.cpu.regs.flags |= Flags::HALFCARRY;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    // ADC A,u8
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("adc(ce) {},{} {}", v1, v2, carry);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xce;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let (sum1, wrapped1) = v1.overflowing_add(v2);
                                let (sum2, wrapped2) = sum1.overflowing_add(carry);
                                expected.cpu.regs.acc = sum2;
                                expected.cpu.regs.flags = Flags::empty();
                                if sum2 == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                if wrapped1 || wrapped2 {
                                    expected.cpu.regs.flags |= Flags::CARRY;
                                }
                                if (v1 & 0xf) + (v2 & 0xf) + carry > 0xf {
                                    expected.cpu.regs.flags |= Flags::HALFCARRY;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    // ADC A,A
                    for v in 0..=0xff {
                        println!("adc(8f) {},{} {}", v, v, carry);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0x8f;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                        if carry == 1 {
                            gb.cpu.regs.flags |= Flags::CARRY;
                        }

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            let (sum1, wrapped1) = v.overflowing_add(v);
                            let (sum2, wrapped2) = sum1.overflowing_add(carry);
                            expected.cpu.regs.acc = sum2;
                            expected.cpu.regs.flags = Flags::empty();
                            if sum2 == 0 {
                                expected.cpu.regs.flags |= Flags::ZERO;
                            }
                            if wrapped1 || wrapped2 {
                                expected.cpu.regs.flags |= Flags::CARRY;
                            }
                            if (v & 0xf) + (v & 0xf) + carry > 0xf {
                                expected.cpu.regs.flags |= Flags::HALFCARRY;
                            }
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn sub8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0x90..=0x96 {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("sub({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let diff = v1.wrapping_sub(v2);
                                    expected.cpu.regs.acc = diff;
                                    expected.cpu.regs.flags = Flags::SUB;
                                    if diff == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    if v1 < v2 {
                                        expected.cpu.regs.flags |= Flags::CARRY;
                                    }
                                    if (v1 & 0xf) < (v2 & 0xf) {
                                        expected.cpu.regs.flags |= Flags::HALFCARRY;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("sub(d6) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xd6;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let diff = v1.wrapping_sub(v2);
                                expected.cpu.regs.acc = diff;
                                expected.cpu.regs.flags = Flags::SUB;
                                if diff == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                if v1 < v2 {
                                    expected.cpu.regs.flags |= Flags::CARRY;
                                }
                                if (v1 & 0xf) < (v2 & 0xf) {
                                    expected.cpu.regs.flags |= Flags::HALFCARRY;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("sub(97) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0x97;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            expected.cpu.regs.acc = 0;
                            expected.cpu.regs.flags = Flags::ZERO | Flags::SUB;
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn sbc8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
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
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let diff = v1.wrapping_sub(v2).wrapping_sub(carry);
                                    expected.cpu.regs.acc = diff;
                                    expected.cpu.regs.flags = Flags::SUB;
                                    if diff == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    if v1 < v2 || (v1 == v2 && carry != 0) {
                                        expected.cpu.regs.flags |= Flags::CARRY;
                                    }
                                    if (v1 & 0xf) < (v2 & 0xf)
                                        || ((v1 & 0xf) == (v2 & 0xf) && carry != 0)
                                    {
                                        expected.cpu.regs.flags |= Flags::HALFCARRY;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("sbc(de) {},{} {}", v1, v2, carry);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xde;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let diff = v1.wrapping_sub(v2).wrapping_sub(carry);
                                expected.cpu.regs.acc = diff;
                                expected.cpu.regs.flags = Flags::SUB;
                                if diff == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                if v1 < v2 || (v1 == v2 && carry != 0) {
                                    expected.cpu.regs.flags |= Flags::CARRY;
                                }
                                if (v1 & 0xf) < (v2 & 0xf)
                                    || ((v1 & 0xf) == (v2 & 0xf) && carry != 0)
                                {
                                    expected.cpu.regs.flags |= Flags::HALFCARRY;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("sbc(9f) {},{} {}", v, v, carry);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0x9f;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            if carry == 0 {
                                expected.cpu.regs.acc = 0;
                                expected.cpu.regs.flags = Flags::ZERO | Flags::SUB;
                            } else {
                                expected.cpu.regs.acc = 0xff;
                                expected.cpu.regs.flags =
                                    Flags::SUB | Flags::CARRY | Flags::HALFCARRY;
                            }
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn and8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0xa0..=0xa6 {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("and({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let res = v1 & v2;
                                    expected.cpu.regs.acc = res;
                                    expected.cpu.regs.flags = Flags::HALFCARRY;
                                    if res == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("and(e6) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xe6;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let res = v1 & v2;
                                expected.cpu.regs.acc = res;
                                expected.cpu.regs.flags = Flags::HALFCARRY;
                                if res == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("and(a7) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0xa7;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            expected.cpu.regs.flags = Flags::HALFCARRY;
                            if v == 0 {
                                expected.cpu.regs.flags |= Flags::ZERO;
                            }
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn xor8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0xa8..=0xae {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("xor({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let res = v1 ^ v2;
                                    expected.cpu.regs.acc = res;
                                    expected.cpu.regs.flags = Flags::empty();
                                    if res == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("xor(ee) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xee;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let res = v1 ^ v2;
                                expected.cpu.regs.acc = res;
                                expected.cpu.regs.flags = Flags::empty();
                                if res == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("and(af) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0xaf;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            expected.cpu.regs.acc = 0;
                            expected.cpu.regs.flags = Flags::ZERO;
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn or8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0xb0..=0xb6 {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("or({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let res = v1 | v2;
                                    expected.cpu.regs.acc = res;
                                    expected.cpu.regs.flags = Flags::empty();
                                    if res == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("or(f6) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xf6;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let res = v1 | v2;
                                expected.cpu.regs.acc = res;
                                expected.cpu.regs.flags = Flags::empty();
                                if res == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("or(b7) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0xb7;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            expected.cpu.regs.flags = Flags::empty();
                            if v == 0 {
                                expected.cpu.regs.flags |= Flags::ZERO;
                            }
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
                    }
                }
            }

            #[test]
            fn cp8bit() {
                init();

                fn set_source(inst: u8, gb: &mut $test_gb, val: u8) {
                    match (inst & 0xf) % 0x8 {
                        0x0 => gb.cpu.regs.b = val,
                        0x1 => gb.cpu.regs.c = val,
                        0x2 => gb.cpu.regs.d = val,
                        0x3 => gb.cpu.regs.e = val,
                        0x4 => gb.cpu.regs.h = val,
                        0x5 => gb.cpu.regs.l = val,
                        0x6 => {
                            let dest = gb.cpu.regs.hl() as usize;
                            gb.mem[dest] = val;
                        }
                        _ => unreachable!(),
                    }
                }
                for existing_flags in (0x00..=0xf0).step_by(0x10) {
                    for inst in 0xb8..=0xbe {
                        for v1 in 0..=0xff {
                            for v2 in 0..=0xff {
                                println!("cp({:02x}) {},{}", inst, v1, v2);
                                let mut gb = <$test_gb>::default();
                                gb.mem[0] = inst;
                                gb.cpu.regs.acc = v1;
                                gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                                gb.cpu.regs.set_hl(1);
                                set_source(inst, &mut gb, v2);

                                let expected = {
                                    let mut expected = gb.clone();
                                    expected.cpu.regs.pc = 1;
                                    let diff = v1.wrapping_sub(v2);
                                    expected.cpu.regs.flags = Flags::SUB;
                                    if diff == 0 {
                                        expected.cpu.regs.flags |= Flags::ZERO;
                                    }
                                    if v1 < v2 {
                                        expected.cpu.regs.flags |= Flags::CARRY;
                                    }
                                    if (v1 & 0xf) < (v2 & 0xf) {
                                        expected.cpu.regs.flags |= Flags::HALFCARRY;
                                    }
                                    expected
                                };

                                run_single_instruction(&mut gb);
                                assert_eq!(gb, expected);
                            }
                        }
                    }
                    for v1 in 0..=0xff {
                        for v2 in 0..=0xff {
                            println!("cp(fe) {},{}", v1, v2);
                            let mut gb = <$test_gb>::default();
                            gb.mem[0] = 0xfe;
                            gb.cpu.regs.acc = v1;
                            gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);
                            gb.mem[1] = v2;

                            let expected = {
                                let mut expected = gb.clone();
                                expected.cpu.regs.pc = 2;
                                let diff = v1.wrapping_sub(v2);
                                expected.cpu.regs.flags = Flags::SUB;
                                if diff == 0 {
                                    expected.cpu.regs.flags |= Flags::ZERO;
                                }
                                if v1 < v2 {
                                    expected.cpu.regs.flags |= Flags::CARRY;
                                }
                                if (v1 & 0xf) < (v2 & 0xf) {
                                    expected.cpu.regs.flags |= Flags::HALFCARRY;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                    for v in 0..=0xff {
                        println!("cp(bf) {},{}", v, v);
                        let mut gb = <$test_gb>::default();
                        gb.mem[0] = 0xbf;
                        gb.cpu.regs.acc = v;
                        gb.cpu.regs.flags = Flags::from_bits_truncate(existing_flags);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = 1;
                            expected.cpu.regs.flags = Flags::ZERO | Flags::SUB;
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
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
                            let mut gb = <$test_gb>::default();
                            gb.cpu.regs.flags = Flags::from_bits_truncate(flags);
                            gb.mem[0x100] = opcode;
                            gb.cpu.regs.pc = 0x100;
                            gb.mem[0x101] = offset as u8;

                            let expected = {
                                let mut expected = gb.clone();
                                if should_jump {
                                    expected.cpu.regs.pc = (0x102 + offset as i32) as u16;
                                } else {
                                    expected.cpu.regs.pc = 0x102;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
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
                            let mut gb = <$test_gb>::default();
                            gb.cpu.regs.flags = Flags::from_bits_truncate(flags);
                            gb.mem[0] = opcode;
                            let [low, high] = dest.to_le_bytes();
                            gb.mem[1] = low;
                            gb.mem[2] = high;

                            let expected = {
                                let mut expected = gb.clone();
                                if should_jump {
                                    expected.cpu.regs.pc = dest;
                                } else {
                                    expected.cpu.regs.pc = 3;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
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
                        let mut gb = <$test_gb>::default();
                        gb.cpu.regs.flags = Flags::from_bits_truncate(flags);
                        gb.mem[0] = 0xe9;
                        gb.cpu.regs.set_hl(dest);

                        let expected = {
                            let mut expected = gb.clone();
                            expected.cpu.regs.pc = dest;
                            expected
                        };

                        run_single_instruction(&mut gb);
                        assert_eq!(gb, expected);
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
                    let mut gb = <$test_gb>::default();
                    gb.mem[0x101] = opcode;
                    gb.cpu.regs.pc = 0x101;
                    gb.cpu.regs.sp = 0x200;

                    let expected = {
                        let mut expected = gb.clone();
                        expected.cpu.regs.pc = dest;
                        expected.cpu.regs.sp = 0x1fe;
                        // Pushed address should be 0x0102 (in little endian order), since that's
                        // the address of the instruction after RST.
                        expected.mem[0x1fe] = 0x02;
                        expected.mem[0x1ff] = 0x01;
                        expected
                    };

                    run_single_instruction(&mut gb);
                    assert_eq!(gb, expected);
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
                            let mut gb = <$test_gb>::default();
                            gb.cpu.regs.flags = Flags::from_bits_truncate(flags);
                            gb.mem[0x100] = opcode;
                            gb.cpu.regs.pc = 0x100;
                            let [low, high] = dest.to_le_bytes();
                            gb.mem[0x1fe] = low;
                            gb.mem[0x1ff] = high;
                            gb.cpu.regs.sp = 0x1fe;

                            let expected = {
                                let mut expected = gb.clone();
                                if should_jump {
                                    expected.cpu.regs.sp = 0x200;
                                    expected.cpu.regs.pc = dest;
                                } else {
                                    expected.cpu.regs.pc = 0x101;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                }
            }

            #[test]
            fn interrupt_return() {
                init();

                for dest in 0x0000u16..=0xffff {
                    println!("reti(0xd9) -> {:04x}", dest);
                    let mut gb = <$test_gb>::default();
                    gb.mem[0x100] = 0xd9;
                    gb.cpu.regs.pc = 0x100;
                    let [low, high] = dest.to_le_bytes();
                    gb.mem[0x1fe] = low;
                    gb.mem[0x1ff] = high;
                    gb.cpu.regs.sp = 0x1fe;

                    let expected = {
                        let mut expected = gb.clone();
                        expected.cpu.regs.sp = 0x200;
                        expected.cpu.regs.pc = dest;
                        expected.cpu.interrupt_master_enable.set();
                        expected
                    };

                    run_single_instruction(&mut gb);
                    assert_eq!(gb, expected);
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
                            let mut gb = <$test_gb>::default();
                            gb.cpu.regs.flags = Flags::from_bits_truncate(flags);
                            gb.mem[0x100] = opcode;
                            gb.cpu.regs.pc = 0x100;
                            let [low, high] = dest.to_le_bytes();
                            gb.mem[0x101] = low;
                            gb.mem[0x102] = high;
                            gb.cpu.regs.sp = 0x200;

                            let expected = {
                                let mut expected = gb.clone();
                                if should_jump {
                                    expected.cpu.regs.sp = 0x1fe;
                                    // Return to 0x0103.
                                    expected.mem[0x1fe] = 0x03;
                                    expected.mem[0x1ff] = 0x01;
                                    expected.cpu.regs.pc = dest;
                                } else {
                                    expected.cpu.regs.pc = 0x103;
                                }
                                expected
                            };

                            run_single_instruction(&mut gb);
                            assert_eq!(gb, expected);
                        }
                    }
                }
            }
        }
    };
}
