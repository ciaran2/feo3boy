//! This module provides the [`Microcode`] type, which specifies the sub-steps that
//! [`Opcodes`][crate::opcode::Opcode] can be broken down into.
use quote::quote;

use feo3boy_microcode_generator::define_microcode;

use crate::compiler::args::{Arg, AsLiteral};
use crate::compiler::OperationType;
use crate::gbz80types::Flags;
use crate::microcode::args::{Reg16, Reg8};

pub mod args;
pub mod combocodes;

/// [`Microcode`] is a set of simple instructions designed specifically for implementing
/// the opcodes on the gbz80 processor used on the gameboy. These instructions operate on
/// a stack of bytes called the microcode stack, which is separate from the gameboy's
/// stack. Each microcode operation pops some values off of the stack, computes a result,
/// and pushes its results onto the stack.
///
/// Most microcode operations are pure functions, meaning they only operate on the
/// microcode stack. The functionality of those operations is defined directly in this
/// crate. For operations which are not-pure, such as skips which execute part of the
/// microcode only conditionally or operations which act directly on the gbz80 CPU in some
/// way, the behavior of those operations is defined externally.
#[define_microcode(Microcode)]
pub mod defs {
    use crate::gbz80types::Flags;

    allowed_types! {
        name = ValTypes,
        types = [
            /// Specifies a stack argument of type `u8`.
            u8 => U8,
            /// Specifies a stack argument of type `bool`. (Assumed to occupy 8 bits on
            /// the stack like u8).
            bool => Bool,
            /// Specifies a stack argument of type `u16`.
            u16 => U16,
            /// Specifies a stack argument of type `Flags`. (Assumed to occupy 8 bits on
            /// the stack like u8).
            Flags => Flags,
        ],
    }

    /// Delays execution for 1m cycle.
    #[microcode_extern(Yield)]
    pub fn r#yield() {}

    /// Read an 8-bit value from a register.
    #[microcode_extern(ReadReg)]
    pub fn read_reg(
        /// The register to read from.
        #[field]
        reg: Reg8,
    ) -> u8 {
    }

    /// Write an 8-bit value to a register.
    #[microcode_extern(WriteReg)]
    pub fn write_reg(
        /// The register to write to.
        #[field]
        reg: Reg8,
        val: u8,
    ) {
    }

    /// Read an 8-bit value from a register.
    #[microcode_extern(ReadReg16)]
    pub fn read_reg(
        /// The register to read from.
        #[field]
        reg: Reg16,
    ) -> u16 {
    }

    /// Write an 8-bit value to a register.
    #[microcode_extern(WriteReg16)]
    pub fn write_reg(
        /// The register to write to.
        #[field]
        reg: Reg16,
        val: u16,
    ) {
    }

    /// Pop a 16 bit value from the stack and use it to read an 8 bit value onto the
    /// stack.
    #[microcode_extern(ReadMem)]
    pub fn read_mem(addr: u16) -> u8 {}

    /// Pop a 16 bit value from the stack and use it as the address, then pop an 8 bit
    /// value from the stack and wite it to that address.
    #[microcode_extern(WriteMem)]
    pub fn write_mem(addr: u16, val: u8) {}

    /// Fetches the flags register onto the microcode stack,
    #[microcode_extern(GetFlagsMasked)]
    pub fn get_flags_masked(#[field] mask: Flags) -> Flags {}

    /// Pops flags off the microcode stack, masks them, and applies them to the flags
    /// register.
    #[microcode_extern(SetFlagsMasked)]
    pub fn set_flags_masked(#[field] mask: Flags, flags: Flags) {}

    /// Append an 8-bit value to the microcode stack. (Essentially provides a constant
    /// value).
    #[microcode(Append)]
    #[inline]
    pub fn append(
        /// The value to place on the stack.
        #[field]
        val: u8,
    ) -> u8 {
        val
    }

    /// Takes one u16 from the stack and pushes 2 copies of it onto the stack.
    #[microcode(Dup16)]
    #[inline]
    pub fn dup(v: u16) -> (u16, u16) {
        (v, v)
    }

    /// Discard an 8 bit value from the microcode stack.
    #[microcode(Discard8)]
    #[inline]
    pub fn discard8(_: u8) {}

    /// Discard a 16 bit value from the microcode stack.
    #[microcode(Discard16)]
    #[inline]
    pub fn discard16(_: u16) {}

    /// Pop a u8 and a u16 off the microcode stack and push them in reverse order (u16 on
    /// top, u8 below it).
    #[microcode(Swap816)]
    #[inline]
    pub fn swap816(top: u8, second: u16) -> (u8, u16) {
        (top, second)
    }

    /// Takes a u16 address off the stack followed by a u16 value, then splits the value
    /// into separate bytes and pushes them on the stack in the order: u8 high, u16 addr,
    /// u8 low, u16 addr. This is useful for performing a 16 bit write where the low byte
    /// is written first.
    #[microcode(Intersperse)]
    #[inline]
    pub fn intersperse(addr: u16, val: u16) -> (u8, u16, u8, u16) {
        let [low, high] = val.to_le_bytes();
        (high, addr, low, addr)
    }

    /// Boolean not. Note this is an interanl microcode operation, not a gameboy ALU
    /// operation.
    #[microcode(Not)]
    #[inline]
    pub fn not(val: bool) -> bool {
        !val
    }

    /// Perform an 8 bit add.
    #[microcode(Add)]
    pub fn add(lhs: u8, rhs: u8) -> (u8, Flags) {
        let mut flags = Flags::empty();
        if (lhs & 0xf) + (rhs & 0xf) > 0xf {
            flags |= Flags::HALFCARRY;
        }
        let (res, carry) = lhs.overflowing_add(rhs);
        flags |= Flags::check_zero(res) | Flags::check_carry(carry);
        (res, flags)
    }

    /// Perform an 8 bit add-carry.
    #[microcode(Adc)]
    pub fn adc(prevflags: Flags, lhs: u8, rhs: u8) -> (u8, Flags) {
        let (mut res, mut flags) = add(lhs, rhs);
        if prevflags.contains(Flags::CARRY) {
            let (res2, flags2) = add(res, 1);
            res = res2;
            // Zero flag should only be set if the second add had a result of zero.
            flags = flags2 | (flags - Flags::ZERO);
        }
        (res, flags)
    }

    /// Perform an 8 bit sub.
    #[microcode(Sub)]
    pub fn sub(lhs: u8, rhs: u8) -> (u8, Flags) {
        let mut flags = Flags::SUB;
        if (lhs & 0xf) < (rhs & 0xf) {
            flags |= Flags::HALFCARRY;
        }
        let (res, carry) = lhs.overflowing_sub(rhs);
        flags |= Flags::check_zero(res) | Flags::check_carry(carry);
        (res, flags)
    }

    /// Perform an 8 bit sub-carry.
    #[microcode(Sbc)]
    pub fn sbc(prevflags: Flags, lhs: u8, rhs: u8) -> (u8, Flags) {
        let (mut res, mut flags) = sub(lhs, rhs);
        if prevflags.contains(Flags::CARRY) {
            let (res2, flags2) = sub(res, 1);
            res = res2;
            // Zero flag should only be set if the second subtract had a result of
            // zero.
            flags = flags2 | (flags - Flags::ZERO);
        }
        (res, flags)
    }

    /// Perform a microcode `and`.
    #[microcode(And)]
    #[inline]
    pub fn and(lhs: u8, rhs: u8) -> (u8, Flags) {
        let res = lhs & rhs;
        let flags = Flags::HALFCARRY | Flags::check_zero(res);
        (res, flags)
    }

    /// Perform a microcde `or`
    #[microcode(Or)]
    #[inline]
    pub fn or(lhs: u8, rhs: u8) -> (u8, Flags) {
        let res = lhs | rhs;
        let flags = Flags::check_zero(res);
        (res, flags)
    }

    /// Perform a microcde `xor`
    #[microcode(Xor)]
    #[inline]
    pub fn xor(lhs: u8, rhs: u8) -> (u8, Flags) {
        let res = lhs ^ rhs;
        let flags = Flags::check_zero(res);
        (res, flags)
    }

    /// Rotate the value left by 8 bits.
    #[microcode(RotateLeft8)]
    #[inline]
    pub fn rotate_left8(val: u8) -> (u8, Flags) {
        let res = val.rotate_left(1);
        let flags = Flags::check_zero(res) | Flags::check_carry(res & 1 != 0);
        (res, flags)
    }

    /// Rotate the value left by 9 bits, rotating through the carry flag.
    #[microcode(RotateLeft9)]
    pub fn rotate_left9(prevflags: Flags, val: u8) -> (u8, Flags) {
        let rotated = val.rotate_left(1);
        // Pull the carry flag into the first bit.
        let res = rotated & 0xfe | prevflags.contains(Flags::CARRY) as u8;
        // Set the carry flag based on the bit that was rotated out of max position.
        let flags = Flags::check_carry(rotated & 1 != 0) | Flags::check_zero(res);
        (res, flags)
    }

    /// Rotate the value right by 8 bits.
    #[microcode(RotateRight8)]
    #[inline]
    pub fn rotate_right8(val: u8) -> (u8, Flags) {
        let res = val.rotate_right(1);
        let flags = Flags::check_zero(res) | Flags::check_carry(res & 0x80 != 0);
        (res, flags)
    }

    /// Rotate the value right by 9 bits, rotating through the carry flag.
    #[microcode(RotateRight9)]
    pub fn rotate_right9(prevflags: Flags, val: u8) -> (u8, Flags) {
        let rotated = val.rotate_right(1);
        // Pull the carry flag into the highest bit.
        let res = rotated & 0x7f | ((prevflags.contains(Flags::CARRY) as u8) << 7);
        // Set the carry flag based on the bit that was rotated out of min position.
        let flags = Flags::check_carry(val & 1 != 0) | Flags::check_zero(res);
        (res, flags)
    }

    /// Complement the value on top of the stack.
    #[microcode(Compliment)]
    pub fn compliment(val: u8) -> (u8, Flags) {
        const FLAGS: Flags = Flags::SUB.union(Flags::HALFCARRY);
        let res = !val;
        (res, FLAGS)
    }

    /// Shift the value left by one bit.
    #[microcode(ShiftLeft)]
    pub fn shift_left(val: u8) -> (u8, Flags) {
        let res = val << 1;
        let flags = Flags::check_zero(res) | Flags::check_carry(val & 0x80 != 0);
        (res, flags)
    }

    /// Shift the value right by one bit.
    #[microcode(ShiftRight)]
    pub fn shift_right(val: u8) -> (u8, Flags) {
        let res = val >> 1;
        let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
        (res, flags)
    }

    /// Shift the value right by one bit, performing sign-extension.
    #[microcode(ShiftRightSignExt)]
    pub fn shift_right_sign_ext(val: u8) -> (u8, Flags) {
        let res = ((val as i8) >> 1) as u8;
        let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
        (res, flags)
    }

    /// Swaps the lower and upper nybble of the byte.
    #[microcode(Swap)]
    pub fn swap(val: u8) -> (u8, Flags) {
        let res = ((val & 0x0f) << 4) | ((val & 0xf0) >> 4);
        let flags = Flags::check_zero(res);
        (res, flags)
    }

    /// Helper for doing binary-coded-decimal. Adjusts the hex didgits to keep both
    /// nybbles in range 0..=9 by adding 0x06 and/or 0x60 to push the digit to the next
    /// nybble. Depends on the carry/halfcarry flags.
    #[microcode(DecimalAdjust)]
    pub fn decmial_adjust(prevflags: Flags, val: u8) -> (u8, Flags) {
        // Always clears HALFCARRY.
        let mut flags = Flags::empty();
        let mut res = val;
        if prevflags.contains(Flags::SUB) {
            if prevflags.contains(Flags::CARRY) {
                flags |= Flags::CARRY;
                res = res.wrapping_sub(0x60);
            }
            if prevflags.contains(Flags::HALFCARRY) {
                res = res.wrapping_sub(0x06);
            }
        } else {
            if prevflags.contains(Flags::CARRY) || val > 0x99 {
                flags |= Flags::CARRY;
                res = res.wrapping_add(0x60);
            }
            if prevflags.contains(Flags::HALFCARRY) || res & 0xf > 9 {
                res = res.wrapping_add(0x06);
            }
        }
        flags |= Flags::check_zero(res);
        (res, flags)
    }

    /// Tests if a particular bit is set in the output.
    #[microcode(TestBit)]
    #[inline]
    pub fn test_bit(
        /// The index of the bit to test.
        #[field]
        bit: u8,
        val: u8,
    ) -> Flags {
        Flags::check_zero(val & (1 << bit)) | Flags::HALFCARRY
    }

    /// Sets a paricular bit in the output.
    #[microcode(SetBit)]
    #[inline]
    pub fn set_bit(
        /// The index of the bit to set.
        #[field]
        bit: u8,
        val: u8,
    ) -> u8 {
        val | (1 << bit)
    }

    /// Clears a paricular bit in the output.
    #[microcode(ResetBit)]
    #[inline]
    pub fn reset_bit(
        /// The index of the bit to clear..
        #[field]
        bit: u8,
        val: u8,
    ) -> u8 {
        val & !(1 << bit)
    }

    /// Increments a 16 bit value.
    #[microcode(Inc16)]
    #[inline]
    pub fn inc16(val: u16) -> u16 {
        val.wrapping_add(1)
    }

    /// Decrements a 16 bit value.
    #[microcode(Dec16)]
    #[inline]
    pub fn dec16(val: u16) -> u16 {
        val.wrapping_sub(1)
    }

    /// Performs a 16 bit add with flags. Pops two 16 bit args off the stack (lhs on top,
    /// rhs below it), adds them, and pushes the result followed by the flags on top. The
    /// returned flags will have 00HC set based on the upper byte of the operation (as if
    /// it was performed by running the pseudo-instructions `add l,<arg-low>; adc
    /// h,<arg-high>`.
    #[microcode(Add16)]
    #[inline]
    pub fn add16(lhs: u16, rhs: u16) -> (u16, Flags) {
        let mut flags = Flags::empty();
        if (lhs & 0x7ff) + (rhs & 0x7ff) > 0x7ff {
            flags |= Flags::HALFCARRY;
        }
        let (res, carry) = lhs.overflowing_add(rhs);
        flags |= Flags::check_carry(carry);
        (res, flags)
    }

    /// Pops a 16 bit address off the stack followed by an 8 bit offset below it. Applies
    /// address offsetting and pushes the new address followed by the flags on top.
    #[microcode(OffsetAddr)]
    pub fn offset_addr(addr: u16, offset: u8) -> (u16, Flags) {
        // Perform sign-extension, then treat as u16.
        let offset = offset as i8 as i16 as u16;

        // JR ignores flags, SP+i8 is used in two instructions and sets flags for carry
        // and half carry based on the lower byte.
        let mut flags = Flags::empty();
        if (addr & 0xf) + (offset & 0xf) > 0xf {
            flags |= Flags::HALFCARRY;
        }
        // Since we're working on 16 bits, we calculate CARRY for the lower byte the same
        // way we typically do a HALFCARRY.
        if (addr & 0xff) + (offset & 0xff) > 0xff {
            flags |= Flags::CARRY;
        }

        // In two's compliment, adding a negative is the same as adding with wraparound.
        (addr.wrapping_add(offset), flags)
    }

    /// Panics if the stop instruction is reached. This should probably become an extern
    /// if stop is ever implemented.
    #[microcode(Stop)]
    pub fn stop() {
        panic!("STOP is bizarre and complicated and not implemented.")
    }

    /// Puts the CPU into the halted state.
    #[microcode_extern(Halt)]
    pub fn halt() {}

    /// Enables interrupts, either immediately or after the next instruction.
    #[microcode_extern(EnableInterrupts)]
    pub fn enable_interrupts(
        /// If true, enable interrupts immediately, otherwise after the next instruction.
        #[field]
        immediate: bool,
    ) {
    }

    /// Disables interrupts immediately.
    #[microcode_extern(DisableInterrupts)]
    pub fn disable_interrupts() {}

    /// Retrieves the value of the halted flag from the CPU.
    #[microcode_extern(CheckHalt)]
    pub fn check_halt() -> bool {}

    /// Sets the CPU to not be halted.
    #[microcode_extern(ClearHalt)]
    pub fn clear_halt() {}

    /// Gets a bool indicating if IME is set.
    #[microcode_extern(CheckIme)]
    pub fn check_ime() -> bool {}

    /// Gets the set of currently active and enabled interrupts.
    #[microcode_extern(GetActiveInterrupts)]
    pub fn get_active_interrupts() -> u8 {}

    /// Gets the address of the interrupt handler for the next active and enabled
    /// interrupt from the interupt vector and clears that interrupt from the interrupt
    /// vector. Does not disable interrupts.
    #[microcode_extern(PopInterrupt)]
    pub fn pop_interrupt() -> u16 {}

    /// Pushes the value of the Halt Bug flag onto the microcode stack, clearing the value
    /// to false.
    #[microcode_extern(PopHaltBug)]
    pub fn pop_halt_bug() -> bool {}

    /// Tells the CPU that interrupt_master_enable.tick should be run when the current
    /// instruction finishes. This will happen on FetchNextInstruction, whether that is
    /// triggered explicitly or by reaching the end of the current instruction.
    #[microcode_extern(TickImeOnEnd)]
    pub fn tick_ime_on_end() {}

    /// Unconditionally skip the given number of microcode steps.
    ///
    /// Skips the microcode pc forward by this number of steps. Note that the microcode pc
    /// is already incremented for the skip instruction, so that is not counted when
    /// figuring out how many steps to skip.
    #[microcode_extern(Skip)]
    pub fn skip(
        /// Number of steps in the microcode to skip over.
        #[field]
        steps: usize,
    ) {
    }

    /// Conditionally skip the given number of microcode steps.
    ///
    /// Pops an 8 bit value off the microcode stack, and if it is non-zero, skips the
    /// microcode pc forward by this number of steps. Note that the microcode pc is
    /// already incremented for the skip instruction, so that is not counted when figuring
    /// out how many steps to skip.
    #[microcode_extern(SkipIf)]
    pub fn skip_if(
        /// Number of steps in the microcode to skip over.
        #[field]
        steps: usize,
        cond: bool,
    ) {
    }

    /// Replace the currently executing instruction with the microcode for the CPU's
    /// internal halt check, interrupt handler, and instruction fetch.
    ///
    /// Executing this instruction also checks if `previous_ime` is set, and if so, ticks
    /// the IME state forward.
    ///
    /// It is not necessary to include this in every instruction, as the CPU will perform
    /// this automatically if it runs out of steps in the currently executing instruction.
    /// This can be useful as a 'break' or 'return' from within an instruction.
    #[microcode_extern(FetchNextInstruction)]
    pub fn fetch_next_instruction() {}

    // Pop an 8 bit value off the microcode stack and look it up the the opcode table.
    // Replace the currently executing instruction with that instruction.
    #[microcode_extern(ParseOpcode)]
    pub fn parse_opcode(opcode: u8) {}

    // Pop an 8 bit value off the microcode stack and look it up the the CB opcode table.
    // Replace the currently executing instruction with that instruction.
    #[microcode_extern(ParseCBOpcode)]
    pub fn parse_cb_opcode(opcode: u8) {}
}
