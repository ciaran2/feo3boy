//! Defines microcode for the `feo3boy` gameboy.
//!
//! [`Microcode`] is a set of simple instructions designed specifically for implementing
//! the opcodes on the gbz80 processor used on the gameboy. These instructions operate on
//! a stack of bytes called the microcode stack, which is separate from the gameboy's
//! stack. Each microcode operation pops some values off of the stack, computes a result,
//! and pushes its results onto the stack.
//!
//! Most microcode operations are pure functions, meaning they only operate on the
//! microcode stack. The functionality of those operations is defined directly in this
//! crate. For operations which are not-pure, such as skips which execute part of the
//! microcode only conditionally or operations which act directly on the gbz80 CPU in some
//! way, the behavior of those operations is defined externally.
use bitflags::bitflags;

use feo3boy_microcode_generator::define_microcode;

bitflags! {
    /// Flags set after various operations.
    #[derive(Default)]
    pub struct Flags: u8 {
        /// result was zero.
        const ZERO = 0x80;
        /// operation was a subtraction.
        const SUB = 0x40;
        /// there was a carry in the middle of the number (bit 4 -> 5 for u8, uncertain which bits
        /// this is for in u16).
        const HALFCARRY = 0x20;
        /// there was a carry out of the top of the number (bit 7 -> carry for u8, presumably bit 15
        /// -> carry for u16, though not sure).
        const CARRY = 0x10;
    }
}

/// Identifies an 8 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Reg8 {
    Acc,
    B,
    C,
    D,
    E,
    H,
    L,
}

/// Identifies a 16 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    Sp,
    Pc,
}

/// `Microcode` defines every operation which can be performed in the microcode system.
#[define_microcode(Microcode)]
pub mod defs {
    allowed_types! {
        name = ValTypes,
        types = [
            /// Specifies a stack argument of type `u8`.
            u8 => U8,
            /// Specifies a stack argument of type `u16`.
            u16 => U16,
            /// Specifies a stack argument of type `Flags`.
            Flags => Flags,
        ],
    }

    /// Delays execution for 1m cycle.
    #[microcode_extern(Yield)]
    pub fn r#yield() {}

    /// Read an 8-bit value from a register.
    #[microcode_extern(ReadReg8)]
    pub fn read_reg8(
        /// The register to read from.
        #[field]
        reg: Reg8,
    ) -> u8 {
    }

    /// Write an 8-bit value to a register.
    #[microcode_extern(WriteReg8)]
    pub fn write_reg8(
        /// The register to write to.
        #[field]
        reg: Reg8,
        val: u8,
    ) {
    }

    /// Read an 8-bit value from memory.
    #[microcode_extern(ReadMem8)]
    pub fn read_mem8(addr: u16) -> u8 {}

    /// Write an 8-bit value to memory.
    #[microcode_extern(WriteMem8)]
    pub fn write_mem8(addr: u16, val: u8) {}

    /// Fetches the flags register onto the microcode stack,
    #[microcode_extern(GetFlagsMasked)]
    pub fn get_flags_masked(#[field] mask: Flags) -> Flags {}

    /// Append an 8-bit value to the microcode stack. (Essentially provides a constant
    /// value).
    #[microcode(Append)]
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
}
