//! Types which are used in the gbz80.
use bitflags::bitflags;
use proc_macro2::TokenStream;
use quote::quote;

use crate::compiler::args::{CrateFetcher, Literal};
use crate::compiler::instr::builder::{InstrBuilder, MicrocodeReadable, MicrocodeWritable};
use crate::microcode::Microcode;

bitflags! {
    /// Flags set after various operations.
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

impl Flags {
    /// Merge the given flags into the current flags by applying the given mask to set
    /// only flags in that mask.
    pub fn merge(&mut self, flags: Flags, mask: Flags) {
        *self = (*self & !mask) | (flags & mask);
    }

    /// If the value is zero, returns `Flags::ZERO`, otherwise returns `Flags::empty()`.
    pub fn check_zero(val: u8) -> Flags {
        if val == 0 {
            Flags::ZERO
        } else {
            Flags::empty()
        }
    }

    /// If carry is true, returns `Flags::CARRY` otherwise returns `Flags::empty()`.
    pub fn check_carry(carry: bool) -> Flags {
        if carry {
            Flags::CARRY
        } else {
            Flags::empty()
        }
    }
}

/// When used as a MicrocodeWritable, Flags causes a write to the Flags register which
/// applies the given mask to the set of flags being written. In effect it is an
/// instruction to overwrite the specified flags.
impl MicrocodeWritable for Flags {
    fn to_write(self) -> InstrBuilder {
        Microcode::SetFlagsMasked { mask: self }.into()
    }
}

/// When used as a MicrocodeReadable, Flags causes a read from the Flags register which
/// applies the given mask to the set of flags being read. In effect it is an
/// instruction to read just the specified flags and put zeroes for all others.
impl MicrocodeReadable for Flags {
    fn to_read(self) -> InstrBuilder {
        Microcode::GetFlagsMasked { mask: self }.into()
    }
}

impl Literal for Flags {
    fn constant_value(&self, crates: CrateFetcher) -> TokenStream {
        let feo3boy_opcodes = crates("feo3boy-opcodes");
        let val = format!("0b{:b}u8", self.bits())
            .parse::<proc_macro2::Literal>()
            .unwrap();
        quote! { #feo3boy_opcodes::gbz80types::Flags::from_bits_retain(#val) }.into()
    }
}
