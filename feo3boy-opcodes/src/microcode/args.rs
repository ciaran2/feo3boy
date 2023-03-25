//! Types which are used as arguments to the [`Microcode`][crate::microcode::Microcode].
use proc_macro2::{Ident, Span};
use quote::quote;

use crate::compiler::args::{AsLiteral, Literal};
use crate::compiler::instr::builder::{InstrBuilder, MicrocodeReadable, MicrocodeWritable};
use crate::microcode::Microcode;

/// Identifies an 8 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Reg8 {
    Acc,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl MicrocodeReadable for Reg8 {
    fn to_read(self) -> InstrBuilder {
        Microcode::ReadReg { reg: self }.into()
    }
}

impl MicrocodeWritable for Reg8 {
    fn to_write(self) -> InstrBuilder {
        Microcode::WriteReg { reg: self }.into()
    }
}

impl AsLiteral for Reg8 {
    fn as_literal(&self) -> Literal {
        let val = match self {
            Self::Acc => "Acc",
            Self::B => "B",
            Self::C => "C",
            Self::D => "D",
            Self::E => "E",
            Self::H => "H",
            Self::L => "L",
        };
        let val = Ident::new(val, Span::call_site());
        quote! { Reg8::#val }.into()
    }
}

/// Identifies a 16 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    Sp,
    Pc,
}

impl MicrocodeReadable for Reg16 {
    fn to_read(self) -> InstrBuilder {
        Microcode::ReadReg16 { reg: self }.into()
    }
}

impl MicrocodeWritable for Reg16 {
    fn to_write(self) -> InstrBuilder {
        Microcode::WriteReg16 { reg: self }.into()
    }
}

impl AsLiteral for Reg16 {
    fn as_literal(&self) -> Literal {
        let val = match self {
            Self::AF => "AF",
            Self::BC => "BC",
            Self::DE => "DE",
            Self::HL => "HL",
            Self::Sp => "Sp",
            Self::Pc => "Pc",
        };
        let val = Ident::new(val, Span::call_site());
        quote! { Reg16::#val }.into()
    }
}
