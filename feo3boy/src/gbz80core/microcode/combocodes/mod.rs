//! Provides combination opcodes. The [`ComboCode`] type is always available. If feature
//! `combo-ops` is not used, `ComboCode`s work like the helper types provided in the main
//! [`microcode`][crate::gbz80core::microcode] module and just expand to several
//! [`Microcode`][crate::gbz80core::microcode::Microcode] steps. However, if the
//! `combo-ops` feature is enabled, `Microcode` gains a new variant called `ComboCode`
//! which directly evaluates the combo code using special-purpose Rust code.

use crate::gbz80core::microcode::MicrocodeBuilder;

mod expansions;
#[cfg(feature = "combo-code")]
mod r#impl;

/// Combination opcodes. Depending on whether the `combo-code` feature is enabled, these
/// are either just shorthands that expand to several
/// [`Microcode`][crate::gbz80core::microcode::Microcode] steps or fully-featured,
/// independent opcodes implemented in Rust which do the equivalent of their `Microcode`
/// expansion in a single step.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ComboCode {
    HandleHalt,
}

impl ComboCode {
    /// Get the set of microcode steps that this combo code expands to.
    pub fn expand(self) -> MicrocodeBuilder {
        match self {
            Self::HandleHalt => expansions::handle_halt(),
        }
    }
}

#[cfg(not(feature = "combo-code"))]
impl From<ComboCode> for MicrocodeBuilder {
    fn from(code: ComboCode) -> Self {
        code.expand()
    }
}

#[cfg(feature = "combo-code")]
impl From<ComboCode> for MicrocodeBuilder {
    fn from(code: ComboCode) -> Self {
        use crate::gbz80core::microcode::Microcode;
        Microcode::ComboCode(code).into()
    }
}
