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
    /// Perform the CPU insternal handling of the halted state.
    HandleHalt,
    /// Checks if interrupts are enabled, then checks if any interrupts are active.
    ///
    /// If any interrupt is enabled and active, it:
    /// - clears the interrupt and the IME.
    /// - Computes the address of the interrupt handler and pushes it to the microcode
    ///   stack.
    /// - Checks and clears the halt-bug flag, and uses the halt status to compute the
    ///   correct return location for the interrupt handler. It then pushes the return
    ///   location to the microcode stack.
    /// - Yields once.
    /// In total, two u16s will be pushed to the stack if an interrupt is being triggered.
    ///
    /// If no interrupt handling is required, skips the next `steps` microcode
    /// instructions.
    BeginInterruptHandlingOrSkip { steps: usize },
    /// Gets the current PC and puts it on the microcode stack and increments the program
    /// counter in-place. This instruction also provides a yield so that the `Immediate8`
    /// helper doesn't have to use a separate yield.
    NextImmediateAddr,
    /// Takes an opcode that is on the microcode stack and handles setting `TickImeOnEnd`,
    /// checking and applying the halt bug, and beginning execution of that opcode.
    ExecuteOpcode,
    /// Fetch the value of HL, then increment it in-place. Results in one u16 pushed onto
    /// the microcode stack.
    HlInc,
    /// Fetch the value of HL, then decrement it in-place. Results in one u16 pushed onto
    /// the microcode stack.
    HlDec,
    /// Load the value of the stack pointer to the microcode stack and then increment the
    /// stack pointer in-place.
    SpInc,
    /// Decrement the value of the stack pointer in-place and then read it onto the
    /// microcode stack.
    DecSp,
    /// Jump to the address of a RST instruction. (Does not do any of the rest of the RST
    /// instruction).
    JumpRst(u8),
}

impl ComboCode {
    /// Get the set of microcode steps that this combo code expands to.
    pub fn expand(self) -> MicrocodeBuilder {
        match self {
            Self::HandleHalt => expansions::handle_halt(),
            Self::BeginInterruptHandlingOrSkip { steps } => {
                expansions::begin_interrupt_handling_or_skip(steps)
            }
            Self::NextImmediateAddr => expansions::next_immediate_addr(),
            Self::ExecuteOpcode => expansions::execute_opcode(),
            Self::HlInc => expansions::hl_inc(),
            Self::HlDec => expansions::hl_dec(),
            Self::SpInc => expansions::sp_inc(),
            Self::DecSp => expansions::dec_sp(),
            Self::JumpRst(addr) => expansions::jump_rst(addr),
        }
    }

    /// Creates microcode for beginning the Interrupt Service Routine. If ISR occurs, the
    /// provided code will be executed after `BeginInterruptHandlingOrSkip`, and if ISR
    /// doesn't occur, the `rest_of_isr` will be skipped.
    ///
    /// In the `rest_of_isr`, there will be two u16 pushed onto the microcode stack. The
    /// top of the stack will be the return address that should be pushed to the gameboy
    /// stack and the second value will be the address the interrupt should jump to.
    ///
    /// A single yield will also be executed during the setup. Any additional yields must
    /// be performed seperately.
    pub fn begin_interrupt_handling_and(
        rest_of_isr: impl Into<MicrocodeBuilder>,
    ) -> MicrocodeBuilder {
        let rest_of_isr = rest_of_isr.into();
        MicrocodeBuilder::first(Self::BeginInterruptHandlingOrSkip {
            steps: rest_of_isr.len(),
        })
        .then(rest_of_isr)
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
