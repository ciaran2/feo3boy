//! Provides the expansions of various
//! [`ComboCode`s][crate::gbz80core::microcode::combocodes::ComboCode].

use crate::gbz80core::microcode::{MicrocodeBuilder, Microcode};

/// Provides the equivalent expansion of the `HandleHalt` `ComboCode`.
pub(super) fn handle_halt() -> MicrocodeBuilder {
    // stack: ...|
    // Load the halt flag onto the microcode stack.
    // stack: ...|halt|
    MicrocodeBuilder::first(Microcode::CheckHalt)
        // Branch based on if the value is true:
        // stack: ...|
        .then(MicrocodeBuilder::if_true(
            // Read the set of active+enabled interrupts.
            // stack: ...|int |
            MicrocodeBuilder::first(Microcode::GetActiveInterrupts)
                // Branch based on if there are any active interrupts.
                // stack: ...|
                .then(MicrocodeBuilder::cond(
                    // If there are active interrupts, clear the halt.
                    Microcode::ClearHalt,
                    // Otherwise, yield and restart the instruction fetch routine.  Yield
                    // is needed here because we haven't fetched an instruction yet, so we
                    // need to wait for another 1m tick without fetching.
                    MicrocodeBuilder::r#yield()
                        // This acts like a return, ending the current Instruction.
                        .then(Microcode::FetchNextInstruction),
                )),
        ))
}
