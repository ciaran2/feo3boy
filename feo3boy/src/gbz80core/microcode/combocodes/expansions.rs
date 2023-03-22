//! Provides the expansions of various
//! [`ComboCode`s][crate::gbz80core::microcode::combocodes::ComboCode].

use crate::gbz80core::microcode::{Microcode, MicrocodeBuilder, Reg16};

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

/// Provides the equivalent expansion of the `BeginInterruptHandlingOrSkip` `ComboCode`.
pub(super) fn begin_interrupt_handling_or_skip(steps: usize) -> MicrocodeBuilder {
    // steps is the number of steps in the *rest* of the ISR handler. That's the number we
    // need to skip over if we get through all the initial conditions as "true"

    // This value is the part that pushes the interrupt handler address and checks the
    // halt bug and pushes the program counter onto the microcode stack.
    //
    // First remove the interrupt from the interrupt vector, pushing the target address
    // onto the microcode stack.
    // stack: ...|intl|inth|
    let compute_isr_addr_and_ret_pc = MicrocodeBuilder::first(Microcode::PopInterrupt)
        // Turn off IME to prevent further interrupts.
        .then(Microcode::DisableInterrupts)
        // Get the current program counter.
        // stack: ...|intl|inth|pcl |pch |
        .then_read(Reg16::Pc)
        // Get whether the halt-bug is active and clear it.
        // stack: ...|intl|inth|pcl |pch |hb  |
        .then(Microcode::PopHaltBug)
        // Branch based on whether the halt-bug is active.
        // stack: ...|intl|inth|pcl |pch |
        // If true, decrement the PC, otherwise leave PC as-is.
        // stack: ...|intl|inth|pcl |pch |
        .then(MicrocodeBuilder::if_true(Microcode::Dec16))
        // `BeginInterruptHandlingOrSkip` also supplies the first yield of the ISR
        // routine.
        .then_yield();

    // Read the "active interrupts" register, so we can branch if any interrupts are
    // active.
    let check_active_interupts_and_start_isr =
        MicrocodeBuilder::first(Microcode::GetActiveInterrupts)
            // If interrupts are not active, bypass the ISR setup step
            // (`compute_isr_addr_and_ret_pc`) and whatever else was intended to be part
            // of ISR routine (the number of steps specified in `steps`).
            .then(Microcode::SkipIfNot {
                steps: compute_isr_addr_and_ret_pc.len() + steps,
            })
            .then(compute_isr_addr_and_ret_pc);

    MicrocodeBuilder::first(Microcode::CheckIme)
        // If IME is not enabled, skip over all the steps of
        // `check_active_interrupts_and_start_isr` as well as the steps of the rest of the
        // ISR routine that are specified by the `steps` argument of the ComboCode.
        .then(Microcode::SkipIfNot {
            steps: check_active_interupts_and_start_isr.len() + steps,
        })
        .then(check_active_interupts_and_start_isr)
}

/// Expansion for `NextImmediateAddr`.
pub(super) fn next_immediate_addr() -> MicrocodeBuilder {
    // Fetch the PC.
    // stack: ...|pcl|pch|
    MicrocodeBuilder::read(Reg16::Pc)
        // Copy the PC so we can increment it.
        // stack: ...|pcl|pch|pcl|pch|
        .then(Microcode::Dup(2))
        // Increment the program counter.
        // stack: ...|pcl|pch|PCL|PCH|
        .then(Microcode::Inc16)
        // Write back the new PC value.
        // stack: ...|pcl|pch|
        .then_write(Reg16::Pc)
        .then_yield()
}

/// Expansion for `ExecuteOpcode`.
pub(super) fn execute_opcode() -> MicrocodeBuilder {
    // This takes the opcode on the stack as an argument.
    // stack: ...|opcode|
    // Tell the CPU we are about to process a real instruction now, so we should tick
    // the IME after this.
    MicrocodeBuilder::first(Microcode::TickImeOnEnd)
        // Before executing, check if the haltbug is triggered, and if so, decrement
        // the PC.
        // stack: ...|opcode|hb|
        .then(Microcode::PopHaltBug)
        // Pop the haltbug flag and process.
        // stack: ...|opcode|
        .then(MicrocodeBuilder::if_true(
            // If true, push the PC, decrement it, and pop it back into the PC.
            MicrocodeBuilder::read(Reg16::Pc)
                .then(Microcode::Dec16)
                .then_write(Reg16::Pc),
        ))
        // Pop the opcode off the stack and parse it, replacing this instruction with
        // that opcode.
        // stack: ...|
        .then(Microcode::ParseOpcode)
}

/// Fetch and increment HL.
pub(super) fn hl_inc() -> MicrocodeBuilder {
    // Grab the current value of HL.
    // stack: ...|l|h|
    MicrocodeBuilder::read(Reg16::HL)
        // Copy the value so we can increment it.
        // stack: ...|l|h|l|h|
        .then(Microcode::Dup(2))
        // Increment the value
        // stack: ...|l|h|L|H|
        .then(Microcode::Inc16)
        // Write the new value to HL, leaving the original value.
        // stack: ...|l|h|
        .then_write(Reg16::HL)
}

/// Fetch and decrement HL.
pub(super) fn hl_dec() -> MicrocodeBuilder {
    // Grab the current value of HL.
    // stack: ...|L|H|
    MicrocodeBuilder::read(Reg16::HL)
        // Copy the value so we can decrement it.
        // stack: ...|L|H|L|H|
        .then(Microcode::Dup(2))
        // Increment the value
        // stack: ...|L|H|l|h|
        .then(Microcode::Dec16)
        // Write the new value to HL, leaving the original value.
        // stack: ...|L|H|
        .then_write(Reg16::HL)
}

/// Fetch and increment the stack pointer.
pub(super) fn sp_inc() -> MicrocodeBuilder {
    // Get the current stack pointer.
    // stack: ...|spl|sph|
    MicrocodeBuilder::read(Reg16::Sp)
        // Copy the SP so we keep the old value as the result.
        // stack: ...|spl|sph|spl|sph|
        .then(Microcode::Dup(2))
        // Increment the stack pointer.
        // stack: ...|spl|sph|SPL|SPH|
        .then(Microcode::Inc16)
        // Write the incremented value back to the register.
        // stack: ...|spl|sph|
        .then_write(Reg16::Sp)
}

/// Decrement and fetch the stack pointer.
pub(super) fn dec_sp() -> MicrocodeBuilder {
    // Get the current stack pointer.
    // stack: ...|SPL|SPH|
    MicrocodeBuilder::read(Reg16::Sp)
        // Decrement the stack pointer.
        // stack: ...|spl|sph|
        .then(Microcode::Dec16)
        // Copy the SP so we still have a copy on the microcode stack after putting it
        // back in the register.
        // stack: ...|spl|sph|spl|sph|
        .then(Microcode::Dup(2))
        // Write the decremented value back to the register.
        // stack: ...|spl|sph|
        .then_write(Reg16::Sp)
}

/// Jumps to the address of an RST instruction.
pub(super) fn jump_rst(addrl: u8) -> MicrocodeBuilder {
    // Set the dest address into the PC.
    // initial stack: ...|
    // Push the low order bytes of the dest address.
    // stack: ...|addrl|
    MicrocodeBuilder::first(Microcode::Append(addrl))
        // Push the high order bytes of the dest address (always 0).
        // stack: ...|addrl|addrh|
        .then(Microcode::Append(0))
        // Set the PC to the specified address.
        .then_write(Reg16::Pc)
}
