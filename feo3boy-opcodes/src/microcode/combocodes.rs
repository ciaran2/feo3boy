//! Combo-codes are helper types which allow you to specify common sequences of microcode
//! operations using a single value.
//!

use crate::compiler::instr::builder::{InstrBuilder, MicrocodeReadable, MicrocodeWritable};
use crate::microcode::args::Reg16;
use crate::microcode::Microcode;

/// 8-bit read helper struct. Expands to a yield followed by an 8-bit read or write
/// operation.
pub struct Mem;

impl MicrocodeReadable for Mem {
    fn to_read(self) -> InstrBuilder {
        InstrBuilder::r#yield().then(Microcode::ReadMem)
    }
}

impl MicrocodeWritable for Mem {
    fn to_write(self) -> InstrBuilder {
        InstrBuilder::r#yield().then(Microcode::WriteMem)
    }
}

/// 8-bit immediate value. Expands to code to fetch an 8-bit value from memory and
/// increment the program counter.
pub struct Immediate8;

impl MicrocodeReadable for Immediate8 {
    fn to_read(self) -> InstrBuilder {
        // Fetch the PC.
        // stack: ...|pcl|pch|
        InstrBuilder::read(Reg16::Pc)
            // Copy the PC so we can increment it.
            // stack: ...|pcl|pch|pcl|pch|
            .then(Microcode::Dup16)
            // Increment the program counter.
            // stack: ...|pcl|pch|PCL|PCH|
            .then(Microcode::Inc16)
            // Write back the new PC value.
            // stack: ...|pcl|pch|
            .then_write(Reg16::Pc)
            .then_yield()
            // Pop the address and use it to read.
            .then(Microcode::ReadMem)
    }
}

/// 16-bit immediate value. Expands to code to fetch two 8-bit immediate values
/// sequentially.
///
/// Endianness is correct because we always push the lower byte of a LE word to the
/// microcode stack first followed by the higher byte, which always keeps the word in
/// gameboy LE byte order regardless of the running system endianness.
pub struct Immediate16;

impl MicrocodeReadable for Immediate16 {
    fn to_read(self) -> InstrBuilder {
        InstrBuilder::read(Immediate8).then_read(Immediate8)
    }
}

/// Helper for reading the stack pointer and incrementing it.
///
/// When used as a [`MicrocodeReadable`], `IncSp` will load the value of the stack pointer
/// to the microcode stack and then increment the stack pointer in-place.
pub struct SpInc;

impl MicrocodeReadable for SpInc {
    fn to_read(self) -> InstrBuilder {
        // Get the current stack pointer.
        // stack: ...|spl|sph|
        InstrBuilder::read(Reg16::Sp)
            // Copy the SP so we keep the old value as the result.
            // stack: ...|spl|sph|spl|sph|
            .then(Microcode::Dup16)
            // Increment the stack pointer.
            // stack: ...|spl|sph|SPL|SPH|
            .then(Microcode::Inc16)
            // Write the incremented value back to the register.
            // stack: ...|spl|sph|
            .then_write(Reg16::Sp)
    }
}

/// Helper for decrementing the stack pointer and reading it.
///
/// When used as a [`MicrocodeReadable`], `DecSp` will decrement the value of the stack
/// pointer in place and read the value into the microcode stack.
pub struct DecSp;

impl MicrocodeReadable for DecSp {
    fn to_read(self) -> InstrBuilder {
        // Get the current stack pointer.
        // stack: ...|SPL|SPH|
        InstrBuilder::read(Reg16::Sp)
            // Decrement the stack pointer.
            // stack: ...|spl|sph|
            .then(Microcode::Dec16)
            // Copy the SP so we still have a copy on the microcode stack after putting it
            // back in the register.
            // stack: ...|spl|sph|spl|sph|
            .then(Microcode::Dup16)
            // Write the decremented value back to the register.
            // stack: ...|spl|sph|
            .then_write(Reg16::Sp)
    }
}

/// Helper struct for manipulating the GameBoy stack from microcode.
pub struct GbStack16;

/// When used as a [`MicrocodeReadable`], `GbStack16` acts as a `pop` operation. A 16 bit
/// value will be read out from the location of the stack pointer, and the stack pointer
/// will be incremented by 2 (the stack starts at the end of memory and moves down).
impl MicrocodeReadable for GbStack16 {
    fn to_read(self) -> InstrBuilder {
        // Get the SP incremented address.
        // stack: ...|spl|sph|
        InstrBuilder::read(SpInc)
            // Yield a cycle and then read from the sp address on the stack to get the
            // low-order byte of the value being popped.
            // stack: ...|vl |
            .then_read(Mem)
            // Fetch and increment the stack pointer again.
            // stack: ...|vl |spl|sph|
            .then_read(SpInc)
            // Yield a cycle and then read from the sp address on the stack to get the
            // high-order byte of the value being popped.
            // stack: ...|vl |vh |
            .then_read(Mem)
    }
}

/// When used as a [`MicrocodeWritable`], `GbStack16` acts as a `push` operation. A 16 bit
/// value will be popped from the microcode stack, the stack pointer will be decremented
/// by 2, and the value will be written to the new location of the stack pointer.
impl MicrocodeWritable for GbStack16 {
    fn to_write(self) -> InstrBuilder {
        // stack: ...|vl |vh |
        // Get the SP decremented address.
        // stack: ...|vl |vh |spl|sph|
        InstrBuilder::read(DecSp)
            // Yield a cycle and then write to the sp address on the stack to write the
            // high-order byte of the value being pushed.
            // stack: ...|vl |
            .then_write(Mem)
            // Fetch and decrement the stack pointer again.
            // stack: ...|vl |spl|sph|
            .then_read(DecSp)
            // Yield a cycle and then write to the sp address on the stack to write the
            // low-order byte of the value being pushed.
            // stack: ...|
            .then_write(Mem)
    }
}

/// Helper for reading the HL and incrementing it.
///
/// When used as a [`MicrocodeReadable`], `HlInc` load the value of HL, and then increment
/// the HL register in-place.
pub struct HlInc;

impl MicrocodeReadable for HlInc {
    fn to_read(self) -> InstrBuilder {
        // Grab the current value of HL.
        // stack: ...|l|h|
        InstrBuilder::read(Reg16::HL)
            // Copy the value so we can increment it.
            // stack: ...|l|h|l|h|
            .then(Microcode::Dup16)
            // Increment the value
            // stack: ...|l|h|L|H|
            .then(Microcode::Inc16)
            // Write the new value to HL, leaving the original value.
            // stack: ...|l|h|
            .then_write(Reg16::HL)
    }
}

/// Helper for reading HL and decrementing it.
///
/// When used as a [`MicrocodeReadable`], `HlDec` load the value of HL, and then decrement
/// the HL register in-place.
pub struct HlDec;

impl MicrocodeReadable for HlDec {
    fn to_read(self) -> InstrBuilder {
        // Grab the current value of HL.
        // stack: ...|L|H|
        InstrBuilder::read(Reg16::HL)
            // Copy the value so we can decrement it.
            // stack: ...|L|H|L|H|
            .then(Microcode::Dup16)
            // Increment the value
            // stack: ...|L|H|l|h|
            .then(Microcode::Dec16)
            // Write the new value to HL, leaving the original value.
            // stack: ...|L|H|
            .then_write(Reg16::HL)
    }
}

/// Provides the CPU's halt-handling behavior.
pub struct HandleHalt;

impl From<HandleHalt> for InstrBuilder {
    fn from(_: HandleHalt) -> Self {
        // stack: ...|
        // Load the halt flag onto the microcode stack.
        // stack: ...|halt|
        InstrBuilder::first(Microcode::CheckHalt)
            // Branch based on if the value is true:
            // stack: ...|
            .then_if_true(
                // Read the set of active+enabled interrupts.
                // stack: ...|int |
                InstrBuilder::first(Microcode::GetActiveInterrupts)
                    // Branch based on if there are any active interrupts.
                    // stack: ...|
                    .then_cond(
                        // If there are active interrupts, clear the halt.
                        Microcode::ClearHalt,
                        // Otherwise, yield and restart the instruction fetch routine.
                        // Yield is needed here because we haven't fetched an instruction
                        // yet, so we need to wait for another 1m tick without fetching.
                        InstrBuilder::r#yield()
                            // This acts like a return, ending the current Instruction.
                            .then(Microcode::FetchNextInstruction),
                    ),
            )
    }
}

/// Helper to build microcode to service an interrupt.
///
/// Checks if an interrupt should be serviced, and if so performs the hidden isr
/// instruction to jump to the interrupt handler. Runs FetchNextInstruction when done to
/// reset and load the instruction at the microcode handler address
pub struct ServiceInterrupt;

impl From<ServiceInterrupt> for InstrBuilder {
    fn from(_: ServiceInterrupt) -> Self {
        // Retrieve the value of the IME
        // stack: ...|ime|
        InstrBuilder::first(Microcode::CheckIme)
            // If IME is true,
            // stack: ...|
            .then_if_true(
                // Retrieve any active and enabled interrupts.
                // stack: ...|ai|
                InstrBuilder::first(Microcode::GetActiveInterrupts)
                    // If there are any active and enabled interrupts,
                    // stack: ...|
                    .then_if_true(
                        // Get the address of the interrupt we are jumping to.
                        // stack: ...|intl|inth|
                        InstrBuilder::first(Microcode::PopInterrupt)
                            .then(Microcode::DisableInterrupts)
                            // Get the current PC.
                            // stack: ...|intl|inth|pcl|pch|
                            .then_read(Reg16::Pc)
                            // Get whether the halt-bug is active.
                            // stack: ...|intl|inth|pcl|pch|hb|
                            .then(Microcode::PopHaltBug)
                            // If the halt bug is active, decrement the PC value.
                            // stack: ...|intl|inth|pcl|pch|
                            .then_if_true(Microcode::Dec16)
                            .then_yield()
                            .then_yield()
                            // Write the value of the PC to the GameBoy stack.
                            // stack: ...|intl|inth|
                            .then_write(GbStack16)
                            .then_yield()
                            // Write the interrupt destination to the PC.
                            // stack: ...|
                            .then_write(Reg16::Pc)
                            .then(Microcode::FetchNextInstruction),
                    ),
            )
    }
}

/// Helper to provide microcode for LoadAndExecute of an instruction, including halt-bug
/// handling and enabling IME state ticking.
pub struct LoadAndExecute;

impl From<LoadAndExecute> for InstrBuilder {
    fn from(_: LoadAndExecute) -> Self {
        // Load the instruction onto the stack.
        // stack: ...|opcode|
        InstrBuilder::read(Immediate8)
            // Tell the CPU we are about to process a real instruction now, so we should
            // tick the IME after this.
            .then(Microcode::TickImeOnEnd)
            // Before executing, check if the haltbug is triggered, and if so, decrement
            // the PC.
            // stack: ...|opcode|hb|
            .then(Microcode::PopHaltBug)
            // Pop the haltbug flag and process.
            // stack: ...|opcode|
            .then_if_true(
                // If true, push the PC, decrement it, and pop it back into the PC.
                InstrBuilder::read(Reg16::Pc)
                    .then(Microcode::Dec16)
                    .then_write(Reg16::Pc),
            )
            // Pop the opcode off the stack and parse it, replacing this instruction with
            // that opcode.
            // stack: ...|
            .then(Microcode::ParseOpcode)
    }
}
