//! Contains the code-generated stepping executor.

use feo3boy_executor_generator::define_state_executor;

use crate::gbz80core::externdefs;

define_state_executor!(
    /// An executor which allows sub-stepping like the
    /// [MicrocodeExecutor][crate::gbz80core::microcode_executor::MicrocodeExecutor], but
    /// is code generated to use a minimum number of states and less interpretation, so is
    /// much more performant.
    ///
    /// In this executor, states only change whenever there is a
    /// [`Yield`][feo3boy_opcodes::microcode::Microcode::Yield] or
    /// [`FetchNextInstruction`][feo3boy_opcodes::microcode::Microcode::FetchNextInstruction].
    ///
    /// Where the `MicrocodeExecutor` executor uses a [`Vec`] as its stack to store bytes
    /// between microcodes, the microcode stack here exists only at compile time. All
    /// values stored on the microcode stack are instead placed either in rust variables
    /// or in fields of the executor's state.
    pub SteppingExecutor,
    externs = [
        ReadReg => externdefs::read_reg,
        WriteReg => externdefs::write_reg,
        ReadReg16 => externdefs::read_reg16,
        WriteReg16 => externdefs::write_reg16,
        ReadMem => externdefs::read_mem,
        WriteMem => externdefs::write_mem,
        GetFlagsMasked => externdefs::get_flags_masked,
        SetFlagsMasked => externdefs::set_flags_masked,
        Halt => externdefs::halt,
        EnableInterrupts => externdefs::enable_interrupts,
        DisableInterrupts => externdefs::disable_interrupts,
        CheckHalt => externdefs::check_halt,
        ClearHalt => externdefs::clear_halt,
        CheckIme => externdefs::check_ime,
        GetActiveInterrupts => externdefs::get_active_interrupts,
        PopInterrupt => externdefs::pop_interrupt,
        PopHaltBug => externdefs::pop_halt_bug,
    ],
);

executor_tests! {
    tests,
    crate::gbz80core::stepping_executor::SteppingExecutor,
    crate::gbz80core::TestGb<Box<[u8; 0x10000]>, crate::gbz80core::stepping_executor::SteppingExecutorState>
}
