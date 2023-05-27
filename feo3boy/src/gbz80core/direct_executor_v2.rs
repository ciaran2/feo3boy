//! Contains the code-generated direct executor.

use feo3boy_executor_generator::define_direct_executor;

use crate::gbz80core::externdefs;

define_direct_executor!(
    /// New version of DirectExecutor which is code-generated from the microcode.
    pub DirectExecutorV2,
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
    crate::gbz80core::direct_executor_v2::DirectExecutorV2,
    crate::gbz80core::TestGb<Box<[u8; 0x10000]>, ()>
}
