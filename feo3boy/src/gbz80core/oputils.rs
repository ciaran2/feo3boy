//! Utilities for implementing operations.

use crate::gbz80core::ExecutorContext;
use crate::interrupts::Interrupts;

/// If `IME` is set, just halts the CPU until there is an interrupt available to be serviced. If
/// there's already an interrupt to be serviced, `HALT` is effectively a `NOP` and excution moved
/// directly to the interrupt handler then to the next instruction.
///
/// If `IME` is not set, there's a possibility of triggering a CPU bug:
/// *   If no interrupt is pending, the CPU still halts and resumes the next time an interrupt
///     becomes pending, but the interrupt just isn't handled because IME is off.
/// *   If there are enabled interrupts pending (`[IE] & [IF] != 0`), the bug is triggered:
///     *   In the normal case, the bug just prevents the program counter from incrementing
///         properly, so the byte after `HALT` will be read twice. (This presumably means that any
///         1-byte instruction will execute twice, and any two-byte instruction will read itself as
///         the following value, but that's somewhat unclear. Its also not clear what happens if an
///         `RST` is executed, since that's a 1 byte jump. Does overwritting the `PC` avert the bug?
///         Note that with any normal `CALL`, `JP`, or `JR`, the repeat byte would be used as the
///         jump target or offset instead.)
///     *   If `EI` executed just before `HALT` such that `IME` would become true after the `HALT`,
///         the bug is even weirder: the interrupt is serviced as normal, but then the interrupt
///         returns to `halt` which is executed again.
///
/// Implementing the behavior of preventing PC increment would require a bunch of complex extra
/// state which would have to be checked in a bunch of places, so for now this just panics if the
/// bug would be encountered.
pub(super) fn halt(ctx: &mut impl ExecutorContext) {
    if ctx.cpu().interrupt_master_enable.enabled() {
        // No need to special-case interrupts here, since the next `tick` call will un-halt anyway.
        ctx.cpu_mut().halted = true;
    } else {
        let enabled_interrupts = ctx.interrupts().enabled();
        let pending_interrupts = ctx.interrupts().queued();
        if enabled_interrupts.intersects(pending_interrupts) {
            // Halt doesn't actually happen, and instead the program counter will fail to
            // increment in the next step.
            ctx.cpu_mut().halt_bug = true;
        } else {
            // `tick` will un-halt next time ([IE] & [IF] != 0), but will not service the interrupt.
            ctx.cpu_mut().halted = true;
        }
    }
}
