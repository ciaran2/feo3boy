//! Provides the direct-execution implementations of combo-codes.

use crate::gbz80core::microcode::combocodes::ComboCode;
use crate::gbz80core::microcode::r#impl as baseimpl;
use crate::gbz80core::microcode::MicrocodeFlow;
use crate::gbz80core::CpuContext;
use crate::interrupts::Interrupts;

impl ComboCode {
    pub(in crate::gbz80core::microcode) fn eval(self, ctx: &mut impl CpuContext) -> MicrocodeFlow {
        match self {
            Self::HandleHalt => handle_halt(ctx),
        }
    }
}

/// Implements Halt handling.
fn handle_halt(ctx: &mut impl CpuContext) -> MicrocodeFlow {
    if ctx.cpu().halted {
        if ctx.interrupts().active().is_empty() {
            // Revert to the start of instruction handling.
            baseimpl::fetch_next_instruction(ctx);
            return MicrocodeFlow::Yield1m;
        }
        ctx.cpu_mut().halted = false;
    }
    MicrocodeFlow::Continue
}
