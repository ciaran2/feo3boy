use std::error::Error;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};

/// Context trait providing access to fields needed to service graphics.
pub trait PpuContext: IoRegsContext + InterruptContext {
    /// Get the ppu state.
    fn ppu(&self) -> &PpuState;

    /// Get mutable access to the ppu state.
    fn ppu_mut(&mut self) -> &mut PpuState;
}

/// Allows core ppu implementation to be agnostic of render implementation
pub trait PpuBackend {
  /// Receive a screen buffer in the form of a list of RGBA quads and forward to the rendering
  /// backend in whatever format is appropriate.
  fn process_buffer(&self, screen_buffer: &[u8]) -> Result<(), &dyn Error>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PpuState {
  screen_buffer: [u8; 92160]
}

impl Default for PpuState {
  fn default() -> PpuState {
    PpuState {
      screen_buffer: [0xff; 92160],
    }
  }
}
