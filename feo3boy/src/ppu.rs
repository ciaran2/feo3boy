use std::error::Error;
use bitflags::bitflags;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};

bitflags! {
    /// Available set of interrupt flags.
    #[derive(Default)]
    pub struct LcdFlags: u8 {
        const BG_DISPLAY = 0b00000001;

        const OBJ_DISPLAY_ENABLE = 0b00000010;

        const OBJ_SIZE = 0b00000100;

        const BG_TILE_MAP_SELECT = 0b00001000;

        const BG_TILE_DATA_SELECT = 0b00010000;

        const WINDOW_DISPLAY_ENABLE = 0b00100000;

        const WINDOW_TILE_MAP_SELECT = 0b01000000;

        const DISPLAY_ENABLE = 0b10000000;
    }
}

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

pub fn tick(ctx: &mut impl PpuContext, tcycles: u64) {
}
