use std::error::Error;
use bitflags::bitflags;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext, MaskableMem};

bitflags! {
    /// Lcd control status flags
    #[derive(Default)]
    pub struct LcdStat: u8 {
        const READ_WRITE = 0b1111000;

        const MODE = 0b0000011;
        const Y_COINCIDENCE = 0b0000100;

        const HBLANK_INTERRUPT_ENABLE = 0b0001000;
        const VBLANK_INTERRUPT_ENABLE = 0b0010000;
        const OAM_INTERRUPT_ENABLE = 0b0100000;
        const Y_COINCIDENCE_INTERRUPT_ENABLE = 0b1000000;
    }
}

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

    fn vram(&self) -> &MaskableMem<0x2000>;
    fn vram_mut(&mut self) -> &mut MaskableMem<0x2000>;

    fn oam(&self) -> &MaskableMem<160>;
    fn oam_mut(&mut self) -> &mut MaskableMem<160>;
}

/// Allows core ppu implementation to be agnostic of render implementation
pub trait PpuBackend {
  /// Receive a screen buffer in the form of a list of RGBA quads and forward to the rendering
  /// backend in whatever format is appropriate.
  fn process_buffer(&self, screen_buffer: &[u8]) -> Result<(), &dyn Error>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PpuState {
  screen_buffer: [u8; 92160],
  scanline_progress: u64
}

impl Default for PpuState {
  fn default() -> PpuState {
    PpuState {
      screen_buffer: [0xff; 92160],
      scanline_progress: 0,
    }
  }
}

pub fn tick(ctx: &mut impl PpuContext, tcycles: u64) {
}
