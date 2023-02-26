use std::error::Error;
use bitflags::bitflags;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext, MaskableMem};
use log::{debug, trace};


pub enum LcdMode {
    HBlank,
    VBlank,
    OamScan,
    WriteScreen,
}

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

impl LcdStat {

    pub fn get_mode(&self) -> LcdMode {
        match (*self | LcdStat::MODE).bits() {
            0 => LcdMode::HBlank,
            1 => LcdMode::VBlank,
            2 => LcdMode::OamScan,
            3 => LcdMode::WriteScreen,
            _ => panic!("Illegal mode number encountered. This should be impossible.")
        }
    }

    pub fn set_mode(&self, mode: LcdMode) -> LcdStat {
        (*self & !LcdStat::MODE) |
            LcdStat::from_bits_truncate(match mode {
                LcdMode::HBlank       => 0,
                LcdMode::VBlank       => 1,
                LcdMode::OamScan      => 2,
                LcdMode::WriteScreen  => 3
            })
    }

    pub fn set_writeable(&self, data: u8) -> LcdStat {
        (*self & !LcdStat::READ_WRITE) | 
            (LcdStat::from_bits_truncate(data) & LcdStat::READ_WRITE)
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

impl PpuState {
    pub fn new() -> PpuState {
        Default::default()
    }
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
    if ctx.ioregs().lcd_control().contains(LcdFlags::DISPLAY_ENABLE) {
        ctx.ppu_mut().scanline_progress += tcycles;
        let lcd_stat = ctx.ioregs().lcd_stat();
        let mut lcdc_y = ctx.ioregs().lcdc_y();
        trace!("LCD is enabled.");

        match ctx.ioregs().lcd_stat().get_mode() {
            LcdMode::HBlank       => {
                if ctx.ppu().scanline_progress > 456 {
                    ctx.ppu_mut().scanline_progress -= 456;
                    lcdc_y += 1;
                    ctx.ioregs_mut().set_lcdc_y(lcdc_y);

                    if lcdc_y == ctx.ioregs().lcdc_y_compare() &&
                        lcd_stat.contains(LcdStat::Y_COINCIDENCE_INTERRUPT_ENABLE) {
                        ctx.interrupts_mut().send(InterruptFlags::STAT);
                    }

                    if lcdc_y < 144 {
                        ctx.ioregs_mut().set_lcd_stat(lcd_stat.set_mode(LcdMode::OamScan));
                        if lcd_stat.contains(LcdStat::OAM_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    }
                    else {
                        ctx.ioregs_mut().set_lcd_stat(lcd_stat.set_mode(LcdMode::VBlank));
                        ctx.interrupts_mut().send(InterruptFlags::VBLANK);
                        if lcd_stat.contains(LcdStat::VBLANK_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    }
                }
            }
            LcdMode::VBlank       => {
                if ctx.ppu().scanline_progress > 456 {
                    ctx.ppu_mut().scanline_progress -= 456;
                    lcdc_y += 1;
                    ctx.ioregs_mut().set_lcdc_y(lcdc_y);

                    if lcdc_y > 153 {
                        ctx.ioregs_mut().set_lcd_stat(lcd_stat.set_mode(LcdMode::OamScan));
                        if lcd_stat.contains(LcdStat::OAM_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    }
                }
            }
            LcdMode::OamScan      => {
                if ctx.ppu().scanline_progress > 80 {
                    ctx.ioregs_mut().set_lcd_stat(lcd_stat.set_mode(LcdMode::WriteScreen))
                }
            }
            LcdMode::WriteScreen  => {
                //fixed cycles as a stand-in for now, should be variable based on sprites
                if ctx.ppu().scanline_progress > 248 {
                    ctx.ioregs_mut().set_lcd_stat(lcd_stat.set_mode(LcdMode::HBlank));
                    if lcd_stat.contains(LcdStat::HBLANK_INTERRUPT_ENABLE) {
                        ctx.interrupts_mut().send(InterruptFlags::STAT)
                    }
                }
            }
        }
    }
    else {
        trace!("LCD is disabled.");
    }
}
