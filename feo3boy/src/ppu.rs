use crate::bits::BitGroup;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{Addr, IoRegs, IoRegsContext, MemDevice, Oam, Vram};
use bitflags::bitflags;
use log::{debug, trace};
use std::collections::VecDeque;
use std::ops::Deref;

pub enum LcdMode {
    HBlank = 0,
    VBlank = 1,
    OamScan = 2,
    WriteScreen = 3,
}

bitflags! {
    /// Lcd control status flags
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[repr(transparent)]
    pub struct LcdStat: u8 {
        /// Mask for selecting the bits which are readable/writiable via memory access.
        const READ_WRITE = 0b111_1000;

        /// Bit 0 of `MODE`. This pseudo-flag allows `from_bits_truncate` to
        /// set this bit independently of the rest of the mode bits.
        const MODE_B0 = 0b000_0001;
        /// Bit 1 of `MODE`. This pseudo-flag allows `from_bits_truncate` to
        /// set this bit independently of the rest of the mode bits.
        const MODE_B1 = 0b000_0010;

        const Y_COINCIDENCE = 0b000_0100;

        const HBLANK_INTERRUPT_ENABLE = 0b000_1000;
        const VBLANK_INTERRUPT_ENABLE = 0b001_0000;
        const OAM_INTERRUPT_ENABLE = 0b010_0000;
        const Y_COINCIDENCE_INTERRUPT_ENABLE = 0b100_0000;
    }
}

impl LcdStat {
    const MODE: BitGroup = BitGroup(0b0000_0011);

    pub fn mode(&self) -> LcdMode {
        match Self::MODE.extract(self.bits()) {
            0 => LcdMode::HBlank,
            1 => LcdMode::VBlank,
            2 => LcdMode::OamScan,
            3 => LcdMode::WriteScreen,
            _ => panic!("Illegal mode number encountered. This should be impossible."),
        }
    }

    pub fn with_mode(mut self, mode: LcdMode) -> LcdStat {
        Self::MODE.apply_bits(&mut self, mode as u8);
        self
    }

    pub fn with_writeable(self, data: u8) -> LcdStat {
        (self - LcdStat::READ_WRITE) | (LcdStat::from_bits_truncate(data) & LcdStat::READ_WRITE)
    }
}

bitflags! {
    /// Available set of interrupt flags.
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[repr(transparent)]
    pub struct LcdFlags: u8 {
        const BG_DISPLAY = 0b00000001;

        const OBJ_DISPLAY_ENABLE = 0b00000010;

        const OBJ_SIZE = 0b00000100;

        const BG_TILE_MAP = 0b00001000;

        const BG_TILE_DATA = 0b00010000;

        const WINDOW_ENABLE = 0b00100000;

        const WINDOW_TILE_MAP = 0b01000000;

        const DISPLAY_ENABLE = 0b10000000;
    }
}

impl LcdFlags {
    fn sprite_height(&self) -> u8 {
        if self.contains(Self::OBJ_SIZE) {
            16
        } else {
            8
        }
    }
}

bitflags! {
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[repr(transparent)]
    pub struct ObjAttrs: u8 {
        /// First bit of the CGB palette index. This flag exists only to allow
        /// `from_bits_truncate` to set this big independently.
        const CGB_PALETTE_B0 = 0b0000_0001;
        /// Second bit of the CGB palette index. This flag exists only to allow
        /// `from_bits_truncate` to set this big independently.
        const CGB_PALETTE_B1 = 0b0000_0010;
        /// Third bit of the CGB palette index. This flag exists only to allow
        /// `from_bits_truncate` to set this big independently.
        const CGB_PALETTE_B2 = 0b0000_0100;

        const CGB_VRAM_BANK  = 0b0000_1000;
        const PALETTE        = 0b0001_0000;
        const X_FLIP         = 0b0010_0000;
        const Y_FLIP         = 0b0100_0000;
        const UNDER_BG       = 0b1000_0000;
    }
}

impl ObjAttrs {
    /// Mask for selecting the bits corresponding to the CGB palette index.
    const CGB_PALETTE: BitGroup = BitGroup(0b0000_0111);

    pub fn cgb_palette(self) -> u8 {
        Self::CGB_PALETTE.extract(self.bits())
    }

    fn palette(self) -> usize {
        self.contains(Self::PALETTE) as usize
    }
}

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
struct Object {
    y: u8,
    x: u8,
    tile_id: u8,
    attrs: ObjAttrs,
}

impl Object {
    fn fetch_line(&self, ctx: &mut impl PpuContext) {
        let object_line = ctx.ioregs().lcdc_y() + 16 - self.y;
        let object_line = if self.attrs.contains(ObjAttrs::Y_FLIP) {
            ctx.ioregs().lcd_control().sprite_height() - object_line
        } else {
            object_line
        };

        let tile_offset = object_line / 8;
        let tile_id = self.tile_id + tile_offset;
        let tile_line = object_line % 8;

        let flip_x = self.attrs.contains(ObjAttrs::X_FLIP);
        let tile_line = get_tile_line(ctx, tile_id, tile_line, true, flip_x);

        let skip_pixels = ctx.ppu().obj_fifo.len();
        for pixel in &tile_line[skip_pixels..] {
            ctx.ppu_mut().obj_fifo.push_back((*pixel, self.attrs));
        }
    }
}

//impl PartialEq for Object {
//    fn eq(&self, other: &Self) -> bool { self.x == other.x }
//}
//
//impl Ord for Object {
//    fn cmp(&self, other: &Self) -> Ordering {
//        other.x.cmp(&self.x)
//    }
//}
//impl PartialOrd for Object {
//    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//        Some(self.cmp(other))
//    }
//}

impl Oam {
    fn get_object(&self, i: u16) -> Object {
        trace!("getting attributes for object: {}", i);
        let base_index = i * 4;
        Object {
            y: self.deref().read(Addr::from(base_index)),
            x: self.deref().read(Addr::from(base_index + 1)),
            tile_id: self.deref().read(Addr::from(base_index + 2)),
            attrs: ObjAttrs::from_bits_truncate(self.deref().read(Addr::from(base_index + 3))),
        }
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait PpuContext: IoRegsContext + InterruptContext {
    /// Get the ppu state.
    fn ppu(&self) -> &PpuState;

    /// Get mutable access to the ppu state.
    fn ppu_mut(&mut self) -> &mut PpuState;

    fn vram(&self) -> &Vram;
    fn vram_mut(&mut self) -> &mut Vram;

    fn oam(&self) -> &Oam;
    fn oam_mut(&mut self) -> &mut Oam;

    fn display_ready(&mut self);
}

pub fn palette_lookup(palette: u8, color: u8) -> usize {
    ((palette & (3 << color * 2)) >> color * 2) as usize
}

fn get_tile_line(
    ctx: &impl PpuContext,
    tile_id: u8,
    line: u8,
    low_data: bool,
    flip_x: bool,
) -> [u8; 8] {
    let base_address = if low_data { 0x0 } else { 0x1000 };
    let tile_offset = if low_data {
        tile_id as i16
    } else {
        tile_id as i8 as i16
    };

    let line_address = (base_address + 16 * tile_offset + 2 * line as i16) as u16;

    let low_bits = ctx.vram().deref().read(Addr::from(line_address));
    let high_bits = ctx.vram().deref().read(Addr::from(line_address + 1));

    let mut output = [0; 8];
    for i in 0..=7 {
        let bit = if flip_x { i } else { 7 - i };
        output[i] = ((low_bits & (1 << bit)) >> bit) + (2 * ((high_bits & (1 << bit)) >> bit));
    }

    output
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PpuState {
    truecolor_palette: [(u8, u8, u8); 4],
    screen_buffer: [(u8, u8, u8); 23040],
    scanline_ticks: u64,
    object_cursor: u16,
    scanline_x: u8,
    fetcher_x: u16,
    obj_queue: VecDeque<Object>,
    bg_fifo: VecDeque<u8>,
    obj_fifo: VecDeque<(u8, ObjAttrs)>,
    bg_discard: u8,
    in_window: bool,
}

impl PpuState {
    pub fn new() -> PpuState {
        Default::default()
    }

    pub fn screen_buffer(&self) -> &[(u8, u8, u8)] {
        &self.screen_buffer
    }

    pub fn scanline_reset(&mut self) {
        self.bg_fifo.clear();
        self.obj_fifo.clear();
        self.obj_queue.clear();
        self.scanline_ticks -= 456;
        self.object_cursor = 0;
        self.scanline_x = 0;
        self.fetcher_x = 0;
        self.bg_discard = 0;
        self.in_window = false;
    }
}

impl Default for PpuState {
    fn default() -> PpuState {
        PpuState {
            truecolor_palette: [
                (0xff, 0xff, 0xff),
                (0xd3, 0xd3, 0xd3),
                (0x5a, 0x5a, 0x5a),
                (0x00, 0x00, 0x00),
            ],
            screen_buffer: [(0xff, 0xff, 0xff); 23040],
            scanline_ticks: 0,
            object_cursor: 0,
            scanline_x: 0,
            fetcher_x: 0,
            obj_queue: VecDeque::new(),
            bg_fifo: VecDeque::new(),
            obj_fifo: VecDeque::new(),
            bg_discard: 0,
            in_window: false,
        }
    }
}

pub fn bg_tile_fetch(ctx: &mut impl PpuContext) {
    let tile_map_flag = if ctx.ppu().in_window {
        LcdFlags::WINDOW_TILE_MAP
    } else {
        LcdFlags::BG_TILE_MAP
    };

    let scroll_offset_x = if ctx.ppu().in_window {
        0
    } else {
        ctx.ioregs().scroll_x() as u16 / 8
    };

    let tile_map_y = if ctx.ppu().in_window {
        (ctx.ioregs().lcdc_y() - ctx.ioregs().window_y()) as u16
    } else {
        ctx.ioregs().lcdc_y().wrapping_add(ctx.ioregs().scroll_y()) as u16
    };

    let tile_id_base = if ctx.ioregs().lcd_control().contains(tile_map_flag) {
        0x1c00
    } else {
        0x1800
    };

    let tile_id_offset =
        ((tile_map_y as u16 / 8) & 0x1f) * 32 + ((ctx.ppu().fetcher_x + scroll_offset_x) & 0x1f);

    let tile_id = ctx
        .vram()
        .deref()
        .read(Addr::from(tile_id_base + tile_id_offset));

    if ctx.ioregs().lcdc_y() / 8 == 0 {
        debug!(
            "tile_id_offset: {}, fetcher_x: {}, scroll_offset: {}, tile_id: {:x}",
            tile_id_offset,
            ctx.ppu().fetcher_x,
            scroll_offset_x,
            tile_id
        );
    }

    let line = (tile_map_y & 0x7) as u8;

    trace!("Fetching line {} of tile {}", line, tile_id);

    let tile_line = get_tile_line(
        ctx,
        tile_id,
        line,
        ctx.ioregs().lcd_control().contains(LcdFlags::BG_TILE_DATA),
        false,
    );

    ctx.ppu_mut().bg_fifo.extend(&tile_line);

    ctx.ppu_mut().fetcher_x += 1;
}

pub fn tick(ctx: &mut impl PpuContext, tcycles: u64) {
    if ctx
        .ioregs()
        .lcd_control()
        .contains(LcdFlags::DISPLAY_ENABLE)
    {
        ctx.ppu_mut().scanline_ticks += tcycles;
        let lcd_stat = ctx.ioregs().lcd_stat();
        let mut lcdc_y = ctx.ioregs().lcdc_y();
        trace!("LCD is enabled.");
        trace!("LCD status {:b}", lcd_stat);
        trace!("Current scanline: {}", lcdc_y);
        trace!("Scanline progress: {}", ctx.ppu().scanline_ticks);

        match ctx.ioregs().lcd_stat().mode() {
            LcdMode::HBlank => {
                trace!("HBlank");
                if ctx.ppu().scanline_ticks > 456 {
                    debug!("End of scan line {}", lcdc_y);
                    ctx.ppu_mut().scanline_reset();
                    lcdc_y += 1;
                    ctx.ioregs_mut().set_lcdc_y(lcdc_y);

                    if lcdc_y == ctx.ioregs().lcdc_y_compare()
                        && lcd_stat.contains(LcdStat::Y_COINCIDENCE_INTERRUPT_ENABLE)
                    {
                        ctx.interrupts_mut().send(InterruptFlags::STAT);
                    }

                    if lcdc_y < 144 {
                        debug!("Video mode transition from 0 (HBlank) to 2 (OAMScan)");
                        ctx.oam_mut().mask();
                        ctx.ioregs_mut()
                            .set_lcd_stat(lcd_stat.with_mode(LcdMode::OamScan));
                        ctx.ppu_mut().bg_discard = ctx.ioregs().scroll_x() & 0x7;
                        if lcd_stat.contains(LcdStat::OAM_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    } else {
                        debug!("Video mode transition from 0 (HBlank) to 1 (VBlank)");
                        ctx.ioregs_mut()
                            .set_lcd_stat(lcd_stat.with_mode(LcdMode::VBlank));
                        ctx.interrupts_mut().send(InterruptFlags::VBLANK);
                        if lcd_stat.contains(LcdStat::VBLANK_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                        ctx.display_ready();
                    }
                }
            }
            LcdMode::VBlank => {
                trace!("VBlank");
                if ctx.ppu().scanline_ticks > 456 {
                    debug!("End of scan line {}", lcdc_y);
                    ctx.ppu_mut().scanline_ticks -= 456;
                    lcdc_y += 1;
                    ctx.ioregs_mut().set_lcdc_y(lcdc_y);

                    if lcdc_y > 153 {
                        debug!("Video mode transition from 1 (VBlank) to 2 (OAMScan)");
                        ctx.ioregs_mut().set_lcdc_y(0);
                        ctx.ioregs_mut()
                            .set_lcd_stat(lcd_stat.with_mode(LcdMode::OamScan));
                        ctx.ppu_mut().bg_discard = ctx.ioregs().scroll_x() & 0x7;
                        if lcd_stat.contains(LcdStat::OAM_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    }
                }
            }
            LcdMode::OamScan => {
                trace!("OAMScan");

                let end_object = ctx.ppu().scanline_ticks as u16 / 2;

                for i in ctx.ppu().object_cursor..end_object {
                    if ctx.ppu().obj_queue.len() < 10 && i < 40 {
                        let object = ctx.oam().get_object(i);
                        let sprite_height = ctx.ioregs().lcd_control().sprite_height();

                        if lcdc_y + 16 >= object.y && lcdc_y + 16 < object.y + sprite_height {
                            let ppu = ctx.ppu_mut();
                            let mut insert_index = ppu.obj_queue.len();
                            for (i, queued_object) in ppu.obj_queue.iter().enumerate() {
                                if object.x < queued_object.x {
                                    insert_index = i;
                                    break;
                                }
                            }

                            ppu.obj_queue.insert(insert_index, object);
                        }
                    } else {
                        break;
                    }
                }

                ctx.ppu_mut().object_cursor = end_object;

                if ctx.ppu().scanline_ticks > 80 {
                    debug!("Video mode transition from 2 (OAMScan) to 3 (WriteScreen)");
                    ctx.vram_mut().mask();
                    ctx.ioregs_mut()
                        .set_lcd_stat(lcd_stat.with_mode(LcdMode::WriteScreen))
                }
            }
            LcdMode::WriteScreen => {
                trace!("WriteScreen");

                for _i in 0..tcycles {
                    if ctx.ioregs().lcd_control().contains(LcdFlags::WINDOW_ENABLE)
                        && ctx.ppu().scanline_x + 7 == ctx.ioregs().window_x()
                        && lcdc_y >= ctx.ioregs().window_y()
                    {
                        ctx.ppu_mut().bg_fifo.clear();
                        ctx.ppu_mut().fetcher_x = 0;
                        ctx.ppu_mut().in_window = true;
                    }

                    if ctx.ppu().bg_fifo.len() < 8 {
                        bg_tile_fetch(ctx);
                    }

                    if !ctx.ppu().obj_queue.is_empty()
                        && ctx.ppu().obj_queue[0].x == ctx.ppu().scanline_x + 8
                    {
                        ctx.ppu_mut().obj_queue.pop_front().unwrap().fetch_line(ctx);
                    }

                    if !ctx.ppu().in_window && ctx.ppu().scanline_x == 0 {
                        for _i in 0..ctx.ppu().bg_discard {
                            ctx.ppu_mut().bg_fifo.pop_front();
                        }
                    }

                    //if lcdc_y == 15 && ctx.ppu().scanline_x % 8 == 0 {
                    //    info!("scanline_x: {}, bg_fifo: {:?}", ctx.ppu().scanline_x, ctx.ppu().bg_fifo);
                    //}
                    //if ctx.ppu().bg_fifo.len() > 8 {
                    //    info!("scanline_x: {}, bg_fifo: {:?}", ctx.ppu().scanline_x, ctx.ppu().bg_fifo);
                    //}

                    let buffer_index = lcdc_y as usize * 160 + ctx.ppu().scanline_x as usize;
                    let bg_pixel = ctx.ppu_mut().bg_fifo.pop_front().unwrap();
                    let obj_pixel = ctx.ppu_mut().obj_fifo.pop_front();

                    ctx.ppu_mut().screen_buffer[buffer_index] = {
                        let gb_color = match obj_pixel {
                            Some(obj_pixel) => {
                                if obj_pixel.0 == 0
                                    || obj_pixel.1.contains(ObjAttrs::UNDER_BG) && bg_pixel != 0
                                {
                                    palette_lookup(ctx.ioregs().bg_palette(), bg_pixel)
                                } else {
                                    let obj_palette = obj_pixel.1.palette();
                                    palette_lookup(
                                        ctx.ioregs().obj_palette(obj_palette),
                                        obj_pixel.0,
                                    )
                                }
                            }
                            None => palette_lookup(ctx.ioregs().bg_palette(), bg_pixel),
                        };
                        ctx.ppu().truecolor_palette[gb_color]
                    };

                    ctx.ppu_mut().scanline_x += 1;
                    if ctx.ppu().scanline_x > 159 {
                        debug!("Video mode transition from 3 (WriteScreen) to 0 (HBlank) after {} cycles.", ctx.ppu().scanline_ticks - 80);
                        ctx.oam_mut().unmask();
                        ctx.vram_mut().unmask();
                        ctx.ioregs_mut()
                            .set_lcd_stat(lcd_stat.with_mode(LcdMode::HBlank));
                        if lcd_stat.contains(LcdStat::HBLANK_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT)
                        }
                    }
                }
            }
        }
    } else {
        trace!("LCD is disabled.");
    }
}
