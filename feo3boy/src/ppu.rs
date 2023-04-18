use crate::bits::BitGroup;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{
    MemContext, MemDevice, MemSource, MemValue, Oam, RelativeAddr, RootMemDevice, Vram,
};
use bitflags::bitflags;
use log::{debug, trace};
use std::collections::VecDeque;
use std::{mem, slice};

/// Address where OAM begins. Used when constructing addresses for directly accessing OAM,
/// in order to correctly register the raw address that is being accessed. Note that this
/// relies on the assumption that VRAM is being mapped in its standard location.
const VRAM_BEGIN: RelativeAddr = RelativeAddr::device_start(0x8000);
/// Address where OAM begins. Used when constructing addresses for directly accessing OAM,
/// in order to correctly register the raw address that is being accessed. Note that this
/// relies on the assumption that OAM is being mapped in its standard location.
const OAM_BEGIN: RelativeAddr = RelativeAddr::device_start(0xfe00);

pub enum LcdMode {
    HBlank = 0,
    VBlank = 1,
    OamScan = 2,
    WriteScreen = 3,
}

bitflags! {
    /// Lcd control status flags
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
    #[memdev(bitflags, writable = LcdStat::WRITABLE)]
    #[repr(transparent)]
    pub struct LcdStat: u8 {
        /// Mask for selecting the bits which are readable/writiable via memory access.
        const WRITABLE = 0b111_1000;

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

    pub fn set_mode(&mut self, mode: LcdMode) {
        Self::MODE.apply_bits(self, mode as u8);
    }
}

bitflags! {
    /// Available set of interrupt flags.
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
    #[memdev(bitflags)]
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
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Hash, MemDevice)]
    #[memdev(bitflags)]
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
// Repr C ensures that the order of fields matches the GB layout. We don't actually
// transmute from the equivalent array type, but doing this may allow the compiler to
// optimize to a transmute.
#[repr(C)]
struct Object {
    y: u8,
    x: u8,
    tile_id: u8,
    attrs: ObjAttrs,
}

impl Object {
    fn fetch_line(&self, ctx: &mut impl PpuContext) {
        let object_line = ctx.ppu_regs().lcdc_y + 16 - self.y;
        let object_line = if self.attrs.contains(ObjAttrs::Y_FLIP) {
            ctx.ppu_regs().lcd_control.sprite_height() - object_line
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

impl MemValue for Object {
    #[inline]
    fn copy_from_mem<S: MemSource>(&mut self, source: S) {
        // Treat self as a byte slice. This is safe because all of the fields are u8 or
        // #[repr(transparent)] around a u8.
        // ObjAttrs safely allows any u8 bit pattern (e.g. via `from_bits_retain`), so
        // filling it with invalid bits is OK. We just have to filter back to only allowed
        // flags afterwards.
        let slice =
            unsafe { slice::from_raw_parts_mut(self as *mut _ as *mut u8, mem::size_of::<Self>()) };
        source.get_bytes(slice);
        // Filter invalid attrs.
        self.attrs &= ObjAttrs::all();
    }

    #[inline]
    fn copy_to_mem<D: crate::memdev::MemDest>(&self, dest: D) {
        // Treat self as a byte slice. This is safe because all of the fields are u8 or
        // #[repr(transparent)] around a u8.
        let slice =
            unsafe { slice::from_raw_parts(self as *const _ as *const u8, mem::size_of::<Self>()) };
        dest.set_bytes(slice);
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
        self.bypass()
            .read_relative(OAM_BEGIN.move_forward_by(base_index))
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait PpuContext: InterruptContext + MemContext {
    /// Get the ppu state.
    fn ppu(&self) -> &PpuState;

    /// Get mutable access to the ppu state.
    fn ppu_mut(&mut self) -> &mut PpuState;

    fn ppu_regs(&self) -> &PpuRegs;
    fn ppu_regs_mut(&mut self) -> &mut PpuRegs;

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

    let [low_bits, high_bits]: [u8; 2] = ctx
        .vram()
        .bypass()
        .read_relative(VRAM_BEGIN.move_forward_by(line_address));

    let mut output = [0; 8];

    /// Extract the specified bit from low and high and form a new two-bit number using
    /// them.
    #[inline(always)]
    fn extract(low: u8, high: u8, bit: usize) -> u8 {
        ((low >> bit) & 1) | (((high >> bit) & 1) << 1)
    }
    if flip_x {
        for (i, bit) in (0..=7).enumerate() {
            output[i] = extract(low_bits, high_bits, bit);
        }
    } else {
        for (i, bit) in (0..=7).rev().enumerate() {
            output[i] = extract(low_bits, high_bits, bit);
        }
    }

    output
}

/// State of the Direct Memory Access system.
#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(C)]
pub struct DmaState {
    addr_low: u8,
    addr_high: u8,
}

impl Default for DmaState {
    fn default() -> Self {
        Self {
            // DMA stops after 0x9f, and is inactive by default so we start the low byte
            // of the DMA address at 0xa0.
            addr_low: 0xa0,
            /// The high byte of DMA is set by the program.
            addr_high: 0,
        }
    }
}

impl DmaState {
    #[inline]
    fn read_inner(&self) -> u8 {
        self.addr_high
    }

    #[inline]
    fn write_inner(&mut self, value: u8) {
        self.addr_low = 0;
        self.addr_high = value;
    }

    /// Gets the DMA base address.
    #[inline]
    pub fn base_addr(&self) -> u16 {
        u16::from_le_bytes([0, self.addr_high])
    }

    /// Returns true if DMA is active.
    #[inline]
    pub fn active(&self) -> bool {
        self.addr_low < 0xa0
    }

    /// Get the source address for the DMA read.
    ///
    /// This returns an absolute address within the GameBoy address space.
    #[inline]
    pub fn source_addr(&self) -> u16 {
        u16::from_le_bytes([self.addr_low, self.addr_high])
    }

    /// Gets the destination address for the DMA read.
    ///
    /// This is a relative address within OAM. The relative base is the default address
    /// that OAM is mapped to. Note that because of how RelativeAddr works, this address
    /// will be valid for a read directly from OAM no matter where OAM is mapped, however
    /// the "raw" address may be wrong if OAM is not mapped in its normal location.
    pub fn dest_addr(&self) -> RelativeAddr {
        OAM_BEGIN.move_forward_by(self.addr_low as u16)
    }

    /// Move forward by one byte. If the last byte in the DMA has been read, this makes
    /// the DMA inactive.
    ///
    /// This should not be called if DMA is not active.
    fn advance(&mut self) {
        // If called while DMA is not active, it can incorrectly wrap around and restart
        // DMA. For performance, this check is only used in Debug mode.
        debug_assert!(self.active());
        self.addr_low += 1;
    }
}

impl MemDevice for DmaState {
    const LEN: usize = 1;

    #[inline]
    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        check_addr!(DmaState, addr);
        self.read_inner()
    }

    #[inline]
    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        check_addr!(DmaState, addr, data.len());
        data[0] = self.read_inner();
    }

    #[inline]
    fn write_byte_relative(&mut self, addr: RelativeAddr, data: u8) {
        check_addr!(DmaState, addr);
        self.write_inner(data);
    }

    #[inline]
    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        check_addr!(DmaState, addr, data.len());
        self.write_inner(data[0]);
    }
}

/// Memory-mapped IO registers used by the PPU.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct PpuRegs {
    pub lcd_control: LcdFlags,
    pub lcd_status: LcdStat,
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub lcdc_y: u8,
    pub lcdc_y_compare: u8,
    /// DMAanager.
    pub dma: DmaState,
    pub bg_palette: u8,
    pub obj_palette: [u8; 2],
    pub window_y: u8,
    pub window_x: u8,
}

memdev_fields!(PpuRegs, len: 0x0c, {
    0x00 => lcd_control,
    0x01 => lcd_status,
    0x02 => scroll_y,
    0x03 => scroll_x,
    0x04 => { lcdc_y, readonly },
    0x05 => lcdc_y_compare,
    0x06 => dma,
    0x07 => bg_palette,
    0x08..=0x09 => obj_palette,
    0x0a => window_y,
    0x0b => window_x,
});

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
        ctx.ppu_regs().scroll_x as u16 / 8
    };

    let tile_map_y = if ctx.ppu().in_window {
        (ctx.ppu_regs().lcdc_y - ctx.ppu_regs().window_y) as u16
    } else {
        ctx.ppu_regs().lcdc_y.wrapping_add(ctx.ppu_regs().scroll_y) as u16
    };

    let tile_id_base = if ctx.ppu_regs().lcd_control.contains(tile_map_flag) {
        0x1c00
    } else {
        0x1800
    };

    let tile_id_offset =
        ((tile_map_y as u16 / 8) & 0x1f) * 32 + ((ctx.ppu().fetcher_x + scroll_offset_x) & 0x1f);

    let tile_id = ctx
        .vram()
        .bypass()
        .read_byte_relative(VRAM_BEGIN.move_forward_by(tile_id_base + tile_id_offset));

    if ctx.ppu_regs().lcdc_y / 8 == 0 {
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
        ctx.ppu_regs().lcd_control.contains(LcdFlags::BG_TILE_DATA),
        false,
    );

    ctx.ppu_mut().bg_fifo.extend(&tile_line);

    ctx.ppu_mut().fetcher_x += 1;
}

pub fn tick(ctx: &mut impl PpuContext, tcycles: u64) {
    tick_dma(ctx, tcycles);

    if ctx
        .ppu_regs()
        .lcd_control
        .contains(LcdFlags::DISPLAY_ENABLE)
    {
        ctx.ppu_mut().scanline_ticks += tcycles;
        let lcd_stat = ctx.ppu_regs().lcd_status;
        let mut lcdc_y = ctx.ppu_regs().lcdc_y;
        trace!("LCD is enabled.");
        trace!("LCD status {:b}", lcd_stat);
        trace!("Current scanline: {}", lcdc_y);
        trace!("Scanline progress: {}", ctx.ppu().scanline_ticks);

        match ctx.ppu_regs().lcd_status.mode() {
            LcdMode::HBlank => {
                trace!("HBlank");
                if ctx.ppu().scanline_ticks > 456 {
                    debug!("End of scan line {}", lcdc_y);
                    ctx.ppu_mut().scanline_reset();
                    lcdc_y += 1;
                    ctx.ppu_regs_mut().lcdc_y = lcdc_y;

                    if lcdc_y == ctx.ppu_regs().lcdc_y_compare
                        && lcd_stat.contains(LcdStat::Y_COINCIDENCE_INTERRUPT_ENABLE)
                    {
                        ctx.interrupts_mut().send(InterruptFlags::STAT);
                    }

                    if lcdc_y < 144 {
                        debug!("Video mode transition from 0 (HBlank) to 2 (OAMScan)");
                        ctx.oam_mut().mask();
                        ctx.ppu_regs_mut().lcd_status.set_mode(LcdMode::OamScan);
                        ctx.ppu_mut().bg_discard = ctx.ppu_regs().scroll_x & 0x7;
                        if lcd_stat.contains(LcdStat::OAM_INTERRUPT_ENABLE) {
                            ctx.interrupts_mut().send(InterruptFlags::STAT);
                        }
                    } else {
                        debug!("Video mode transition from 0 (HBlank) to 1 (VBlank)");
                        ctx.ppu_regs_mut().lcd_status.set_mode(LcdMode::VBlank);
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
                    ctx.ppu_regs_mut().lcdc_y = lcdc_y;

                    if lcdc_y > 153 {
                        debug!("Video mode transition from 1 (VBlank) to 2 (OAMScan)");
                        ctx.ppu_regs_mut().lcdc_y = 0;
                        ctx.ppu_regs_mut().lcd_status.set_mode(LcdMode::OamScan);
                        ctx.ppu_mut().bg_discard = ctx.ppu_regs().scroll_x & 0x7;
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
                        let sprite_height = ctx.ppu_regs().lcd_control.sprite_height();

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
                    ctx.ppu_regs_mut().lcd_status.set_mode(LcdMode::WriteScreen)
                }
            }
            LcdMode::WriteScreen => {
                trace!("WriteScreen");

                for _i in 0..tcycles {
                    if ctx.ppu_regs().lcd_control.contains(LcdFlags::WINDOW_ENABLE)
                        && ctx.ppu().scanline_x + 7 == ctx.ppu_regs().window_x
                        && lcdc_y >= ctx.ppu_regs().window_y
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
                                    palette_lookup(ctx.ppu_regs().bg_palette, bg_pixel)
                                } else {
                                    let obj_palette = obj_pixel.1.palette();
                                    palette_lookup(
                                        ctx.ppu_regs().obj_palette[obj_palette],
                                        obj_pixel.0,
                                    )
                                }
                            }
                            None => palette_lookup(ctx.ppu_regs().bg_palette, bg_pixel),
                        };
                        ctx.ppu().truecolor_palette[gb_color]
                    };

                    ctx.ppu_mut().scanline_x += 1;
                    if ctx.ppu().scanline_x > 159 {
                        debug!("Video mode transition from 3 (WriteScreen) to 0 (HBlank) after {} cycles.", ctx.ppu().scanline_ticks - 80);
                        ctx.oam_mut().unmask();
                        ctx.vram_mut().unmask();
                        ctx.ppu_regs_mut().lcd_status.set_mode(LcdMode::HBlank);
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

/// Run one DMA tick.
fn tick_dma(ctx: &mut impl PpuContext, _tcycles: u64) {
    if ctx.ppu_regs().dma.active() {
        // The address add can never overflow because "base" is always a multiple of 0x100
        // and offset is always 0x00..0xa0, so essentially this address add is just
        // combining bits.
        let src = ctx.ppu_regs().dma.source_addr();
        let dest = ctx.ppu_regs().dma.dest_addr();
        let byte = ctx.mem().read_byte(src);
        ctx.oam_mut()
            .bypass_mut()
            .bypass_mut()
            .write_byte_relative(dest, byte);
        ctx.ppu_regs_mut().dma.advance();
    }
}
