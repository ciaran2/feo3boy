use std::convert::TryFrom;
use std::fmt;
use std::ops::Deref;

use log::trace;
use thiserror::Error;

use crate::interrupts::{InterruptContext, InterruptEnable, InterruptFlags, Interrupts};
use crate::ppu::{LcdFlags, LcdStat};
use crate::timer::{TimerControl};
use crate::input::{ButtonRegister};

pub use cartridge::{Cartridge, Mbc1Rom, ParseCartridgeError, RamBank, RomBank, RomOnly};

mod cartridge;

/// A memory address within system memory. Provides both the raw address and relative address so
/// that devices can report both raw and relative addresses in error messages.
#[derive(Copy, Clone, Debug)]
pub struct Addr {
    /// Raw address, originating from game code.
    raw: u16,

    /// Address relative to the start of a particular memory device.
    relative: u16,
}

impl From<u16> for Addr {
    /// Create a new address from a raw address. No offset is applied, so initially both the raw
    /// and relative addresses are the same.
    fn from(raw: u16) -> Self {
        Addr { raw, relative: raw }
    }
}

impl Addr {
    /// Gets the raw address that this Addr points at.
    pub fn raw(&self) -> u16 {
        self.raw
    }

    /// Gets the address this Addr points at relative to the start of some unspecified device.
    pub fn relative(&self) -> u16 {
        self.relative
    }

    /// Gets the address this Addr points at relative to the start of the (unspecified) device, as
    /// a usize for convenient indexing.
    pub fn index(&self) -> usize {
        self.relative as usize
    }

    /// Gets the current offset.
    pub fn offset(&self) -> u16 {
        self.raw - self.relative
    }

    /// Constructs a new address, offsetting the relative address by the specified amount.
    pub fn offset_by(&self, shift: u16) -> Self {
        assert!(shift <= self.relative, "Attempting to offset with overflow");
        Addr {
            raw: self.raw,
            relative: self.relative - shift,
        }
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}({:x})", self.raw, self.relative)
    }
}

/// Provides access to system memory.
pub trait MemDevice {
    /// Read the byte at the specified address.
    fn read(&self, addr: Addr) -> u8;

    /// Write the byte at the sepcified address.
    fn write(&mut self, addr: Addr, data: u8);
}

/// Context trait for accessing memory.
pub trait MemContext {
    /// Type of MemDevice in this context.
    type Mem: MemDevice;

    /// Gets the memory.
    fn mem(&self) -> &Self::Mem;

    /// Get a mutable reference to the memory.
    fn mem_mut(&mut self) -> &mut Self::Mem;
}

/// Wraps a memory device to make it read-only.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct ReadOnly<M>(M);

impl<M> Deref for ReadOnly<M> {
    type Target = M;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<M> ReadOnly<M> {
    /// Constructs a ReadOnly memory device that wraps the given underlying memory.
    pub fn new(mem: M) -> Self {
        Self(mem)
    }

    /// Unwraps the inner memory device and returns it. This allows mutable access again.
    pub fn into_inner(self) -> M {
        self.0
    }
}

impl<M: MemDevice> MemDevice for ReadOnly<M> {
    fn read(&self, addr: Addr) -> u8 {
        self.0.read(addr)
    }

    fn write(&mut self, addr: Addr, _value: u8) {
        // Read the address to allow the wrapped device to validate the address range.
        self.0.read(addr);
    }
}

/// A rom which does bounds checks, but contains no actual memory (always returns 0, ignores
/// writes).
pub struct NullRom<const N: usize>;

impl<const N: usize> MemDevice for NullRom<N> {
    fn read(&self, addr: Addr) -> u8 {
        assert!(
            addr.index() < N,
            "Address {}  out of range for {} byte nullrom",
            addr,
            N
        );
        0
    }

    fn write(&mut self, addr: Addr, _value: u8) {
        assert!(
            addr.index() < N,
            "Address {}  out of range for {} byte nullrom",
            addr,
            N
        );
    }
}

/// Rom for the bios, which is swapped out once started.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BiosRom(ReadOnly<[u8; 0x100]>);

impl BiosRom {
    /// Construct a `BiosRom` from an array of 256 bytes. Does not validate the contents.
    pub fn new(data: [u8; 0x100]) -> Self {
        Self(ReadOnly::new(data))
    }

    /// Constructs a `BiosRom` from a slice of contained bytes. The slice must be exactly 256
    /// bytes. No other validation of the contents is performed.
    // TryFrom/TryInto aren't standard imports, so provide a convenience method that doesn't
    // require a TryFrom/TryInto import for the caller.
    pub fn try_from_slice(data: &[u8]) -> Result<Self, BiosSizeError> {
        Self::try_from(data)
    }
}

impl Deref for BiosRom {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Default for BiosRom {
    fn default() -> Self {
        Self(ReadOnly::new([0; 0x100]))
    }
}

impl MemDevice for BiosRom {
    fn read(&self, addr: Addr) -> u8 {
        self.0.read(addr)
    }

    fn write(&mut self, addr: Addr, value: u8) {
        self.0.write(addr, value)
    }
}

/// Error when converting a slice to a [`BiosRom`]. Contains the number of bytes of the given
/// slice.
#[derive(Copy, Clone, Debug, Error)]
#[error("Expected exactly 256 bytes, got {0}")]
pub struct BiosSizeError(pub usize);

impl TryFrom<&[u8]> for BiosRom {
    type Error = BiosSizeError;

    fn try_from(data: &[u8]) -> Result<Self, BiosSizeError> {
        if data.len() != 0x100 {
            Err(BiosSizeError(data.len()))
        } else {
            let mut arr = [0; 0x100];
            arr.copy_from_slice(data);
            Ok(Self(ReadOnly::new(arr)))
        }
    }
}

impl TryFrom<Vec<u8>> for BiosRom {
    type Error = BiosSizeError;

    fn try_from(data: Vec<u8>) -> Result<Self, BiosSizeError> {
        Self::try_from(&*data)
    }
}

impl<const N: usize> MemDevice for [u8; N] {
    fn read(&self, addr: Addr) -> u8 {
        match self.get(addr.index()) {
            Some(val) => *val,
            None => panic!("Address {}  out of range for {} byte memory array", addr, N),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        match self.get_mut(addr.index()) {
            Some(val) => *val = value,
            None => panic!("Address {}  out of range for {} byte memory array", addr, N),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MaskableMem<const N: usize> {
    data: [u8; N],
    masked: bool,
}

impl<const N: usize> MaskableMem<N> {
    pub fn new() -> Self {
        MaskableMem {
            data: [0; N],
            masked: false,
        }
    }

    pub fn mask(&mut self) {
        self.masked = true;
    }

    pub fn unmask(&mut self) {
        self.masked = false;
    }

    pub fn bytes(&self) -> &[u8] {
        &self.data
    }

    pub fn bytes_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }
}

impl<const N: usize> MemDevice for MaskableMem<N> {
    fn read(&self, addr: Addr) -> u8 {
        if !self.masked {
            self.data.read(addr)
        }
        else {
            0xff
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        if !self.masked {
            self.data.write(addr, value)
        }
    }
}

// This makes sure that Box<dyn MemDevice> implements MemDevice (as well as Box<Anything that
// implements MemDevice>).
impl<D: MemDevice + ?Sized> MemDevice for Box<D> {
    fn read(&self, addr: Addr) -> u8 {
        (**self).read(addr)
    }

    fn write(&mut self, addr: Addr, value: u8) {
        (**self).write(addr, value)
    }
}

/// Memory device connecting memory mapped IO.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemMappedIo {
    pub buttons: ButtonRegister,
    pub serial_data: u8,
    pub serial_control: u8,
    pub divider: u16,
    pub timer: u8,
    pub timer_mod: u8,
    pub timer_control: TimerControl,
    pub bios_enabled: bool,
    // ppu status and settings
    pub lcd_control: LcdFlags,
    pub lcd_status: LcdStat,
    pub scroll_y: u8,
    pub scroll_x: u8,
    pub lcdc_y: u8,
    pub lcdc_y_compare: u8,
    pub dma_addr: u8,
    pub bg_palette: u8,
    pub obj0_palette: u8,
    pub obj1_palette: u8,
    pub window_y: u8,
    pub window_x: u8,
    pub interrupt_flags: InterruptFlags,
}

impl MemMappedIo {
    /// Construct new memory-mapped IO manager.
    pub fn new() -> Self {
        MemMappedIo {
            buttons: ButtonRegister::all(),
            serial_data: 0x00,
            serial_control: 0x00,
            divider: 0x0000,
            timer: 0x00,
            timer_mod: 0x00,
            timer_control: TimerControl::empty(),
            bios_enabled: true,
            // ppu status and settings
            lcd_control: LcdFlags::empty(),
            lcd_status: LcdStat::empty(),
            scroll_y: 0x00,
            scroll_x: 0x00,
            lcdc_y: 0x00,
            lcdc_y_compare: 0x00,
            dma_addr: 0x00,
            bg_palette: 0x00,
            obj0_palette: 0x00,
            obj1_palette: 0x00,
            window_y: 0x00,
            window_x: 0x00,
            interrupt_flags: InterruptFlags::empty(),
        }
    }

    /// Returns true if bios is enabled.
    pub fn bios_enabled(&self) -> bool {
        self.bios_enabled
    }
}

impl Default for MemMappedIo {
    fn default() -> Self {
        Self::new()
    }
}

impl MemDevice for MemMappedIo {
    fn read(&self, addr: Addr) -> u8 {
        match addr.relative() {
            0x00 => self.buttons.bits(),
            0x01 => self.serial_data,
            0x02 => self.serial_control,
            0x03 => 0xff,
            0x04 => (self.divider / 0x100) as u8,
            0x05 => self.timer,
            0x06 => self.timer_mod,
            0x07 => self.timer_control.bits(),
            0x08..=0x0e => 0xff,
            0x0f => self.interrupt_flags.bits(),
            0x10..=0x3f => 0xff,
            0x40 => self.lcd_control.bits(),
            0x41 => self.lcd_status.bits(),
            0x42 => self.scroll_y,
            0x43 => self.scroll_x,
            0x44 => self.lcdc_y,
            0x45 => self.lcdc_y_compare,
            0x46 => self.dma_addr,
            0x47 => self.bg_palette,
            0x48 => self.obj0_palette,
            0x49 => self.obj1_palette,
            0x4a => self.window_y,
            0x4b => self.window_x,
            0x4c..=0x4f => 0xff,
            0x50 => self.bios_enabled as u8,
            0x51..=0x7f => 0xff,
            _ => panic!("Address {} out of range for Mem Mapped IO", addr),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        match addr.relative() {
            0x00 => self.buttons = self.buttons.set_writable(value),
            0x01 => self.serial_data = value,
            0x02 => self.serial_control = value,
            0x03 => {},
            0x04 => self.divider = 0x0000,
            0x05 => self.timer = value,
            0x06 => self.timer_mod = value,
            0x07 => self.timer_control = TimerControl::from_bits_truncate(value),
            0x08..=0x0e => {},
            0x0f => self.interrupt_flags = InterruptFlags::from_bits_truncate(value),
            0x10..=0x3f => {},
            0x40 => self.lcd_control = LcdFlags::from_bits_truncate(value),
            0x41 => self.lcd_status = self.lcd_status.set_writeable(value),
            0x42 => self.scroll_y = value,
            0x43 => self.scroll_x = value,
            0x44 => {},
            0x45 => self.lcdc_y_compare = value,
            0x46 => self.dma_addr = value,
            0x47 => self.bg_palette = value,
            0x48 => self.obj0_palette = value,
            0x49 => self.obj1_palette = value,
            0x4a => self.window_y = value,
            0x4b => self.window_x = value,
            0x4c..=0x4f => {},
            0x50 => {
                if value & 1 != 0 {
                    self.bios_enabled = false;
                }
            }
            0x51..=0x7f => {}
            _ => panic!("Address {} out of range for Mem Mapped IO", addr),
        }
    }
}

impl IoRegs for MemMappedIo {
    fn buttons(&self) -> ButtonRegister {
        self.buttons
    }
    fn set_buttons(&mut self, val: ButtonRegister) {
        self.buttons = val;
    }

    fn serial_data(&self) -> u8 {
        self.serial_data
    }

    fn set_serial_data(&mut self, val: u8) {
        self.serial_data = val;
    }

    fn serial_control(&self) -> u8 {
        self.serial_control
    }

    fn set_serial_control(&mut self, val: u8) {
        self.serial_control = val;
    }

    fn divider(&self) -> u16 {
        self.divider
    }
    fn set_divider(&mut self, val: u16) {
        self.divider = val;
    }
    fn timer(&self) -> u8 {
        self.timer
    }
    fn set_timer(&mut self, val: u8) {
        self.timer = val;
    }
    fn timer_mod(&self) -> u8 {
        self.timer_mod
    }
    fn set_timer_mod(&mut self, val: u8) {
        self.timer_mod = val;
    }
    fn timer_control(&self) -> TimerControl {
        self.timer_control
    }
    fn set_timer_control(&mut self, val: TimerControl) {
        self.timer_control = val;
    }

    fn lcd_control(&self) -> LcdFlags {
        self.lcd_control
    }

    fn lcd_stat(&self) -> LcdStat {
        self.lcd_status
    }
    fn set_lcd_stat(&mut self, val: LcdStat) {
        self.lcd_status = val;
    }

    fn scroll_y(&self) -> u8 {
        self.scroll_y
    }
    fn set_scroll_y(&mut self, val: u8) {
        self.scroll_y = val;
    }

    fn scroll_x(&self) -> u8 {
        self.scroll_x
    }
    fn set_scroll_x(&mut self, val: u8) {
        self.scroll_x = val;
    }

    fn lcdc_y(&self) -> u8 {
        self.lcdc_y
    }
    fn set_lcdc_y(&mut self, val: u8) {
        self.lcdc_y = val;
    }

    fn lcdc_y_compare(&self) -> u8 {
        self.lcdc_y_compare
    }
    fn set_lcdc_y_compare(&mut self, val: u8) {
        self.lcdc_y_compare = val;
    }

    fn dma_addr(&self) -> u8 {
        self.dma_addr
    }
    fn set_dma_addr(&mut self, val: u8) {
        self.dma_addr = val;
    }

    fn bg_palette(&self) -> u8 {
        self.bg_palette
    }
    fn set_bg_palette(&mut self, val: u8) {
        self.bg_palette = val;
    }

    fn obj0_palette(&self) -> u8 {
        self.obj0_palette
    }
    fn set_obj0_palette(&mut self, val: u8) {
        self.obj0_palette = val;
    }

    fn obj1_palette(&self) -> u8 {
        self.obj1_palette
    }
    fn set_obj1_palette(&mut self, val: u8) {
        self.obj1_palette = val;
    }

    fn window_y(&self) -> u8 {
        self.window_y
    }
    fn set_window_y(&mut self, val: u8) {
        self.window_y = val;
    }

    fn window_x(&self) -> u8 {
        self.window_x
    }
    fn set_window_x(&mut self, val: u8) {
        self.window_x = val;
    }
}

/// Context trait for providing access to IO registers.
pub trait IoRegsContext {
    type IoRegs: IoRegs;

    /// Get read-only access to the IO registers.
    fn ioregs(&self) -> &Self::IoRegs;

    /// Get write access to the IO registers.
    fn ioregs_mut(&mut self) -> &mut Self::IoRegs;
}

/// Trait for use with contexts to provide access to IO registers.
pub trait IoRegs {
    /// Get the current button control and status
    fn buttons(&self) -> ButtonRegister;

    /// Set the current button control and status
    fn set_buttons(&mut self, val: ButtonRegister);
    
    /// Get the current value of the serial data register.
    fn serial_data(&self) -> u8;

    /// Set the current value of the serial data register.
    fn set_serial_data(&mut self, val: u8);

    /// Get the current value of the serial control register.
    fn serial_control(&self) -> u8;

    /// Set the current value of the serial control register.
    fn set_serial_control(&mut self, val: u8);

    fn divider(&self) -> u16;
    fn set_divider(&mut self, val: u16);
    fn timer(&self) -> u8;
    fn set_timer(&mut self, val: u8);
    fn timer_mod(&self) -> u8;
    fn set_timer_mod(&mut self, val: u8);
    fn timer_control(&self) -> TimerControl;
    fn set_timer_control(&mut self, val: TimerControl);

    fn lcd_control(&self) -> LcdFlags;

    fn lcd_stat(&self) -> LcdStat;
    fn set_lcd_stat(&mut self, val: LcdStat);

    fn scroll_y(&self) -> u8;
    fn set_scroll_y(&mut self, val: u8);

    fn scroll_x(&self) -> u8;
    fn set_scroll_x(&mut self, val: u8);

    fn lcdc_y(&self) -> u8;
    fn set_lcdc_y(&mut self, val: u8);

    fn lcdc_y_compare(&self) -> u8;
    fn set_lcdc_y_compare(&mut self, val: u8);

    fn dma_addr(&self) -> u8;
    fn set_dma_addr(&mut self, val: u8);

    fn bg_palette(&self) -> u8;
    fn set_bg_palette(&mut self, val: u8);

    fn obj0_palette(&self) -> u8;
    fn set_obj0_palette(&mut self, val: u8);

    fn obj1_palette(&self) -> u8;
    fn set_obj1_palette(&mut self, val: u8);

    fn window_y(&self) -> u8;
    fn set_window_y(&mut self, val: u8);

    fn window_x(&self) -> u8;
    fn set_window_x(&mut self, val: u8);
    // TODO: other IO registers.
}

/// MemoryDevice which configures the standard memory mapping of the real GameBoy.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GbMmu {
    /// The bios. Mapped to 0..0x100 while bios is enabled.
    pub bios: BiosRom,
    /// The inserted cartridge. Mapped to 0..0x8000 (rom) and 0xA000..0xC000 (ram).
    pub cart: Cartridge,
    /// Video Ram. Mapped to 0x8000..0xA000
    pub vram: MaskableMem::<0x2000>,
    /// Working Ram. Mapped to 0xC000..0xE000 and duplicately mapped at 0xE000..0xFE00.
    pub wram: [u8; 0x2000],
    /// Spirte info. Mapped to 0xFE00..0xFEA0.
    pub oam: MaskableMem::<160>,
    /// Memory mapped IO. Mapped to 0xff00..FF80.
    // TODO: don't require this to be exposed directly (use traits like for interrupts).
    pub io: MemMappedIo,
    /// "Page Zero", memory primarily used for software-hardware interaction. Mapped to
    /// 0xFF80..0xffff
    pub zram: [u8; 127],
    /// Interrupt enable register. Mapped to 0xffff
    pub interrupt_enable: InterruptEnable,
}

impl GbMmu {
    /// Construct a new MMU with the given bios and cartridge.
    /// Panics if the given bios data is not exactly 256 bytes.
    pub fn new(bios: BiosRom, cart: Cartridge) -> GbMmu {
        GbMmu {
            bios,
            cart,
            vram: MaskableMem::<0x2000>::new(),
            wram: [0; 0x2000],
            oam: MaskableMem::<160>::new(),
            io: MemMappedIo::new(),
            zram: [0; 127],
            interrupt_enable: InterruptEnable(InterruptFlags::empty()),
        }
    }
}

impl Default for GbMmu {
    fn default() -> Self {
        Self::new(Default::default(), Cartridge::None)
    }
}

impl MemDevice for GbMmu {
    fn read(&self, addr: Addr) -> u8 {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        trace!("Read from MMU address {:#x}", addr.raw);
        // Address guaranteed to be in range since we cover the whole memory space.
        match addr.relative() {
            0x0..=0xff if self.io.bios_enabled() => self.bios.read(addr),
            0x0..=0x7fff => self.cart.read(addr),
            0x8000..=0x9fff => self.vram.read(addr.offset_by(0x8000)),
            // Cartridge ram starts right after cartridge Rom, so the offset used here is the
            // size of vram, since we only want to shift the address by the ammount we skipped in
            // order to splice in the vram.
            0xa000..=0xbfff => self.cart.read(addr.offset_by(0x2000)),
            0xc000..=0xdfff => self.wram.read(addr.offset_by(0xc000)),
            0xe000..=0xfdff => self.wram.read(addr.offset_by(0xe000)),
            0xfe00..=0xfe9f => self.oam.read(addr.offset_by(0xfe00)),
            // Unmapped portion above sprite information, always returns 0.
            0xfea0..=0xfeff => 0,
            0xff00..=0xff7f => self.io.read(addr.offset_by(0xff00)),
            0xff80..=0xfffe => self.zram.read(addr.offset_by(0xff80)),
            0xffff => self.interrupt_enable.read(addr.offset_by(0xffff)),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        // Address guaranteed to be in range since we cover the whole memory space.
        match addr.relative() {
            0x0..=0xff if self.io.bios_enabled() => self.bios.write(addr, value),
            0x0..=0x7fff => self.cart.write(addr, value),
            0x8000..=0x9fff => self.vram.write(addr.offset_by(0x8000), value),
            // Cartridge ram starts right after cartridge Rom, so the offset used here is the
            // size of vram, since we only want to shift the address by the ammount we skipped in
            // order to splice in the vram.
            0xa000..=0xbfff => self.cart.write(addr.offset_by(0x2000), value),
            0xc000..=0xdfff => self.wram.write(addr.offset_by(0xc000), value),
            0xe000..=0xfdff => self.wram.write(addr.offset_by(0xe000), value),
            0xfe00..=0xfe9f => self.oam.write(addr.offset_by(0xfe00), value),
            // Unmapped portion above sprite information.
            0xfea0..=0xfeff => {}
            0xff00..=0xff7f => self.io.write(addr.offset_by(0xff00), value),
            0xff80..=0xfffe => self.zram.write(addr.offset_by(0xff80), value),
            0xffff => self.interrupt_enable.write(addr.offset_by(0xffff), value),
        }
    }
}

impl MemContext for GbMmu {
    type Mem = Self;

    fn mem(&self) -> &Self::Mem {
        self
    }

    fn mem_mut(&mut self) -> &mut Self::Mem {
        self
    }
}

impl IoRegsContext for GbMmu {
    type IoRegs = MemMappedIo;

    fn ioregs(&self) -> &Self::IoRegs {
        &self.io
    }

    fn ioregs_mut(&mut self) -> &mut Self::IoRegs {
        &mut self.io
    }
}

impl InterruptContext for GbMmu {
    type Interrupts = Self;

    fn interrupts(&self) -> &Self::Interrupts {
        self
    }

    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        self
    }
}

impl Interrupts for GbMmu {
    #[inline]
    fn queued(&self) -> InterruptFlags {
        self.io.interrupt_flags
    }

    #[inline]
    fn set_queued(&mut self, flags: InterruptFlags) {
        self.io.interrupt_flags = flags;
    }

    #[inline]
    fn enabled(&self) -> InterruptFlags {
        self.interrupt_enable.0
    }

    #[inline]
    fn set_enabled(&mut self, flags: InterruptFlags) {
        self.interrupt_enable.0 = flags;
    }
}
