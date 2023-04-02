use std::convert::TryFrom;
use std::fmt;
use std::ops::{Deref, DerefMut};

use log::trace;
use thiserror::Error;

pub use feo3boy_memdev_derive::MemDevice;

use crate::apu::ApuRegs;
use crate::input::ButtonRegister;
use crate::interrupts::{InterruptContext, InterruptEnable, InterruptFlags, Interrupts};
use crate::ppu::PpuRegs;
use crate::serial::SerialRegs;
use crate::timer::TimerRegs;

pub use cartridge::{
    Cartridge, Mbc1Rom, Mbc3Rom, ParseCartridgeError, RamBank, RomBank, RomOnly, SaveData,
};

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
    /// Intended to be used to offset to the start of a child [`MemDevice`].
    ///
    /// Produces an address which is relative to `shift` in the current relative address
    /// space. This is useful when passing an address to a child mem device: call
    /// `offset_by` with the index of the start of the child relative to the parent to get
    /// an address which indexes from the start of the child.
    pub fn offset_by(&self, shift: u16) -> Self {
        assert!(
            shift <= self.relative,
            "Attempting to offset with overflow. Addr: {self}, shift: {shift}"
        );
        Addr {
            raw: self.raw,
            relative: self.relative - shift,
        }
    }

    /// Skip over is the reverse of `offset_by`. It is intended to be used when you want
    /// to bypass part of a child `MemDevice`. It produces an address which skips the
    /// first `skipped` bytes of the current relative address space. Typical usage would
    /// be `addr.offset_by(child_start).skip_over(num_bytes_to_skip)`.
    pub fn skip_over(&self, skipped: u16) -> Self {
        Addr {
            raw: self.raw,
            relative: match self.relative.checked_add(skipped) {
                Some(val) => val,
                None => {
                    panic!("Attempting to skip_over with overflow. Addr: {self}, shift: {skipped}")
                }
            },
        }
    }
}

impl fmt::Display for Addr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}h({:x}h)", self.raw, self.relative)
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

/// Memory Device for the bios-enable bit.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BiosEnable(bool);

impl BiosEnable {
    /// Return true if bios is enabled.
    pub fn enabled(&self) -> bool {
        self.0
    }
}

impl Default for BiosEnable {
    fn default() -> Self {
        Self(true)
    }
}

impl MemDevice for BiosEnable {
    fn read(&self, addr: Addr) -> u8 {
        assert!(
            addr.index() == 0,
            "Address {} out of range for BiosEnable",
            addr
        );
        // Should this return 0 for enabled? You write nonzero to it to *disable* bios.
        // Actually, should it even return anything at all? Is it even readable?
        self.0 as u8
    }

    fn write(&mut self, addr: Addr, data: u8) {
        assert!(
            addr.index() == 0,
            "Address {} out of range for BiosEnable",
            addr
        );
        // If value is false, stay false. If passed a non-zero value, become false.
        self.0 = self.0 && data == 0;
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

/// Workaround for the implementation of Default for arrays being so janky
/// Separated from main MemDevice interface in case arrays get const defaults later
pub trait CustomDefault {
    fn custom_default() -> Self;
}

impl<const N: usize> CustomDefault for [u8; N] {
    fn custom_default() -> Self {
        [0; N]
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct MaskableMem<M> {
    device: M,
    masked: bool,
}

impl<M: CustomDefault> MaskableMem<M> {
    pub fn new() -> Self {
        MaskableMem::custom_default()
    }

    pub fn mask(&mut self) {
        self.masked = true;
    }

    pub fn unmask(&mut self) {
        self.masked = false;
    }
}

impl<M: CustomDefault> CustomDefault for MaskableMem<M> {
    fn custom_default() -> Self {
        MaskableMem {
            device: M::custom_default(),
            masked: false,
        }
    }
}

impl<M: MemDevice> MemDevice for MaskableMem<M> {
    fn read(&self, addr: Addr) -> u8 {
        if !self.masked {
            self.device.read(addr)
        } else {
            0xff
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        if !self.masked {
            self.device.write(addr, value)
        }
    }
}

impl<M> Deref for MaskableMem<M> {
    type Target = M;
    fn deref(&self) -> &Self::Target {
        &self.device
    }
}

impl<M> DerefMut for MaskableMem<M> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.device
    }
}

/// A u8 acts as a single-byte memory device.
impl MemDevice for u8 {
    #[inline]
    fn read(&self, addr: Addr) -> u8 {
        assert!(addr.index() == 0, "Address {} out of range for u8", addr);
        *self
    }

    #[inline]
    fn write(&mut self, addr: Addr, val: u8) {
        assert!(addr.index() == 0, "Address {} out of range for u8", addr);
        *self = val;
    }
}

// This makes sure that Box<dyn MemDevice> implements MemDevice (as well as Box<Anything that
// implements MemDevice>).
impl<D: MemDevice + ?Sized> MemDevice for Box<D> {
    #[inline]
    fn read(&self, addr: Addr) -> u8 {
        (**self).read(addr)
    }

    #[inline]
    fn write(&mut self, addr: Addr, value: u8) {
        (**self).write(addr, value)
    }
}

/// Memory device connecting memory mapped IO.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct MemMappedIo {
    pub buttons: ButtonRegister,
    /// Registers used by the serial subsystem.
    pub serial_regs: SerialRegs,
    /// Registers used by the timer.
    pub timer_regs: TimerRegs,
    /// Set of active interrupts.
    pub interrupt_flags: InterruptFlags,
    /// apu status and settings
    pub apu_regs: ApuRegs,
    /// ppu status and settings
    pub ppu_regs: PpuRegs,
    pub bios_enable: BiosEnable,
}

impl MemMappedIo {
    /// Construct new memory-mapped IO manager.
    pub fn new() -> Self {
        Default::default()
    }
}

memdev_fields! {
    MemMappedIo {
        0x00 => buttons,
        0x01..=0x02 => serial_regs,
        0x03 => 0xff,
        0x04..=0x07 => timer_regs,
        0x08..=0x0e => 0xff,
        0x0f => interrupt_flags,
        0x10 => 0xff,
        0x10..=0x23 => apu_regs,
        0x24..=0x3f => 0xff,
        0x40..=0x4b => ppu_regs,
        0x4c..=0x4f => 0xff,
        0x50 => bios_enable,
        0x51..=0x7f => 0xff,
    }
}

pub type Vram = MaskableMem<MaskableMem<[u8; 0x2000]>>;
pub type Oam = MaskableMem<MaskableMem<[u8; 160]>>;

/// MemoryDevice which configures the standard memory mapping of the real GameBoy.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct GbMmu {
    /// The bios. Mapped to 0..0x100 while bios is enabled.
    pub bios: BiosRom,
    /// The inserted cartridge. Mapped to 0..0x8000 (rom) and 0xA000..0xC000 (ram).
    pub cart: Cartridge,
    /// Video Ram. Mapped to 0x8000..0xA000
    pub vram: Vram,
    /// Working Ram. Mapped to 0xC000..0xE000 and duplicately mapped at 0xE000..0xFE00.
    pub wram: [u8; 0x2000],
    /// Spirte info. Mapped to 0xFE00..0xFEA0.
    pub oam: Oam,
    /// Memory mapped IO. Mapped to 0xff00..FF80.
    // TODO: don't require this to be exposed directly (use traits like for interrupts).
    pub io: MemMappedIo,
    /// "Page Zero", memory primarily used for software-hardware interaction. Mapped to
    /// 0xFF80..0xffff
    pub zram: [u8; 127],
    /// Interrupt enable register. Mapped to 0xffff
    pub interrupt_enable: InterruptEnable,

    dma_active: bool,
    dma_base: u16,
    dma_offset: u16,
}

impl GbMmu {
    /// Construct a new MMU with the given bios and cartridge.
    /// Panics if the given bios data is not exactly 256 bytes.
    pub fn new(bios: BiosRom, cart: Cartridge) -> GbMmu {
        GbMmu {
            bios,
            cart,
            vram: Vram::new(),
            wram: [0; 0x2000],
            oam: Oam::new(),
            io: MemMappedIo::new(),
            zram: [0; 127],
            interrupt_enable: InterruptEnable(InterruptFlags::empty()),
            dma_active: false,
            dma_base: 0x0000,
            dma_offset: 0x0000,
        }
    }

    pub fn tick(&mut self, _tcycles: u64) {
        // very dirty implementation rn
        if self.dma_active {
            let byte = self.read(Addr::from(self.dma_base + self.dma_offset));
            self.oam
                .deref_mut()
                .deref_mut()
                .write(Addr::from(self.dma_offset), byte);
            self.dma_offset += 1;
            if self.dma_offset > 0x9f {
                self.dma_active = false;
            }
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
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.read(addr),
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
            0xff00..=0xff45 => self.io.read(addr.offset_by(0xff00)),
            0xff46 => (self.dma_base >> 8) as u8,
            0xff47..=0xff7f => self.io.read(addr.offset_by(0xff00)),
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
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.write(addr, value),
            0x0..=0x7fff => self.cart.write(addr, value),
            0x8000..=0x9fff => self.vram.write(addr.offset_by(0x8000), value),
            // Cartridge ram starts right after cartridge rom internally, so for this
            // mapping we offset to the address where we are mapping cart ram, then skip
            // over the cartridge rom to make sure that this mapping begins from the start
            // of cartridge ram.
            0xa000..=0xbfff => self
                .cart
                .write(addr.offset_by(0xa000).skip_over(0x8000), value),
            0xc000..=0xdfff => self.wram.write(addr.offset_by(0xc000), value),
            0xe000..=0xfdff => self.wram.write(addr.offset_by(0xe000), value),
            0xfe00..=0xfe9f => self.oam.write(addr.offset_by(0xfe00), value),
            // Unmapped portion above sprite information.
            0xfea0..=0xfeff => {}
            0xff00..=0xff45 => self.io.write(addr.offset_by(0xff00), value),
            0xff46 => {
                self.dma_active = true;
                self.dma_base = (value as u16) << 8;
                self.dma_offset = 0;
            }
            0xff47..=0xff7f => self.io.write(addr.offset_by(0xff00), value),
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
