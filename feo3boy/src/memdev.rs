use std::convert::TryFrom;
use std::ops::{Deref, DerefMut, Range};
use std::{fmt, mem, slice};

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

pub trait RangeOverlaps {
    /// Returns true if this range fully encloses other.
    fn encloses(&self, other: &Self) -> bool;
}

impl RangeOverlaps for Range<usize> {
    fn encloses(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

/// A memory address relative to a particular MemDevice. Tracks both the absolute address
/// (for debugging purposes) and the relative address for indexing.
#[derive(Copy, Clone, Debug)]
pub struct RelativeAddr {
    /// Raw address, originating from game code.
    raw: u16,
    /// Address relative to the start of a particular memory device.
    relative: u16,
}

impl From<u16> for RelativeAddr {
    /// Create a new address from a raw address. No offset is applied, so initially both the raw
    /// and relative addresses are the same.
    #[inline]
    fn from(raw: u16) -> Self {
        Self::new(raw)
    }
}

impl RelativeAddr {
    /// Creates a new address from the given raw value. The address is initially based at
    /// the start of memory.
    pub const fn new(raw: u16) -> Self {
        RelativeAddr { raw, relative: raw }
    }

    /// Get a RelativeAddr for the first byte in the device starting at `addr`. This will
    /// have `raw == addr` and `relative == 0`. It is equivalent to
    /// `RelativeAddr::new(addr).offset_by(addr)`.
    pub const fn device_start(addr: u16) -> Self {
        RelativeAddr {
            raw: addr,
            relative: 0,
        }
    }

    /// Gets the raw address that this Addr points at.
    pub const fn raw(&self) -> u16 {
        self.raw
    }

    /// Gets the address this Addr points at relative to the start of some unspecified device.
    pub const fn relative(&self) -> u16 {
        self.relative
    }

    /// Gets the address this Addr points at relative to the start of the (unspecified) device, as
    /// a usize for convenient indexing.
    pub const fn index(&self) -> usize {
        self.relative as usize
    }

    /// Gets the current offset.
    pub const fn offset(&self) -> u16 {
        self.raw - self.relative
    }

    /// Get a range of the specified length that starts from this this address.
    pub const fn range(&self, len: usize) -> Range<usize> {
        self.index()..self.index() + len
    }

    /// Constructs a new address, offsetting the relative address by the specified amount.
    /// Intended to be used to offset to the start of a child [`MemDevice`].
    ///
    /// Produces an address which is relative to `shift` in the current relative address
    /// space. This is useful when passing an address to a child mem device: call
    /// `offset_by` with the index of the start of the child relative to the parent to get
    /// an address which indexes from the start of the child.
    pub const fn offset_by(&self, shift: u16) -> Self {
        assert!(
            shift <= self.relative,
            "Attempting to offset with overflow."
        );
        RelativeAddr {
            raw: self.raw,
            relative: self.relative - shift,
        }
    }

    /// Skip over is the reverse of `offset_by`. It is intended to be used when you want
    /// to bypass part of a child `MemDevice`. It produces an address which skips the
    /// first `skipped` bytes of the current relative address space. Typical usage would
    /// be `addr.offset_by(child_start).skip_over(num_bytes_to_skip)`.
    pub const fn skip_over(&self, skipped: u16) -> Self {
        RelativeAddr {
            raw: self.raw,
            relative: match self.relative.checked_add(skipped) {
                Some(val) => val,
                None => {
                    panic!("Attempting to skip_over with overflow.")
                }
            },
        }
    }

    /// Move the address forward by the specified number of bytes. This modifies both the
    /// raw and relative addresses. Prevents wrapping in debug mode but not release.
    pub const fn move_forward_by(&self, added: u16) -> Self {
        RelativeAddr {
            raw: self.raw + added,
            relative: self.relative + added,
        }
    }

    /// Move the address forward by the specified number of bytes. This modifies both the
    /// raw and relative addresses.
    pub const fn move_forward_by_wrapping(&self, added: u16) -> Self {
        RelativeAddr {
            raw: self.raw.wrapping_add(added),
            relative: self.relative.wrapping_add(added),
        }
    }
}

impl fmt::Display for RelativeAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}h({:x}h)", self.raw, self.relative)
    }
}

/// Provides access to system memory.
pub trait MemDevice {
    /// Length of this MemDevice in bytes. Reads and writes must always be within the device.
    const LEN: usize;

    /// Read a typed value from this MemDevice. The value must be fully contained within
    /// this device.
    #[inline]
    fn read_relative_into<V>(&self, addr: RelativeAddr, dest: &mut V)
    where
        V: MemValue,
    {
        struct Reader<'a, M: ?Sized> {
            addr: RelativeAddr,
            device: &'a M,
        }
        impl<'a, M: MemDevice + ?Sized> MemSource for Reader<'a, M> {
            #[inline(always)]
            fn get_byte(self) -> u8 {
                self.device.read_byte_relative(self.addr)
            }
            #[inline(always)]
            fn get_bytes(self, data: &mut [u8]) {
                self.device.read_bytes_relative(self.addr, data)
            }
        }

        dest.copy_from_mem(Reader { addr, device: self })
    }

    /// Read a typed value from this MemDevice. The value must be fully contained within
    /// this device.
    #[inline]
    fn read_relative<V>(&self, addr: RelativeAddr) -> V
    where
        V: MemValue + Default,
    {
        let mut val = V::default();
        self.read_relative_into(addr, &mut val);
        val
    }

    /// Write a typed value into this MemDevice. The value must fit fully within the
    /// device.
    #[inline]
    fn write_relative_from<V>(&mut self, addr: RelativeAddr, source: &V)
    where
        V: MemValue,
    {
        struct Writer<'a, M: ?Sized> {
            addr: RelativeAddr,
            device: &'a mut M,
        }
        impl<'a, M: MemDevice + ?Sized> MemDest for Writer<'a, M> {
            #[inline(always)]
            fn set_byte(self, val: u8) {
                self.device.write_byte_relative(self.addr, val)
            }
            #[inline(always)]
            fn set_bytes(self, data: &[u8]) {
                self.device.write_bytes_relative(self.addr, data)
            }
        }

        source.copy_to_mem(Writer { addr, device: self })
    }

    /// Write a typed value into this MemDevice. The value must fit fully within the
    /// device.
    #[inline]
    fn write_relative<V>(&mut self, addr: RelativeAddr, val: V)
    where
        V: MemValue,
    {
        self.write_relative_from(addr, &val)
    }

    /// Read the byte at the specified address.
    ///
    /// The default implementation just calls `read_range_relative` with a single-byte
    /// slice. A mem device may override this method if it can provide a more efficient
    /// single-byte read.
    #[inline]
    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        let mut out = 0;
        self.read_bytes_relative(addr, slice::from_mut(&mut out));
        out
    }

    /// Read a range of bytes into a slice. The read bytes must not wrap past the end of
    /// the device.
    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]);

    /// Write the byte at the sepcified address.
    ///
    /// The default implementation just calls `write_range_relative` with a single-byte
    /// slice. A mem device may override this method if it can provide a more efficient
    /// single-byte write.
    #[inline]
    fn write_byte_relative(&mut self, addr: RelativeAddr, data: u8) {
        self.write_bytes_relative(addr, slice::from_ref(&data));
    }

    /// Write a range of bytes into memory from a slice. The slice must not exceed the
    /// length of the MemDevice.
    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]);
}

/// A [`MemDevice`] which exists at the root. Root MemDevices can read ranges that wrap
/// around from the end of memory to the start.
///
/// A Root MemDevice can read data at any address. Because the device is at the root,
/// addresses are `u16`.
///
/// The length of a RootMemDevice must always be 0x10000, however generic equality
/// constraints currently prevent us from mandating that in the type signature. Once those
/// are available, this should be changed to RootMemDevice: MemDevice<LEN = 0x10000>.
pub trait RootMemDevice: MemDevice {
    /// Read a typed value from this MemDevice. The value must be fully contained within
    /// this device.
    #[inline]
    fn read_into<V>(&self, addr: u16, dest: &mut V)
    where
        V: MemValue,
    {
        struct Reader<'a, M: ?Sized> {
            addr: u16,
            device: &'a M,
        }
        impl<'a, M: RootMemDevice + ?Sized> MemSource for Reader<'a, M> {
            #[inline(always)]
            fn get_byte(self) -> u8 {
                self.device.read_byte(self.addr)
            }
            #[inline(always)]
            fn get_bytes(self, data: &mut [u8]) {
                self.device.read_bytes(self.addr, data)
            }
        }

        dest.copy_from_mem(Reader { addr, device: self })
    }

    /// Read a typed value from this MemDevice. The value must be fully contained within
    /// this device.
    #[inline]
    fn read<V>(&self, addr: u16) -> V
    where
        V: MemValue + Default,
    {
        let mut val = V::default();
        self.read_into(addr, &mut val);
        val
    }

    /// Write a typed value into this MemDevice. The value must fit fully within the
    /// device.
    #[inline]
    fn write_from<V>(&mut self, addr: u16, source: &V)
    where
        V: MemValue,
    {
        struct Writer<'a, M: ?Sized> {
            addr: u16,
            device: &'a mut M,
        }
        impl<'a, M: RootMemDevice + ?Sized> MemDest for Writer<'a, M> {
            #[inline(always)]
            fn set_byte(self, val: u8) {
                self.device.write_byte(self.addr, val)
            }
            #[inline(always)]
            fn set_bytes(self, data: &[u8]) {
                self.device.write_bytes(self.addr, data)
            }
        }

        source.copy_to_mem(Writer { addr, device: self })
    }

    /// Write a typed value into this MemDevice. The value must fit fully within the
    /// device.
    #[inline]
    fn write<V>(&mut self, addr: u16, val: V)
    where
        V: MemValue,
    {
        self.write_from(addr, &val)
    }

    /// Read a single byte from memory.
    #[inline]
    fn read_byte(&self, addr: u16) -> u8 {
        self.read_byte_relative(addr.into())
    }

    /// Read a range of bytes, wrapping at the ends of memory.
    fn read_bytes(&self, mut addr: u16, mut data: &mut [u8]) {
        const SIZE: usize = u16::MAX as usize + 1;
        loop {
            let len = SIZE - addr as usize;
            if len < data.len() {
                let (current, rest) = data.split_at_mut(len);
                self.read_bytes_relative(addr.into(), current);
                data = rest;
                addr = 0;
            } else {
                self.read_bytes_relative(addr.into(), data);
                break;
            }
        }
    }

    /// Write a single byte to memory.
    #[inline]
    fn write_byte(&mut self, addr: u16, val: u8) {
        self.write_byte_relative(addr.into(), val);
    }

    /// Write a range of bytes, wrapping at the ends of memory.
    fn write_bytes(&mut self, mut addr: u16, mut data: &[u8]) {
        const SIZE: usize = u16::MAX as usize + 1;
        loop {
            let len = SIZE - addr as usize;
            if len < data.len() {
                let (current, rest) = data.split_at(len);
                self.write_bytes_relative(addr.into(), current);
                data = rest;
                addr = 0;
            } else {
                self.write_bytes_relative(addr.into(), data);
                break;
            }
        }
    }
}

/// Helper trait for [`MemValue`] which allows it to work with both [`MemDevice`] and
/// [`RootMemDevice`] and does not require it to know about addresses.
///
/// This trait represents memory in a particular MemDevice at a particular address which
/// can be read from.
pub trait MemSource {
    /// Retrieve a single byte from the source address.
    fn get_byte(self) -> u8;
    /// Retrieve a slice of bytes from the source address.
    fn get_bytes(self, data: &mut [u8]);
}

/// Helper trait for [`MemValue`] which allows it to work with both [`MemDevice`] and
/// [`RootMemDevice`] and does not require it to know about addresses.
///
/// This trait represents memory in a particular MemDevice at a particular address which
/// can be written to.
pub trait MemDest {
    /// Set a single byte at the destination address.
    fn set_byte(self, val: u8);
    /// Write a slice of bytes at the destination address.
    fn set_bytes(self, data: &[u8]);
}

/// A value which can be stored in GameBoy memory.
pub trait MemValue {
    /// Convert this type into its in-memory representation.
    fn copy_from_mem<S: MemSource>(&mut self, source: S);

    fn copy_to_mem<D: MemDest>(&self, dest: D);
}

impl MemValue for u8 {
    #[inline]
    fn copy_from_mem<S: MemSource>(&mut self, source: S) {
        *self = source.get_byte()
    }

    #[inline]
    fn copy_to_mem<D: MemDest>(&self, dest: D) {
        dest.set_byte(*self)
    }
}

impl<const N: usize> MemValue for [u8; N] {
    #[inline]
    fn copy_from_mem<S: MemSource>(&mut self, source: S) {
        source.get_bytes(self.as_mut())
    }

    #[inline]
    fn copy_to_mem<D: MemDest>(&self, dest: D) {
        dest.set_bytes(self.as_ref())
    }
}

impl MemValue for i8 {
    #[inline]
    fn copy_from_mem<S: MemSource>(&mut self, source: S) {
        *self = source.get_byte() as i8
    }

    #[inline]
    fn copy_to_mem<D: MemDest>(&self, dest: D) {
        dest.set_byte(*self as u8)
    }
}

impl MemValue for u16 {
    #[inline]
    fn copy_from_mem<S: MemSource>(&mut self, source: S) {
        let mut bytes = [0u8; 2];
        source.get_bytes(bytes.as_mut());
        *self = u16::from_le_bytes(bytes);
    }

    #[inline]
    fn copy_to_mem<D: MemDest>(&self, dest: D) {
        let bytes = self.to_le_bytes();
        dest.set_bytes(bytes.as_ref());
    }
}

/// Context trait for accessing memory.
pub trait MemContext {
    /// Type of MemDevice in this context.
    type Mem: RootMemDevice;

    /// Gets the memory.
    fn mem(&self) -> &Self::Mem;

    /// Get a mutable reference to the memory.
    fn mem_mut(&mut self) -> &mut Self::Mem;
}

/// Makes a `MemDevice` into a `RootMemDevice` by treating all bytes past its end behave
/// as a `NullRom`.
#[derive(Debug, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub struct RootExtend<M>(M);

impl<M> RootExtend<M> {
    /// Performs reference-to-reference conversion from any `MemDevice` to `RootExtend`.
    #[inline]
    pub fn wrap_ref<'a>(m: &'a M) -> &'a Self {
        // RootExtend has the same memory representation (`repr(transparent)`) as `M` so
        // it is safe to transmute the reference with the same lifetime while preserving
        // mutability.
        unsafe { mem::transmute(m) }
    }

    /// Performs reference-to-reference conversion from any `MemDevice` to `RootExtend`.
    #[inline]
    pub fn wrap_mut<'a>(m: &'a mut M) -> &'a mut Self {
        // RootExtend has the same memory representation (`repr(transparent)`) as `M` so
        // it is safe to transmute the reference with the same lifetime while preserving
        // mutability.
        unsafe { mem::transmute(m) }
    }
}

impl<M: MemDevice> MemDevice for RootExtend<M> {
    const LEN: usize = u16::MAX as usize + 1;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        assert!(
            addr.relative() == addr.raw(),
            "Using RootExtend with offset address {}",
            addr
        );
        if addr.index() < M::LEN {
            self.0.read_byte_relative(addr)
        } else {
            check_addr!(RootExtend<M>, addr);
            0xff
        }
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, mut data: &mut [u8]) {
        assert!(
            addr.relative() == addr.raw(),
            "Using RootExtend with offset address {}",
            addr
        );
        check_addr!(RootExtend<M>, addr, data.len());
        if addr.index() < M::LEN {
            let num_read = (M::LEN - addr.index()).min(data.len());
            self.0.read_bytes_relative(addr, &mut data[..num_read]);
            data = &mut data[num_read..];
        }
        // Fill any bytes not covered by the actuall data with 0xff. check_addr ensures
        // all of data is in range, so we just fill it.
        data.fill(0xff);
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, data: u8) {
        assert!(
            addr.relative() == addr.raw(),
            "Using RootExtend with offset address {}",
            addr
        );
        if addr.index() < M::LEN {
            self.0.write_byte_relative(addr, data)
        } else {
            check_addr!(RootExtend<M>, addr);
        }
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        assert!(
            addr.relative() == addr.raw(),
            "Using RootExtend with offset address {}",
            addr
        );
        check_addr!(RootExtend<M>, addr, data.len());
        if addr.index() < M::LEN {
            let num_written = (M::LEN - addr.index()).min(data.len());
            self.0.write_bytes_relative(addr, &data[..num_written]);
        }
    }
}

impl<M: MemDevice> RootMemDevice for RootExtend<M> {}

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
    const LEN: usize = M::LEN;

    #[inline]
    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        self.0.read_bytes_relative(addr, data);
    }

    #[inline]
    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        self.0.read_byte_relative(addr)
    }

    #[inline]
    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        check_addr!(ReadOnly<M>, addr, data.len());
    }

    #[inline]
    fn write_byte_relative(&mut self, addr: RelativeAddr, _data: u8) {
        check_addr!(ReadOnly<M>, addr);
    }
}

impl<M: RootMemDevice> RootMemDevice for ReadOnly<M> {
    fn read_byte(&self, addr: u16) -> u8 {
        self.0.read_byte(addr)
    }

    fn read_bytes(&self, addr: u16, data: &mut [u8]) {
        self.0.read_bytes(addr, data)
    }

    fn write_byte(&mut self, _addr: u16, _val: u8) {}

    fn write_bytes(&mut self, _addr: u16, _data: &[u8]) {}
}

/// A rom which does bounds checks, but contains no actual memory (always returns 0xff,
/// ignores writes).
pub struct NullRom<const N: usize>;

impl<const N: usize> MemDevice for NullRom<N> {
    const LEN: usize = N;

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        check_addr!(NullRom<N>, addr, data.len());
        data.fill(0xff);
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        check_addr!(NullRom<N>, addr, data.len());
    }
}

const BIOS_LEN: usize = 0x100;

/// Rom for the bios, which is swapped out once started.
#[repr(transparent)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, MemDevice)]
#[memdev(passthrough)]
pub struct BiosRom(ReadOnly<[u8; BIOS_LEN]>);

impl BiosRom {
    /// Construct a `BiosRom` from an array of 256 bytes. Does not validate the contents.
    pub fn new(data: [u8; BIOS_LEN]) -> Self {
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
        Self(ReadOnly::new([0; BIOS_LEN]))
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
        if data.len() != BIOS_LEN {
            Err(BiosSizeError(data.len()))
        } else {
            let mut arr = [0; BIOS_LEN];
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
#[derive(Debug, Clone, Eq, PartialEq, MemDevice)]
#[memdev(byte, read = Self::get_byte, write = Self::set_byte)]
pub struct BiosEnable(bool);

impl BiosEnable {
    /// Return true if bios is enabled.
    pub fn enabled(&self) -> bool {
        self.0
    }

    fn get_byte(&self) -> u8 {
        self.0 as u8
    }

    fn set_byte(&mut self, data: u8) {
        // If value is false, stay false. If passed a non-zero value, become false.
        self.0 = self.0 && data == 0;
    }
}

impl Default for BiosEnable {
    fn default() -> Self {
        Self(true)
    }
}

impl<const N: usize> MemDevice for [u8; N] {
    const LEN: usize = N;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match self.get(addr.index()) {
            Some(val) => *val,
            None => panic!("Address {}  out of range for {} byte memory array", addr, N),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        match self.get_mut(addr.index()) {
            Some(val) => *val = value,
            None => panic!("Address {}  out of range for {} byte memory array", addr, N),
        }
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        match self.get(addr.range(data.len())) {
            Some(vals) => data.copy_from_slice(vals),
            None => panic!(
                "Address {} + {} byte slice out of range for {} byte memory array",
                addr,
                data.len(),
                N
            ),
        }
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        match self.get_mut(addr.range(data.len())) {
            Some(vals) => vals.copy_from_slice(data),
            None => panic!(
                "Address {} + {} byte slice out of range for {} byte memory array",
                addr,
                data.len(),
                N
            ),
        }
    }
}

impl RootMemDevice for [u8; 0x10000] {
    fn read_byte(&self, addr: u16) -> u8 {
        self[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) {
        self[addr as usize] = val;
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
}

impl<M> MaskableMem<M> {
    pub fn mask(&mut self) {
        self.masked = true;
    }

    pub fn unmask(&mut self) {
        self.masked = false;
    }

    /// Bypass masking to access the inner memory directly.
    pub fn bypass(&self) -> &M {
        &self.device
    }

    /// Bypass masking to access the inner memory directly.
    pub fn bypass_mut(&mut self) -> &mut M {
        &mut self.device
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
    const LEN: usize = M::LEN;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        if !self.masked {
            self.device.read_byte_relative(addr)
        } else {
            check_addr!(MaskableMem<M>, addr);
            0xff
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        if !self.masked {
            self.device.write_byte_relative(addr, value)
        } else {
            check_addr!(MaskableMem<M>, addr);
        }
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        if !self.masked {
            self.device.read_bytes_relative(addr, data)
        } else {
            check_addr!(MaskableMem<M>, addr, data.len());
            data.fill(0xff);
        }
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        if !self.masked {
            self.device.write_bytes_relative(addr, data)
        } else {
            check_addr!(MaskableMem<M>, addr, data.len());
        }
    }
}

impl<M: RootMemDevice> RootMemDevice for MaskableMem<M> {}

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
    const LEN: usize = 1;

    #[inline]
    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        check_addr!(u8, addr);
        *self
    }

    #[inline]
    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        check_addr!(u8, addr);
        *self = val;
    }

    #[inline]
    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        check_addr!(u8, addr, data.len());
        match data.first_mut() {
            Some(out) => *out = *self,
            None => {}
        }
    }

    #[inline]
    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        check_addr!(u8, addr, data.len());
        match data.first() {
            Some(val) => *self = *val,
            None => {}
        }
    }
}

// This makes sure that Box<dyn MemDevice> implements MemDevice (as well as Box<Anything that
// implements MemDevice>).
impl<D: MemDevice> MemDevice for Box<D> {
    const LEN: usize = D::LEN;

    #[inline]
    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        (**self).read_byte_relative(addr)
    }

    #[inline]
    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        (**self).write_byte_relative(addr, value)
    }

    #[inline]
    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        (**self).read_bytes_relative(addr, data)
    }

    #[inline]
    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        (**self).write_bytes_relative(addr, data)
    }
}

impl<D: RootMemDevice> RootMemDevice for Box<D> {
    #[inline]
    fn read_byte(&self, addr: u16) -> u8 {
        (**self).read_byte(addr)
    }

    #[inline]
    fn read_bytes(&self, addr: u16, data: &mut [u8]) {
        (**self).read_bytes(addr, data)
    }

    #[inline]
    fn write_byte(&mut self, addr: u16, data: u8) {
        (**self).write_byte(addr, data)
    }

    #[inline]
    fn write_bytes(&mut self, addr: u16, data: &[u8]) {
        (**self).write_bytes(addr, data)
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

memdev_fields!(MemMappedIo, len: 0x80, {
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
});

pub type Vram = MaskableMem<MaskableMem<[u8; 0x2000]>>;
pub type Oam = MaskableMem<MaskableMem<[u8; 160]>>;

/// Simple memory device that consists of just 0x10000 bytes treated all as ram. This
/// MemDevice is primarily intended for use in tests. The only difference between this and
/// `Box<[u8; 0x10000]>` is that it provides convenient From implementations for
/// converting from slices and arrays of arbitrary numbers of bytes, which is convenient
/// for testing.
#[derive(Debug, MemDevice)]
#[memdev(passthrough)]
pub struct AllRam(Box<[u8; 0x10000]>);

impl RootMemDevice for AllRam {
    #[inline]
    fn read_byte(&self, addr: u16) -> u8 {
        self.0.read_byte(addr)
    }

    #[inline]
    fn read_bytes(&self, addr: u16, data: &mut [u8]) {
        self.0.read_bytes(addr, data)
    }

    #[inline]
    fn write_byte(&mut self, addr: u16, val: u8) {
        self.0.write_byte(addr, val);
    }

    #[inline]
    fn write_bytes(&mut self, addr: u16, data: &[u8]) {
        self.0.write_bytes(addr, data)
    }
}

impl From<&[u8]> for AllRam {
    /// Creates an AllRam with initial data set by copying from a byte slice.
    fn from(bytes: &[u8]) -> Self {
        assert!(bytes.len() <= 0x10000);
        let mut ram = Box::new([0; 0x10000]);
        ram[..bytes.len()].copy_from_slice(bytes);
        Self(ram)
    }
}

impl<const N: usize> From<&[u8; N]> for AllRam {
    /// Creates an AllRam with initial data set by copying from an array of any size less
    /// than 0x10000.
    fn from(bytes: &[u8; N]) -> Self {
        (&bytes[..]).into()
    }
}

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
        }
    }
}

impl Default for GbMmu {
    fn default() -> Self {
        Self::new(Default::default(), Cartridge::None)
    }
}

impl MemDevice for GbMmu {
    const LEN: usize = u16::MAX as usize + 1;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        trace!("Read from MMU address {:#x}", addr.raw);
        dispatch_memdev_byte!(GbMmu, addr, |addr| {
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.read_byte_relative(addr),
            0x0..=0x7fff => self.cart.read_byte_relative(addr),
            0x8000..=0x9fff => self.vram.read_byte_relative(addr),
            // Cartridge ram uses the same device as cartridge rom, so skip over the
            // number of bytes in Cart ROM when reading from Cart RAM.
            0xa000..=0xbfff => self.cart.read_byte_relative(addr.skip_over(0x8000)),
            0xc000..=0xdfff => self.wram.read_byte_relative(addr),
            0xe000..=0xfdff => self.wram.read_byte_relative(addr),
            0xfe00..=0xfe9f => self.oam.read_byte_relative(addr),
            // Unmapped portion above sprite information, always returns 0xff.
            0xfea0..=0xfeff => 0xff,
            0xff00..=0xff7f => self.io.read_byte_relative(addr),
            0xff80..=0xfffe => self.zram.read_byte_relative(addr),
            0xffff => self.interrupt_enable.read_byte_relative(addr),
        })
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        trace!("Read {} bytes from MMU address {:#x}", data.len(), addr.raw);
        dispatch_memdev_bytes!(GbMmu, addr, data, |addr, mut data| {
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.read_bytes_relative(addr, data),
            0x0..=0x7fff => self.cart.read_bytes_relative(addr, data),
            0x8000..=0x9fff => self.vram.read_bytes_relative(addr, data),
            // Cartridge ram uses the same device as cartridge rom, so skip over the
            // number of bytes in Cart ROM when reading from Cart RAM.
            0xa000..=0xbfff => self.cart.read_bytes_relative(addr.skip_over(0x8000), data),
            0xc000..=0xdfff => self.wram.read_bytes_relative(addr, data),
            0xe000..=0xfdff => self.wram.read_bytes_relative(addr, data),
            0xfe00..=0xfe9f => self.oam.read_bytes_relative(addr, data),
            // Unmapped portion above sprite information, always returns 0xff.
            0xfea0..=0xfeff => data.fill(0xff),
            0xff00..=0xff7f => self.io.read_bytes_relative(addr, data),
            0xff80..=0xfffe => self.zram.read_bytes_relative(addr, data),
            0xffff => self.interrupt_enable.read_bytes_relative(addr, data),
        });
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        trace!("Write at MMU address {:#x}", addr.raw);
        // Address guaranteed to be in range since we cover the whole memory space.
        dispatch_memdev_byte!(GbMmu, addr, |addr| {
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.write_byte_relative(addr, value),
            0x0..=0x7fff => self.cart.write_byte_relative(addr, value),
            0x8000..=0x9fff => self.vram.write_byte_relative(addr, value),
            // Cartridge ram uses the same device as cartridge rom, so skip over the
            // number of bytes in Cart ROM when reading from Cart RAM.
            0xa000..=0xbfff => self.cart.write_byte_relative(addr.skip_over(0x8000), value),
            0xc000..=0xdfff => self.wram.write_byte_relative(addr, value),
            0xe000..=0xfdff => self.wram.write_byte_relative(addr, value),
            0xfe00..=0xfe9f => self.oam.write_byte_relative(addr, value),
            // Unmapped portion above sprite information.
            0xfea0..=0xfeff => (),
            0xff00..=0xff7f => self.io.write_byte_relative(addr, value),
            0xff80..=0xfffe => self.zram.write_byte_relative(addr, value),
            0xffff => self.interrupt_enable.write_byte_relative(addr, value),
        })
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        assert!(
            addr.relative() == addr.raw(),
            "Using Root MMU with offset address {}",
            addr
        );
        trace!("Write {} bytes at MMU address {:#x}", data.len(), addr.raw);
        dispatch_memdev_bytes!(GbMmu, addr, data, |addr, ref data| {
            0x0..=0xff if self.io.bios_enable.enabled() => self.bios.write_bytes_relative(addr, data),
            0x0..=0x7fff => self.cart.write_bytes_relative(addr, data),
            0x8000..=0x9fff => self.vram.write_bytes_relative(addr, data),
            // Cartridge ram uses the same device as cartridge rom, so skip over the
            // number of bytes in Cart ROM when reading from Cart RAM.
            0xa000..=0xbfff => self.cart.write_bytes_relative(addr.skip_over(0x8000), data),
            0xc000..=0xdfff => self.wram.write_bytes_relative(addr, data),
            0xe000..=0xfdff => self.wram.write_bytes_relative(addr, data),
            0xfe00..=0xfe9f => self.oam.write_bytes_relative(addr, data),
            // Unmapped portion above sprite information, always returns 0.
            0xfea0..=0xfeff => {},
            0xff00..=0xff7f => self.io.write_bytes_relative(addr, data),
            0xff80..=0xfffe => self.zram.write_bytes_relative(addr, data),
            0xffff => self.interrupt_enable.write_bytes_relative(addr, data),
        });
    }
}

impl RootMemDevice for GbMmu {}

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

#[cfg(test)]
mod tests {
    use super::*;

    use rand::distributions::Uniform;
    use rand::{Rng, SeedableRng};
    use rand_pcg::Pcg64Mcg;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn mmu_range_readwrite() {
        init();

        let mut bios = BiosRom::new([0u8; 0x100]);
        for (i, byte) in bios.0 .0.iter_mut().enumerate() {
            *byte = i as u8;
        }
        let mut mmu_individual = GbMmu::new(bios, Cartridge::None);
        let mut mmu_slice = GbMmu::new(bios, Cartridge::None);

        let mut input_buf = vec![];
        let mut output_buf_individual = vec![];
        let mut output_buf_slice = vec![];
        let mut rng =
            Pcg64Mcg::from_seed([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF]);

        let len_dist = Uniform::new_inclusive(1, 0x400);

        assert_eq!(mmu_individual, mmu_slice);

        for _ in 0..0x80000 {
            let addr: u16 = rng.gen();
            let len = rng.sample(&len_dist);
            input_buf.resize(len, 0u8);
            rng.fill(&mut input_buf[..]);

            for (i, &val) in input_buf.iter().enumerate() {
                mmu_individual.write_byte(addr.wrapping_add(i as u16), val);
            }
            mmu_slice.write_bytes(addr, &input_buf);

            output_buf_individual.resize(len, 0u8);
            for (i, res) in output_buf_individual.iter_mut().enumerate() {
                *res = mmu_individual.read_byte(addr.wrapping_add(i as u16));
            }
            output_buf_slice.resize(len, 0u8);
            mmu_slice.read_bytes(addr, &mut output_buf_slice);

            assert_eq!(output_buf_individual, output_buf_slice);
        }

        // To reduce the test runtime, only compare the final result.
        assert_eq!(mmu_individual, mmu_slice);
    }
}
