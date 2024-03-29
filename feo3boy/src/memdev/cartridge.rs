//! Implementation of different cartridge types.

use std::convert::TryFrom;
use std::io::{self, ErrorKind, Read, Write};
use std::num::NonZeroU8;
use std::slice;

use log::warn;
use thiserror::Error;

use super::{MemDevice, NullRom, ReadOnly, RelativeAddr};

/// Length of a cartridge in bytes, counting both ram and rom. This counts the number of
/// bytes in ram/rom contiguously. The gap between ram and rom only exists in how the
/// cartridge memory space is mapped into memory by the memory manager.
const CART_MEMDEV_LEN: usize = 0xA000;

/// Errors that can result from attempting to parse a cartridge dump.
#[derive(Debug, Error)]
pub enum ParseCartridgeError {
    /// The MBC type in the cartridge header is known, but we haven't implemented it yet.
    #[error("Unsupported MBC type: {0:#04X}")]
    UnsupportedMbcType(u8),
    /// The MBC type in the cartridge header was not recognized.
    #[error("Unknown MBC type: {0:#04X}")]
    UnknownMbcType(u8),
    /// The rom size code was not valid for this rom type.
    #[error("Unsupported rom size {rom_size:#04X} banks for rom type {rom_type:#04X}")]
    UnsupportedRomSize {
        /// The rom type code.
        rom_type: u8,
        /// The rom size in banks.
        rom_size: usize,
    },
    /// The ram size code was not valid for this rom type.
    #[error("Unsupported ram size {ram_size:#04X} banks for rom type {rom_type:#04X}")]
    UnsupportedRamSize {
        /// The rom type code.
        rom_type: u8,
        /// The ram size in banks.
        ram_size: usize,
    },
    /// The Rom-size code was not recognized.
    #[error("Unrecognized rom-size code: {0:#04X}")]
    UnrecognizedRomSizeCode(u8),
    /// The Ram-size code was not recognized.
    #[error("Unrecognized ram-size code: {0:#04X}")]
    UnrecognizedRamSizeCode(u8),
    /// Extra data was found after the expected end of the cartridge rom.
    #[error("Found extra data after the expected end of the cartridge rom.")]
    ExtraData,
    /// Ran out of cartridge data before the end of the cartridge. This may mean a bank was
    /// incomplete, or it may mean that the number of banks in the file did not match the expected
    /// number in the header. The source will always be an IO error with `ErrorKind::UnexpectedEof`.
    #[error("Unexpected end of cartridge dump")]
    InsufficientData(#[source] io::Error),
    /// A general IO error was encountered.
    #[error("Error while trying to read cartridge: {0:?}")]
    IoError(#[source] io::Error),
}

impl From<io::Error> for ParseCartridgeError {
    fn from(err: io::Error) -> Self {
        match err.kind() {
            ErrorKind::UnexpectedEof => ParseCartridgeError::InsufficientData(err),
            _ => ParseCartridgeError::IoError(err),
        }
    }
}

pub trait SaveData {
    fn write_save_data(&self, writer: impl Write) -> Result<(), io::Error>;

    fn load_save_data(&mut self, reader: impl Read) -> Result<(), io::Error>;

    fn has_save_data(&self) -> bool;
}

/// Enum of different cartridge types.
///
/// Note that in the GB, the cartridges occupy two memory spaces, one before GPU ram for the ROM
/// portion, and one after for the RAM. The `Cartridge` tye and all of the rom implementation types
/// ignore this split, and map the ram portion directly after the rom. It is the responsibility of
/// the caller to remap the memory spaces as needed to insert the GPU ram.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Cartridge {
    /// No cartridge. All reads return 0 and all writes are ignored.
    None,
    /// A basic [`RomOnly`] cartridge.
    RomOnly(RomOnly),
    /// An [`Mbc1Rom`] cartridge.
    Mbc1(Mbc1Rom),
    /// An [`Mbc3Rom`] cartridge.
    Mbc3(Mbc3Rom),
}

impl Cartridge {
    /// Parse a cartridge rom.
    ///
    /// Note: expects the cartridge header to contain accurate data about the number of ram and rom
    /// banks.
    pub fn parse(mut reader: impl Read) -> Result<Cartridge, ParseCartridgeError> {
        /// Location of the cartridge type in the header.
        const CART_TYPE: usize = 0x147;
        /// Location of the cartridge rom size in the header.
        const ROM_SIZE: usize = 0x148;
        /// Location of the cartridge ram size in the header.
        const RAM_SIZE: usize = 0x149;
        /// Location of the cartridge header checksum in the header.
        const HEADER_CHECKSUM: usize = 0x14d;
        /// Header length (including the first 0x100 bytes which aren't really part of the header).
        /// This is the number of bytes you need to read to get the whole header.
        const HEADER_LEN: usize = 0x150;

        /// Finish loading bank 0, which is partially read in order to get the cartridge header.
        fn finish_bank0(
            header: &[u8; HEADER_LEN],
            mut reader: impl Read,
            dest: &mut [u8; ROM_BANK_SIZE],
        ) -> Result<(), ParseCartridgeError> {
            dest[..HEADER_LEN].copy_from_slice(&header[..]);
            reader.read_exact(&mut dest[HEADER_LEN..])?;
            Ok(())
        }

        /// Tries to read one more byte to make sure the reader is actually at EOF.
        fn ensure_eof(mut reader: impl Read) -> Result<(), ParseCartridgeError> {
            let mut buf = 0u8;
            if reader.read(slice::from_mut(&mut buf))? == 0 {
                // zero bytes read = EOF.
                Ok(())
            } else {
                Err(ParseCartridgeError::ExtraData)
            }
        }

        /// Size of the rom in number of banks.
        fn rom_size(header: &[u8; HEADER_LEN]) -> Result<usize, ParseCartridgeError> {
            let code = header[ROM_SIZE];
            match code {
                0..=8 => Ok(2 << code),
                0x52..=0x54 => {
                    let low = (code & 0xf) as usize;
                    let high = ((code & 0xf0) >> 4) as usize;
                    let banks = (2 << low) + (2 << high);
                    warn!("Oddball rom size of {} banks is not supported", banks);
                    Err(ParseCartridgeError::UnrecognizedRomSizeCode(code))
                }
                _ => Err(ParseCartridgeError::UnrecognizedRomSizeCode(code)),
            }
        }

        /// Size of the ram in number of banks.
        fn ram_size(header: &[u8; HEADER_LEN]) -> Result<usize, ParseCartridgeError> {
            let code = header[RAM_SIZE];
            match code {
                0 => Ok(0),
                1 => {
                    warn!("2 KiB ram size is unsupported, using 1 8 KiB ram bank instead.");
                    Ok(1)
                }
                2 => Ok(1),
                3 => Ok(4),
                4 => Ok(16),
                5 => Ok(8),
                _ => Err(ParseCartridgeError::UnrecognizedRamSizeCode(code)),
            }
        }

        // Buffer of the next loaded bank.
        let mut header = [0u8; HEADER_LEN];
        // Load the first bank in order to read the cartridge header.
        reader.read_exact(&mut header[..])?;

        let computed_checksum = header[0x134..=0x14c]
            .iter()
            .fold(0u8, |x, &h| x.wrapping_sub(h).wrapping_sub(1));
        let header_checksum = header[HEADER_CHECKSUM];
        if computed_checksum != header_checksum {
            warn!(
                "Header checksum {} did not match computed checksum {}",
                header_checksum, computed_checksum
            );
        }

        match header[CART_TYPE] {
            code @ (0 | 8 | 9) => {
                match rom_size(&header) {
                    Ok(size) if size == 2 => {},
                    Ok(size) => warn!("RomOnly cartridge had rom size {}, but RomOnly always has exactly 2 banks.", size),
                    Err(ParseCartridgeError::UnrecognizedRomSizeCode(code)) => warn!("RomOnly cartridge had an invalid rom size code {}", code),
                    Err(_) => unreachable!(),
                }
                match (code, ram_size(&header)) {
                    (0, Ok(0)) | (8 | 9, Ok(1)) => {}
                    (0, Ok(size)) => warn!("RomOnly cartridge with no ram specified {} ram banks. It will be run without ram.", size),
                    (8 | 9, Ok(size)) => warn!("RomOnly cartridge with ram specified {} ram banks. It will be run with 1 ram bank.", size),
                    (_, Err(ParseCartridgeError::UnrecognizedRamSizeCode(code))) => warn!("RomOnly cartrige had an unrecognized ram size code {}.", code),
                    _ => unreachable!(),
                }

                let mut rom = RomOnly::empty();
                finish_bank0(&header, &mut reader, &mut rom.rom_banks[0].0)?;
                reader.read_exact(&mut rom.rom_banks[1].0[..])?;
                ensure_eof(reader)?;

                if matches!(code, 8 | 9) {
                    rom.ram_bank = Some(Box::new([0u8; RAM_BANK_SIZE]));
                    rom.save_ram = code == 9;
                }
                Ok(Cartridge::RomOnly(rom))
            }
            rom_type @ 1..=3 => {
                let rom_size = rom_size(&header)?;
                if rom_size > 128 {
                    return Err(ParseCartridgeError::UnsupportedRomSize { rom_type, rom_size });
                }
                let ram_size = match (rom_type, ram_size(&header)) {
                    (1, Err(e)) => {
                        warn!("Error parsing ram type for ramless MBC1: {}", e);
                        0
                    }
                    (1, Ok(0)) => 0,
                    (1, Ok(size)) => {
                        warn!("Got {} ram banks on a ramless MBC1, expected 0.", size);
                        0
                    }
                    (2 | 3, Err(e)) => return Err(e),
                    (2 | 3, Ok(0)) => {
                        warn!("MBC1 + RAM cartridge had 0 ram banks");
                        0
                    }
                    (2 | 3, Ok(size @ (1 | 4))) => size,
                    (2 | 3, Ok(ram_size)) => {
                        return Err(ParseCartridgeError::UnsupportedRamSize { rom_type, ram_size })
                    }
                    _ => unreachable!(),
                };

                let mut rom_banks = Vec::with_capacity(rom_size);
                rom_banks.push(ReadOnly([0u8; ROM_BANK_SIZE]));
                finish_bank0(&header, &mut reader, &mut rom_banks[0].0)?;
                for bank in 1..rom_size {
                    rom_banks.push(ReadOnly([0u8; ROM_BANK_SIZE]));
                    reader.read(&mut rom_banks[bank].0[..])?;
                }
                ensure_eof(reader)?;

                Ok(Cartridge::Mbc1(Mbc1Rom::new(
                    rom_banks,
                    ram_size,
                    rom_type == 3,
                )))
            }
            rom_type @ 0xf..=0x13 => {
                let rom_size = rom_size(&header)?;
                if rom_size > 128 {
                    return Err(ParseCartridgeError::UnsupportedRomSize { rom_type, rom_size });
                }
                let ram_size = match (rom_type, ram_size(&header)) {
                    (0xf | 0x11, Err(e)) => {
                        warn!("Error parsing ram type for ramless MBC3: {}", e);
                        0
                    }
                    (0xf | 0x11, Ok(0)) => 0,
                    (0xf | 0x11, Ok(size)) => {
                        warn!("Got {} ram banks on a ramless MBC3, expected 0.", size);
                        0
                    }
                    (0x10 | 0x12 | 0x13, Err(e)) => return Err(e),
                    (0x10 | 0x12 | 0x13, Ok(size @ (1 | 4))) => size,
                    (0x10 | 0x12 | 0x13, Ok(ram_size)) => {
                        return Err(ParseCartridgeError::UnsupportedRamSize { rom_type, ram_size })
                    }
                    _ => unreachable!(),
                };

                let mut rom_banks = Vec::with_capacity(rom_size);
                rom_banks.push(ReadOnly([0u8; ROM_BANK_SIZE]));
                finish_bank0(&header, &mut reader, &mut rom_banks[0].0)?;
                for bank in 1..rom_size {
                    rom_banks.push(ReadOnly([0u8; ROM_BANK_SIZE]));
                    reader.read(&mut rom_banks[bank].0[..])?;
                }
                ensure_eof(reader)?;

                Ok(Cartridge::Mbc3(Mbc3Rom::new(
                    rom_banks,
                    ram_size,
                    rom_type == 0x10 || rom_type == 0x13,
                )))
            }
            code @ (5..=6 | 0xb..=0xd | 0x19..=0x1e | 0x20 | 0x22 | 0xfc..=0xff) => {
                Err(ParseCartridgeError::UnsupportedMbcType(code))
            }
            code => Err(ParseCartridgeError::UnknownMbcType(code)),
        }
    }
}

impl TryFrom<&[u8]> for Cartridge {
    type Error = ParseCartridgeError;

    fn try_from(data: &[u8]) -> Result<Self, Self::Error> {
        Cartridge::parse(data)
    }
}

impl TryFrom<Vec<u8>> for Cartridge {
    type Error = ParseCartridgeError;

    fn try_from(data: Vec<u8>) -> Result<Self, Self::Error> {
        Self::try_from(&*data)
    }
}

impl MemDevice for Cartridge {
    const LEN: usize = CART_MEMDEV_LEN;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match self {
            Cartridge::None => NullRom::<CART_MEMDEV_LEN>.read_byte_relative(addr),
            Cartridge::RomOnly(ref cart) => cart.read_byte_relative(addr),
            Cartridge::Mbc1(ref cart) => cart.read_byte_relative(addr),
            Cartridge::Mbc3(ref cart) => cart.read_byte_relative(addr),
        }
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        match self {
            Cartridge::None => NullRom::<CART_MEMDEV_LEN>.read_bytes_relative(addr, data),
            Cartridge::RomOnly(ref cart) => cart.read_bytes_relative(addr, data),
            Cartridge::Mbc1(ref cart) => cart.read_bytes_relative(addr, data),
            Cartridge::Mbc3(ref cart) => cart.read_bytes_relative(addr, data),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        match self {
            Cartridge::None => NullRom::<CART_MEMDEV_LEN>.write_byte_relative(addr, value),
            Cartridge::RomOnly(ref mut cart) => cart.write_byte_relative(addr, value),
            Cartridge::Mbc1(ref mut cart) => cart.write_byte_relative(addr, value),
            Cartridge::Mbc3(ref mut cart) => cart.write_byte_relative(addr, value),
        }
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        match self {
            Cartridge::None => NullRom::<CART_MEMDEV_LEN>.write_bytes_relative(addr, data),
            Cartridge::RomOnly(ref mut cart) => cart.write_bytes_relative(addr, data),
            Cartridge::Mbc1(ref mut cart) => cart.write_bytes_relative(addr, data),
            Cartridge::Mbc3(ref mut cart) => cart.write_bytes_relative(addr, data),
        }
    }
}

impl SaveData for Cartridge {
    fn write_save_data(&self, writer: impl Write) -> Result<(), io::Error> {
        match self {
            Cartridge::None => Ok(()),
            Cartridge::RomOnly(ref cart) => cart.write_save_data(writer),
            Cartridge::Mbc1(ref cart) => cart.write_save_data(writer),
            Cartridge::Mbc3(ref cart) => cart.write_save_data(writer),
        }
    }

    fn load_save_data(&mut self, reader: impl Read) -> Result<(), io::Error> {
        match self {
            Cartridge::None => Ok(()),
            Cartridge::RomOnly(ref mut cart) => cart.load_save_data(reader),
            Cartridge::Mbc1(ref mut cart) => cart.load_save_data(reader),
            Cartridge::Mbc3(ref mut cart) => cart.load_save_data(reader),
        }
    }

    fn has_save_data(&self) -> bool {
        match self {
            Cartridge::None => false,
            Cartridge::RomOnly(ref cart) => cart.has_save_data(),
            Cartridge::Mbc1(ref cart) => cart.has_save_data(),
            Cartridge::Mbc3(ref cart) => cart.has_save_data(),
        }
    }
}

/// Rom banks are 0x4000 = 16 KiB.
const ROM_BANK_SIZE: usize = 0x4000;

/// A single 16 KiB rom bank within a cartridge.
pub type RomBank = ReadOnly<[u8; ROM_BANK_SIZE]>;

/// Ram banks are 0x2000 = 8 KiB.
const RAM_BANK_SIZE: usize = 0x2000;

/// A single 8 KiB ram bank within a cartridge.
pub type RamBank = [u8; RAM_BANK_SIZE];

/// Cartridge which has only 2 rom banks and optionally up to 1 ram bank.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RomOnly {
    /// Rom banks. Both are always accessible.
    pub rom_banks: Box<[RomBank; 2]>,
    /// Ram bank, may or may not be included.
    pub ram_bank: Option<Box<RamBank>>,
    /// Whether ram is saved when the device is powered off. (Does the ram have a battery?)
    save_ram: bool,
}

impl RomOnly {
    pub fn new(
        rom_banks: Box<[RomBank; 2]>,
        ram_bank: Option<Box<RamBank>>,
        save_ram: bool,
    ) -> Self {
        Self {
            rom_banks,
            ram_bank,
            save_ram,
        }
    }

    /// Constructs a new `RomOnly` with empty (all 0) rom banks and no ram bank.
    pub fn empty() -> Self {
        Self {
            rom_banks: Box::new([ReadOnly([0u8; ROM_BANK_SIZE]); 2]),
            ram_bank: None,
            save_ram: false,
        }
    }
}

impl MemDevice for RomOnly {
    const LEN: usize = CART_MEMDEV_LEN;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(RomOnly, addr, |addr| {
            0..=0x3fff => self.rom_banks[0].read_byte_relative(addr),
            0x4000..=0x7fff => self.rom_banks[1].read_byte_relative(addr),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref ram) => ram.read_byte_relative(addr),
                None => 0xff,
            },
        })
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        dispatch_memdev_bytes!(RomOnly, addr, data, |addr, mut data| {
            0..=0x3fff => self.rom_banks[0].read_bytes_relative(addr, data),
            0x4000..=0x7fff => self.rom_banks[1].read_bytes_relative(addr, data),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref ram) => ram.read_bytes_relative(addr, data),
                None => data.fill(0xff),
            },
        })
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        dispatch_memdev_byte!(RomOnly, addr, |addr| {
            0..=0x3fff => self.rom_banks[0].write_byte_relative(addr, value),
            0x4000..=0x7fff => self.rom_banks[1].write_byte_relative(addr, value),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref mut ram) => ram.write_byte_relative(addr, value),
                None => {}
            },
        })
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        dispatch_memdev_bytes!(RomOnly, addr, data, |addr, ref data| {
            0..=0x3fff => self.rom_banks[0].write_bytes_relative(addr, data),
            0x4000..=0x7fff => self.rom_banks[1].write_bytes_relative(addr, data),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref mut ram) => ram.write_bytes_relative(addr, data),
                None => {},
            },
        })
    }
}

impl SaveData for RomOnly {
    fn write_save_data(&self, mut writer: impl Write) -> Result<(), io::Error> {
        if self.save_ram {
            match self.ram_bank {
                Some(ref ram_bank) => match writer.write_all(ram_bank.as_ref()) {
                    Ok(_) => Ok(()),
                    Err(e) => return Err(e),
                },
                None => Ok(()),
            }
        } else {
            Ok(())
        }
    }

    fn load_save_data(&mut self, mut reader: impl Read) -> Result<(), io::Error> {
        if self.save_ram {
            match self.ram_bank {
                Some(ref mut ram_bank) => match reader.read_exact(ram_bank.as_mut()) {
                    Ok(_) => Ok(()),
                    Err(e) => return Err(e),
                },
                None => Ok(()),
            }
        } else {
            Ok(())
        }
    }

    fn has_save_data(&self) -> bool {
        self.save_ram
    }
}

/// Variant 1 of the system ROMs.

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mbc1Rom {
    /// Set of rom banks loaded from the cartridge.
    rom_banks: Vec<RomBank>,
    /// Set of ram banks on this Mbc1Rom, if any. If none, this will be an empty vector.
    ram_banks: Vec<RamBank>,
    /// Whether ram is saved when the device is powered off. (Does the ram have a battery?)
    save_ram: bool,

    // Reigsters:
    /// Whether ram is enabled for reading/writing. Otherwise writes are ignored and reads return
    /// dummy values.
    ram_enable: bool,
    /// Rom bank select. This is the low-order 5 bits (0..5) of the rom bank.
    rom_bank: NonZeroU8,
    /// Bank set is a 2 bit register that either selects the ram-bank or the high-order 2 bits
    /// (5..7) of the rom bank, depending on the mode register. Note that because these two bits
    /// are shared between rom and ram, if mode is 0, only ram bank 0 is accessible, and if mode is
    /// 1, only rom banks 0..32 are accessible. Note also that the behavior depends on the relative
    /// size of ram and rom.
    bank_set: u8,
    /// Switches between simple and advanced banking mode.
    ///
    /// In simple banking mode, ram banking is disabled, and rom banking only affects the 4000-7FFF
    /// range.
    ///
    /// In advanced mode the banking behavior depends on the size of the ram/rom. If the cartridge
    /// is large-ram, advanced banking mode switches between ram banks using the bank_set register.
    /// If the cartridge is large-rom, the bank set register instead applies to both the high order
    /// bits of the bank set *and* to select the "fixed" bank.
    advanced_banking_mode: bool,
}

impl Mbc1Rom {
    /// Construct a new Mbc1Rom with the given rom banks and number of ram banks.
    fn new(rom_banks: Vec<RomBank>, num_ram_banks: usize, save_ram: bool) -> Self {
        assert!(rom_banks.len() >= 2, "Must have at least 2 rom banks.");
        assert!(
            rom_banks.len() <= 128,
            "MBC1 Rom can have at most 128 rom banks."
        );
        assert!(
            rom_banks.len().count_ones() == 1,
            "Number of rom banks must be a power of 2."
        );
        assert!(num_ram_banks <= 4, "MBC1 Rom can have at most 4 ram banks.");
        assert!(
            num_ram_banks == 0 || num_ram_banks.count_ones() == 1,
            "Number of ram banks must be a power of 2."
        );
        if rom_banks.len() > 32 && num_ram_banks > 1 {
            // It is unclear to me what happens if a cartridge has both > 32 rom banks and > 1 ram
            // bank.  This doc https://gbdev.io/pandocs/MBC1.html describes the situation with > 1
            // ram bank and > 32 rom banks, but not both. Perhaps there were no official cartridges
            // where that was the case.
            //
            // The implemenation I decided to go with was to just always apply the bank_set bits,
            // and then modulo by the number of banks of ram/rom. If only one of ram/rom is large
            // enough to require using bank_set, the behavior is definitely correct, but if both are
            // large enough to need bank_set, then both will be banked simultaneously, and I don't
            // know if that's right.
            warn!("MBC1 Rom is both Large Ram and Large Rom. Banking behavior may be wrong.");
        }
        Mbc1Rom {
            rom_banks,
            ram_banks: vec![[0u8; RAM_BANK_SIZE]; num_ram_banks],
            save_ram,
            ram_enable: false,
            rom_bank: NonZeroU8::new(1).unwrap(),
            bank_set: 0,
            advanced_banking_mode: false,
        }
    }

    /// Whether ram is enabled for reading/writing. Otherwise writes are ignored and reads return
    /// dummy values.
    #[inline]
    pub fn ram_enable(&self) -> bool {
        self.ram_enable
    }

    /// Rom bank select. This is the low-order 5 bits (0..5) of the rom bank.
    #[inline]
    pub fn rom_bank(&self) -> NonZeroU8 {
        self.rom_bank
    }

    /// Bank set is a 2 bit register that either selects the ram-bank or the high-order 2 bits
    /// (5..7) of the rom bank, depending on the mode register. Note that because these two bits
    /// are shared between rom and ram, if mode is 0, only ram bank 0 is accessible, and if mode is
    /// 1, only rom banks 0..32 are accessible. Note also that the behavior depends on the relative
    /// size of ram and rom.
    #[inline]
    pub fn bank_set(&self) -> u8 {
        self.bank_set
    }

    /// Switches between simple and advanced banking mode.
    ///
    /// In simple banking mode, ram banking is disabled, and rom banking only affects the 4000-7FFF
    /// range.
    ///
    /// In advanced mode the banking behavior depends on the size of the ram/rom. If the cartridge
    /// is large-ram, advanced banking mode switches between ram banks using the bank_set register.
    /// If the cartridge is large-rom, the bank set register instead applies to both the high order
    /// bits of the bank set *and* to select the "fixed" bank.
    #[inline]
    pub fn advanced_banking_mode(&self) -> bool {
        self.advanced_banking_mode
    }

    /// Get the index of the lower rom bank currently selected.
    #[inline]
    pub fn selected_lower_bank(&self) -> usize {
        if self.advanced_banking_mode {
            (self.bank_set as usize * 32) % self.rom_banks.len()
        } else {
            0
        }
    }

    /// Get the index of the upper rom bank currently selected.
    #[inline]
    pub fn selected_upper_bank(&self) -> usize {
        let low_order = self.rom_bank.get();
        let high_order = self.bank_set << 5;
        (low_order | high_order) as usize % self.rom_banks.len()
    }

    /// Get the currently selected ram bank indes, regardless of whether ram is enabled.
    #[inline]
    pub fn selected_ram_bank(&self) -> usize {
        if self.ram_banks.is_empty() || !self.advanced_banking_mode {
            0
        } else {
            self.bank_set as usize % self.ram_banks.len()
        }
    }

    /// Get a reference to the set of rom banks.
    pub fn rom_banks(&self) -> &[RomBank] {
        self.rom_banks.as_ref()
    }

    /// Get a reference to the set of rom banks.
    pub fn ram_banks(&self) -> &[RamBank] {
        self.ram_banks.as_ref()
    }

    /// Convenient access to the "fixed" lower rom bank. This bank only changes in Advanced rom
    /// mode.
    pub fn lower_bank(&self) -> &RomBank {
        &self.rom_banks[self.selected_lower_bank()]
    }

    /// Get the currently selected rom bank. This will never be bank 0, 32, 64, or 96.
    pub fn upper_bank(&self) -> &RomBank {
        &self.rom_banks[self.selected_upper_bank()]
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    pub fn ram_bank(&self) -> Option<&RamBank> {
        if self.ram_banks.is_empty() || !self.ram_enable {
            None
        } else {
            let bank = self.selected_ram_bank();
            Some(&self.ram_banks[bank])
        }
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    fn ram_bank_mut(&mut self) -> Option<&mut RamBank> {
        if self.ram_banks.is_empty() || !self.ram_enable {
            None
        } else {
            let bank = self.selected_ram_bank();
            Some(&mut self.ram_banks[bank])
        }
    }
}

impl MemDevice for Mbc1Rom {
    const LEN: usize = CART_MEMDEV_LEN;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(Mbc1Rom, addr, |addr| {
            0..=0x3fff => self.lower_bank().read_byte_relative(addr),
            0x4000..=0x7fff => self.upper_bank().read_byte_relative(addr),
            0x8000..=0x9fff => match self.ram_bank() {
                Some(bank) => bank.read_byte_relative(addr),
                None => 0xff,
            },
        })
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        dispatch_memdev_bytes!(Mbc1Rom, addr, data, |addr, mut data| {
            0..=0x3fff => self.lower_bank().read_bytes_relative(addr, data),
            0x4000..=0x7fff => self.upper_bank().read_bytes_relative(addr, data),
            0x8000..=0x9fff => match self.ram_bank() {
                Some(bank) => bank.read_bytes_relative(addr, data),
                None => data.fill(0xff),
            },
        })
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        dispatch_memdev_byte!(Mbc1Rom, addr, |addr| {
            0x0000..=0x1fff => self.ram_enable = (value & 0xF) == 0xA,
            // Set the low-order bits of the rom-bank selection from the lower 5 bits of the
            // provided value. If 0 is provided, raise the value to 1.
            0x2000..=0x3fff => self.rom_bank = NonZeroU8::new((value & 0x1f).max(1)).unwrap(),
            // Take the 3 bottom bits as the bank set. These will be applied based on whether the
            // mode is ram mode or rom mode when used.
            0x4000..=0x5fff => self.bank_set = value & 0x3,
            // Change between basic and advanced banking mode.
            0x6000..=0x7fff => self.advanced_banking_mode = (value & 1) != 0,
            0x8000..=0x9fff => match self.ram_bank_mut() {
                Some(bank) => bank.write_byte_relative(addr, value),
                None => {}
            },
        })
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        // Writes to the control registers are applied as if all the bytes in the range
        // were written in order, so only the last byte in each control register range
        // counts.
        dispatch_memdev_bytes!(Mbc1Rom, addr, data, |addr, ref data| {
            0x0000..=0x1fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.ram_enable = (value & 0xF) == 0xA;
            },
            // Set the low-order bits of the rom-bank selection from the lower 5 bits of the
            // provided value. If 0 is provided, raise the value to 1.
            0x2000..=0x3fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.rom_bank = NonZeroU8::new((value & 0x1f).max(1)).unwrap();
            },
            // Take the 3 bottom bits as the bank set. These will be applied based on whether the
            // mode is ram mode or rom mode when used.
            0x4000..=0x5fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.bank_set = value & 0x3;
            },
            // Change between basic and advanced banking mode.
            0x6000..=0x7fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.advanced_banking_mode = (value & 1) != 0;
            },
            0x8000..=0x9fff => match self.ram_bank_mut() {
                Some(bank) => bank.write_bytes_relative(addr, data),
                None => {}
            },
        })
    }
}

impl SaveData for Mbc1Rom {
    fn write_save_data(&self, mut writer: impl Write) -> Result<(), io::Error> {
        for ram_bank in &self.ram_banks {
            match writer.write_all(ram_bank) {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn load_save_data(&mut self, mut reader: impl Read) -> Result<(), io::Error> {
        for ram_bank in &mut self.ram_banks {
            reader.read_exact(ram_bank)?
        }
        Ok(())
    }

    fn has_save_data(&self) -> bool {
        self.save_ram
    }
}

/// Variant 3 of the system ROMs.

/// Utility enum for MBC3 operation
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RamOrRtc<R, C> {
    Ram(R),
    Rtc(C),
    None,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mbc3Rom {
    /// Set of rom banks loaded from the cartridge.
    rom_banks: Vec<RomBank>,
    /// Set of ram banks on this Mbc3Rom, if any. If none, this will be an empty vector.
    ram_banks: Vec<RamBank>,
    /// RTC registers. Mapped as RAM banks 8-12
    rtc_regs: [u8; 5],

    /// Whether ram is saved when the device is powered off. (Does the ram have a battery?)
    save_ram: bool,

    // Registers:
    /// Whether ram is enabled for reading/writing. Otherwise writes are ignored and reads return
    /// dummy values.
    ram_enable: bool,
    /// Rom bank select. This is the low-order 5 bits (0..5) of the rom bank.
    rom_bank: u8,
    /// Bank set is a 2 bit register that either selects the ram-bank or the high-order 2 bits
    /// (5..7) of the rom bank, depending on the mode register. Note that because these two bits
    /// are shared between rom and ram, if mode is 0, only ram bank 0 is accessible, and if mode is
    /// 1, only rom banks 0..32 are accessible. Note also that the behavior depends on the relative
    /// size of ram and rom.
    bank_set: u8,

    /// Whether the RTC registers are latched. Latching stabilizes the registers for
    /// reading/writing while the clock advances in the background. The registers can be updated to
    /// the current time by unlatching and relatching them.
    rtc_latch: bool,
}

impl Mbc3Rom {
    /// Construct a new Mbc3Rom with the given rom banks and number of ram banks.
    fn new(rom_banks: Vec<RomBank>, num_ram_banks: usize, save_ram: bool) -> Self {
        assert!(rom_banks.len() >= 2, "Must have at least 2 rom banks.");
        assert!(
            rom_banks.len() <= 128,
            "MBC3 Rom can have at most 128 rom banks."
        );
        assert!(
            rom_banks.len().count_ones() == 1,
            "Number of rom banks must be a power of 2."
        );
        assert!(num_ram_banks <= 4, "MBC3 Rom can have at most 4 ram banks.");
        assert!(
            num_ram_banks == 0 || num_ram_banks.count_ones() == 1,
            "Number of ram banks must be a power of 2."
        );
        if rom_banks.len() > 32 && num_ram_banks > 1 {
            // It is unclear to me what happens if a cartridge has both > 32 rom banks and > 1 ram
            // bank.  This doc https://gbdev.io/pandocs/MBC3.html describes the situation with > 1
            // ram bank and > 32 rom banks, but not both. Perhaps there were no official cartridges
            // where that was the case.
            //
            // The implemenation I decided to go with was to just always apply the bank_set bits,
            // and then modulo by the number of banks of ram/rom. If only one of ram/rom is large
            // enough to require using bank_set, the behavior is definitely correct, but if both are
            // large enough to need bank_set, then both will be banked simultaneously, and I don't
            // know if that's right.
            warn!("MBC3 Rom is both Large Ram and Large Rom. Banking behavior may be wrong.");
        }
        Mbc3Rom {
            rom_banks,
            ram_banks: vec![[0u8; RAM_BANK_SIZE]; num_ram_banks],
            rtc_regs: [0u8; 5],
            save_ram,
            ram_enable: false,
            rom_bank: 0,
            bank_set: 0,
            rtc_latch: false,
        }
    }

    /// Whether ram is enabled for reading/writing. Otherwise writes are ignored and reads return
    /// dummy values.
    #[inline]
    pub fn ram_enable(&self) -> bool {
        self.ram_enable
    }

    /// Rom bank select. All 7 bits.
    #[inline]
    pub fn rom_bank(&self) -> u8 {
        self.rom_bank
    }

    /// Bank set is a 2 bit register that either selects the ram-bank or the high-order 2 bits
    /// (5..7) of the rom bank, depending on the mode register. Note that because these two bits
    /// are shared between rom and ram, if mode is 0, only ram bank 0 is accessible, and if mode is
    /// 1, only rom banks 0..32 are accessible. Note also that the behavior depends on the relative
    /// size of ram and rom.
    #[inline]
    pub fn bank_set(&self) -> u8 {
        self.bank_set
    }

    /// Whether the RTC register values are latched for stable reading/writing
    #[inline]
    pub fn rtc_latch(&self) -> bool {
        self.rtc_latch
    }

    /// Get the index of the lower rom bank currently selected.
    #[inline]
    pub fn selected_lower_bank(&self) -> usize {
        0
    }

    /// Get the index of the upper rom bank currently selected.
    #[inline]
    pub fn selected_upper_bank(&self) -> usize {
        (self.rom_bank as usize % self.rom_banks.len()).max(1)
    }

    /// Get the currently selected ram bank indes, regardless of whether ram is enabled.
    #[inline]
    pub fn selected_ram_bank(&self) -> usize {
        if self.ram_banks.is_empty() {
            0
        } else {
            self.bank_set as usize % self.ram_banks.len()
        }
    }

    /// Get a reference to the set of rom banks.
    pub fn rom_banks(&self) -> &[RomBank] {
        self.rom_banks.as_ref()
    }

    /// Get a reference to the set of rom banks.
    pub fn ram_banks(&self) -> &[RamBank] {
        self.ram_banks.as_ref()
    }

    /// Convenient access to the "fixed" lower rom bank. This bank only changes in Advanced rom
    /// mode.
    pub fn lower_bank(&self) -> &RomBank {
        &self.rom_banks[self.selected_lower_bank()]
    }

    /// Get the currently selected rom bank.
    pub fn upper_bank(&self) -> &RomBank {
        &self.rom_banks[self.selected_upper_bank()]
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    pub fn ram_rtc_bank(&self) -> RamOrRtc<&RamBank, &u8> {
        if !self.ram_enable {
            RamOrRtc::None
        } else {
            match self.bank_set {
                0x0..=0x3 => {
                    if self.ram_banks.is_empty() {
                        RamOrRtc::None
                    } else {
                        let bank = self.selected_ram_bank();
                        RamOrRtc::Ram(&self.ram_banks[bank])
                    }
                }
                0x8..=0xC => RamOrRtc::Rtc(&self.rtc_regs[self.bank_set as usize - 0x8]),
                _ => RamOrRtc::None,
            }
        }
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    fn ram_rtc_bank_mut(&mut self) -> RamOrRtc<&mut RamBank, &mut u8> {
        if !self.ram_enable {
            RamOrRtc::None
        } else {
            match self.bank_set {
                0x0..=0x3 => {
                    if self.ram_banks.is_empty() {
                        RamOrRtc::None
                    } else {
                        let bank = self.selected_ram_bank();
                        RamOrRtc::Ram(&mut self.ram_banks[bank])
                    }
                }
                0x8..=0xC => RamOrRtc::Rtc(&mut self.rtc_regs[self.bank_set as usize - 0x8]),
                _ => RamOrRtc::None,
            }
        }
    }
}

impl MemDevice for Mbc3Rom {
    const LEN: usize = CART_MEMDEV_LEN;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        dispatch_memdev_byte!(Mbc3Rom, addr, |addr| {
            0..=0x3fff => self.lower_bank().read_byte_relative(addr),
            0x4000..=0x7fff => self.upper_bank().read_byte_relative(addr),
            0x8000..=0x9fff => match self.ram_rtc_bank() {
                RamOrRtc::Ram(bank) => bank.read_byte_relative(addr),
                RamOrRtc::Rtc(reg) => *reg,
                RamOrRtc::None => 0xff,
            },
        })
    }

    fn read_bytes_relative(&self, addr: RelativeAddr, data: &mut [u8]) {
        dispatch_memdev_bytes!(Mbc3Rom, addr, data, |addr, mut data| {
            0..=0x3fff => self.lower_bank().read_bytes_relative(addr, data),
            0x4000..=0x7fff => self.upper_bank().read_bytes_relative(addr, data),
            0x8000..=0x9fff => match self.ram_rtc_bank() {
                RamOrRtc::Ram(bank) => bank.read_bytes_relative(addr, data),
                RamOrRtc::Rtc(reg) => data.fill(*reg),
                RamOrRtc::None => data.fill(0xff),
            },
        })
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, value: u8) {
        dispatch_memdev_byte!(Mbc3Rom, addr, |addr| {
            0x0000..=0x1fff => self.ram_enable = (value & 0xF) == 0xA,
            // Set the low-order bits of the rom-bank selection from the lower 5 bits of the
            // provided value. If 0 is provided, raise the value to 1.
            0x2000..=0x3fff => self.rom_bank = value & 0x7f,
            // Take the 3 bottom bits as the bank set. These will be applied based on whether the
            // mode is ram mode or rom mode when used.
            0x4000..=0x5fff => self.bank_set = value & 0x3,
            // Change between basic and advanced banking mode.
            0x6000..=0x7fff => self.rtc_latch = (value & 1) != 0,
            0x8000..=0x9fff => match self.ram_rtc_bank_mut() {
                RamOrRtc::Ram(bank) => bank.write_byte_relative(addr, value),
                RamOrRtc::Rtc(reg) => *reg = value,
                RamOrRtc::None => {}
            },
        })
    }

    fn write_bytes_relative(&mut self, addr: RelativeAddr, data: &[u8]) {
        dispatch_memdev_bytes!(Mbc3Rom, addr, data, |addr, ref data| {
            0x0000..=0x1fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.ram_enable = (value & 0xF) == 0xA
            },
            // Set the low-order bits of the rom-bank selection from the lower 5 bits of the
            // provided value. If 0 is provided, raise the value to 1.
            0x2000..=0x3fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.rom_bank = value & 0x7f
            },
            // Take the 3 bottom bits as the bank set. These will be applied based on whether the
            // mode is ram mode or rom mode when used.
            0x4000..=0x5fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.bank_set = value & 0x3
            },
            // Change between basic and advanced banking mode.
            0x6000..=0x7fff => {
                let value = data.last()
                    .expect("dispatch_range should never provide an empty range");
                self.rtc_latch = (value & 1) != 0
            },
            0x8000..=0x9fff => match self.ram_rtc_bank_mut() {
                RamOrRtc::Ram(bank) => bank.write_bytes_relative(addr, data),
                RamOrRtc::Rtc(reg) => {
                    let value = data.last()
                        .expect("dispatch_range should never provide an empty range");
                    *reg = *value
                },
                RamOrRtc::None => {}
            },
        })
    }
}

impl SaveData for Mbc3Rom {
    fn write_save_data(&self, mut writer: impl Write) -> Result<(), io::Error> {
        for ram_bank in &self.ram_banks {
            match writer.write_all(ram_bank) {
                Ok(_) => (),
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }

    fn load_save_data(&mut self, mut reader: impl Read) -> Result<(), io::Error> {
        for ram_bank in &mut self.ram_banks {
            reader.read_exact(ram_bank)?
        }
        Ok(())
    }

    fn has_save_data(&self) -> bool {
        self.save_ram
    }
}

#[cfg(test)]
mod tests {
    use std::mem;

    use super::*;

    use rand::distributions::Uniform;
    use rand::{Rng, SeedableRng};
    use rand_pcg::Pcg64Mcg;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn romonly_range_readwrite() {
        init();

        let mut rom_banks = Box::new([ReadOnly([0u8; ROM_BANK_SIZE]); 2]);
        let mut ram_bank = Box::new([0u8; RAM_BANK_SIZE]);

        // Pre-fill memory with unique values.
        let mut nextval = 0u32;
        for bank in rom_banks.iter_mut() {
            for chunk in bank.0.chunks_exact_mut(mem::size_of_val(&nextval)) {
                chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
                nextval += 1;
            }
        }
        for chunk in ram_bank.chunks_exact_mut(mem::size_of_val(&nextval)) {
            chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
            nextval += 1;
        }

        let mut cart_individual = RomOnly::new(rom_banks, Some(ram_bank), false);
        let mut cart_slice = cart_individual.clone();

        let mut input_buf = vec![];
        let mut output_buf_individual = vec![];
        let mut output_buf_slice = vec![];
        let mut rng =
            Pcg64Mcg::from_seed([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF]);

        let len_dist = Uniform::new_inclusive(1, 0x400);

        assert_eq!(cart_individual, cart_slice);

        for _ in 0..0x80000 {
            let len = rng.sample(&len_dist);

            let addr_dist = Uniform::new(0, CART_MEMDEV_LEN - len);
            let addr: RelativeAddr = (rng.sample(&addr_dist) as u16).into();
            input_buf.resize(len, 0u8);
            rng.fill(&mut input_buf[..]);

            for (i, &val) in input_buf.iter().enumerate() {
                cart_individual.write_byte_relative(addr.move_forward_by(i as u16), val);
            }
            cart_slice.write_bytes_relative(addr, &input_buf);

            output_buf_individual.resize(len, 0u8);
            for (i, res) in output_buf_individual.iter_mut().enumerate() {
                *res = cart_individual.read_byte_relative(addr.move_forward_by(i as u16));
            }
            output_buf_slice.resize(len, 0u8);
            cart_slice.read_bytes_relative(addr, &mut output_buf_slice);

            assert_eq!(output_buf_individual, output_buf_slice);
        }

        // To reduce the test runtime, only compare the final result.
        assert_eq!(cart_individual, cart_slice);
    }

    #[test]
    fn mbc1_range_readwrite() {
        init();

        let mut cart = Mbc1Rom::new(vec![ReadOnly([0u8; ROM_BANK_SIZE]); 128], 4, false);

        // Pre-fill memory with unique values.
        let mut nextval = 0u32;
        for bank in cart.rom_banks.iter_mut() {
            for chunk in bank.0.chunks_exact_mut(mem::size_of_val(&nextval)) {
                chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
                nextval += 1;
            }
        }
        for bank in cart.ram_banks.iter_mut() {
            for chunk in bank.chunks_exact_mut(mem::size_of_val(&nextval)) {
                chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
                nextval += 1;
            }
        }

        let mut cart_individual = cart.clone();
        let mut cart_slice = cart;

        let mut input_buf = vec![];
        let mut output_buf_individual = vec![];
        let mut output_buf_slice = vec![];
        let mut rng =
            Pcg64Mcg::from_seed([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF]);

        let len_dist = Uniform::new_inclusive(1, 0x400);

        assert_eq!(cart_individual, cart_slice);

        for _ in 0..0x80000 {
            let len = rng.sample(&len_dist);

            let addr_dist = Uniform::new(0, CART_MEMDEV_LEN - len);
            let addr: RelativeAddr = (rng.sample(&addr_dist) as u16).into();
            input_buf.resize(len, 0u8);
            rng.fill(&mut input_buf[..]);

            for (i, &val) in input_buf.iter().enumerate() {
                cart_individual.write_byte_relative(addr.move_forward_by(i as u16), val);
            }
            cart_slice.write_bytes_relative(addr, &input_buf);

            output_buf_individual.resize(len, 0u8);
            for (i, res) in output_buf_individual.iter_mut().enumerate() {
                *res = cart_individual.read_byte_relative(addr.move_forward_by(i as u16));
            }
            output_buf_slice.resize(len, 0u8);
            cart_slice.read_bytes_relative(addr, &mut output_buf_slice);

            assert_eq!(output_buf_individual, output_buf_slice);
        }

        // To reduce the test runtime, only compare the final result.
        assert_eq!(cart_individual, cart_slice);
    }

    #[test]
    fn mbc3_range_readwrite() {
        init();

        let mut cart = Mbc3Rom::new(vec![ReadOnly([0u8; ROM_BANK_SIZE]); 128], 4, false);

        // Pre-fill memory with unique values.
        let mut nextval = 0u32;
        for bank in cart.rom_banks.iter_mut() {
            for chunk in bank.0.chunks_exact_mut(mem::size_of_val(&nextval)) {
                chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
                nextval += 1;
            }
        }
        for bank in cart.ram_banks.iter_mut() {
            for chunk in bank.chunks_exact_mut(mem::size_of_val(&nextval)) {
                chunk.copy_from_slice(nextval.to_le_bytes().as_ref());
                nextval += 1;
            }
        }

        let mut cart_individual = cart.clone();
        let mut cart_slice = cart;

        let mut input_buf = vec![];
        let mut output_buf_individual = vec![];
        let mut output_buf_slice = vec![];
        let mut rng =
            Pcg64Mcg::from_seed([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0xA, 0xB, 0xC, 0xD, 0xE, 0xF]);

        let len_dist = Uniform::new_inclusive(1, 0x400);

        assert_eq!(cart_individual, cart_slice);

        for _ in 0..0x80000 {
            let len = rng.sample(&len_dist);

            let addr_dist = Uniform::new(0, CART_MEMDEV_LEN - len);
            let addr: RelativeAddr = (rng.sample(&addr_dist) as u16).into();
            input_buf.resize(len, 0u8);
            rng.fill(&mut input_buf[..]);

            for (i, &val) in input_buf.iter().enumerate() {
                cart_individual.write_byte_relative(addr.move_forward_by(i as u16), val);
            }
            cart_slice.write_bytes_relative(addr, &input_buf);

            output_buf_individual.resize(len, 0u8);
            for (i, res) in output_buf_individual.iter_mut().enumerate() {
                *res = cart_individual.read_byte_relative(addr.move_forward_by(i as u16));
            }
            output_buf_slice.resize(len, 0u8);
            cart_slice.read_bytes_relative(addr, &mut output_buf_slice);

            assert_eq!(output_buf_individual, output_buf_slice);
        }

        // To reduce the test runtime, only compare the final result.
        assert_eq!(cart_individual, cart_slice);
    }
}
