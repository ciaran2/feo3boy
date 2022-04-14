//! Implementation of different cartridge types.

use std::convert::TryFrom;
use std::io::{self, ErrorKind, Read};
use std::slice;
use std::num::NonZeroU8;

use log::warn;
use thiserror::Error;

use super::{Addr, MemDevice, NullRom, ReadOnly};

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

/// Enum of different cartridge types.
///
/// Note that in the GB, the cartridges occupy two memory spaces, one before GPU ram for the ROM
/// portion, and one after for the RAM. The `Cartridge` tye and all of the rom implementation types
/// ignore this split, and map the ram portion directly after the rom. It is the responsibility of
/// the caller to remap the memory spaces as needed to insert the GPU ram.
#[derive(Clone, Debug)]
pub enum Cartridge {
    /// No cartridge. All reads return 0 and all writes are ignored.
    None,
    /// A basic [`RomOnly`] cartridge.
    RomOnly(RomOnly),
    /// An [`Mbc1Rom`] cartridge.
    Mbc1(Mbc1Rom),
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
                    (0, Ok(0)) | (8 | 9, Ok(2)) => {}
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
                    (2 | 3, Ok(size @ (8 | 32))) => size,
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
            code @ (5..=6 | 0xb..=0xd | 0xf..=0x13 | 0x19..=0x1e | 0x20 | 0x22 | 0xfc..=0xff) => {
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
    fn read(&self, addr: Addr) -> u8 {
        match self {
            Cartridge::None => NullRom::<0xA000>.read(addr),
            Cartridge::RomOnly(ref cart) => cart.read(addr),
            Cartridge::Mbc1(ref cart) => cart.read(addr),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        match self {
            Cartridge::None => NullRom::<0xA000>.write(addr, value),
            Cartridge::RomOnly(ref mut cart) => cart.write(addr, value),
            Cartridge::Mbc1(ref mut cart) => cart.write(addr, value),
        }
    }
}

/// Rom banks are 0x4000 = 16 KiB.
const ROM_BANK_SIZE: usize = 0x4000;

/// A single 16 KiB rom bank within a cartridge.
pub type RomBank = ReadOnly<[u8; ROM_BANK_SIZE]>;

/// Ram banks are 0x2000 = 8 KiB.
const RAM_BANK_SIZE: usize = 0x4000;

/// A single 8 KiB ram bank within a cartridge.
pub type RamBank = [u8; RAM_BANK_SIZE];

/// Cartridge which has only 2 rom banks and optionally up to 1 ram bank.
#[derive(Clone, Debug)]
pub struct RomOnly {
    /// Rom banks. Both are always accessible.
    rom_banks: Box<[RomBank; 2]>,
    /// Ram bank, may or may not be included.
    ram_bank: Option<Box<RamBank>>,
    /// Whether ram is saved when the device is powered off. (Does the ram have a battery?)
    save_ram: bool,
}

impl RomOnly {
    /// Constructs a new `RomOnly` with empty (all 0) rom banks and no ram bank.
    fn empty() -> Self {
        Self {
            rom_banks: Box::new([ReadOnly([0u8; ROM_BANK_SIZE]); 2]),
            ram_bank: None,
            save_ram: false,
        }
    }
}

impl MemDevice for RomOnly {
    fn read(&self, addr: Addr) -> u8 {
        match addr.relative() {
            0..=0x3fff => self.rom_banks[0].read(addr),
            0x4000..=0x7fff => self.rom_banks[1].read(addr.offset_by(0x4000)),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref ram) => ram.read(addr.offset_by(0x8000)),
                None => 0,
            },
            _ => panic!("Address {} out of range for Mbc1Rom", addr),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        match addr.relative() {
            0..=0x3fff => self.rom_banks[0].write(addr, value),
            0x4000..=0x7fff => self.rom_banks[1].write(addr.offset_by(0x4000), value),
            0x8000..=0x9fff => match self.ram_bank {
                Some(ref mut ram) => ram.write(addr.offset_by(0x8000), value),
                None => {}
            },
            _ => panic!("Address {} out of range for Mbc1Rom", addr),
        }
    }
}

/// Variant 1 of the system ROMs.

#[derive(Clone, Debug)]
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

    /// Convenient access to the "fixed" lower rom bank. This bank only changes in Advanced rom
    /// mode.
    fn lower_bank(&self) -> &RomBank {
        if self.advanced_banking_mode {
            let bank = (self.bank_set as usize * 32) % self.rom_banks.len();
            &self.rom_banks[bank]
        } else {
            &self.rom_banks[0]
        }
    }

    /// Get the currently selected rom bank. This will never be bank 0, 32, 64, or 96.
    fn upper_bank(&self) -> &RomBank {
        let low_order = self.rom_bank.get();
        let high_order = self.bank_set << 5;
        let rom = (low_order | high_order) as usize % self.rom_banks.len();
        &self.rom_banks[rom]
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    fn ram_bank(&self) -> Option<&RamBank> {
        if self.ram_banks.is_empty() || !self.ram_enable {
            None
        } else if self.advanced_banking_mode {
            let bank = self.bank_set as usize % self.ram_banks.len();
            Some(&self.ram_banks[bank])
        } else {
            Some(&self.ram_banks[0])
        }
    }

    /// Gets the currently selected ram bank, if the rom has ram and ram is enabled.
    fn ram_bank_mut(&mut self) -> Option<&mut RamBank> {
        if self.ram_banks.is_empty() || !self.ram_enable {
            None
        } else if self.advanced_banking_mode {
            let bank = self.bank_set as usize % self.ram_banks.len();
            Some(&mut self.ram_banks[bank])
        } else {
            Some(&mut self.ram_banks[0])
        }
    }
}

impl MemDevice for Mbc1Rom {
    fn read(&self, addr: Addr) -> u8 {
        match addr.relative() {
            0..=0x3fff => self.lower_bank().read(addr),
            0x4000..=0x7fff => self.upper_bank().read(addr.offset_by(0x4000)),
            0x8000..=0x9fff => match self.ram_bank() {
                Some(bank) => bank.read(addr.offset_by(0x8000)),
                None => 0,
            },
            _ => panic!("Address {} out of range for Mbc1Rom", addr),
        }
    }

    fn write(&mut self, addr: Addr, value: u8) {
        match addr.relative() {
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
                Some(bank) => bank.write(addr.offset_by(0x8000), value),
                None => {}
            },
            _ => panic!("Address {} out of range for Mbc1Rom", addr),
        }
    }
}
