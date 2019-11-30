use std::io::Read;

pub trait MemDevice {
  fn read(&self, addr: u16) -> u8;
  fn write(&mut self, addr: u16, data: u8);
}

/*
 * ROMs
 */
pub struct NullRom;

impl MemDevice for NullRom {
  fn read(&self, _addr: u16) -> u8 {
    0xff
  }
  fn write(&mut self, _addr: u16, _value: u8) {
  }
}

pub struct BiosRom {
  rom: [u8;0x100],
}

impl BiosRom {
  fn new(source: &mut Read) -> BiosRom {
    let mut bios = BiosRom {rom: [0;0x100]};
    source.read(&mut bios.rom).unwrap();
    bios
  }
}

impl MemDevice for BiosRom {
  fn read(&self, addr: u16) -> u8 {
    if addr > 0xff {
      panic!("Illegal address to BIOS device {:x}.", addr)
    }
    self.rom[addr as usize]
  }
  fn write(&mut self, _addr: u16, _value: u8) {}
}

pub struct Mbc1Rom {
  rom_banks: [[u8;16384];128],
  ram_banks: [[u8;8192];4],
  ram_enable: bool,
  rom_bank: u8,
  bank_set: u8,
  ram_mode: bool,
}

impl MemDevice for Mbc1Rom {
  //type Output = u8;
  fn read(&self, addr: u16) -> u8 {
    0xff
  }
  fn write(&mut self, addr: u16, value: u8) {
    if addr < 0x2000 {
      self.ram_enable = value == 0x0A;
    }
    else if addr < 0x4000 {
      self.rom_bank = value & 0b1111;
    }
    else if addr < 0x6000 {
      // high 2 bits
      self.bank_set = (value & 0b11000000) >> 6;
    }
    else if addr < 0x8000 {
      self.ram_mode = (value & 0b1) == 1;
    }
    else if addr < 0xA000 && self.ram_enable {
      let ram_bank = if self.ram_mode {self.bank_set} else {0};
      self.ram_banks[ram_bank as usize][(addr - 0x8000) as usize] = value;
    }
  }
}

/*
 * RAM devices
 */
pub struct Ram8k {
  ram: [u8; 0x2000],
}
impl Ram8k {
  fn new() -> Ram8k {
    Ram8k {ram: [0u8; 0x2000]}
  }
}
impl MemDevice for Ram8k {
  fn read(&self, addr: u16) -> u8 {
    if addr > 0x1fff {
      panic!("Illegal 8k RAM address {:x}", addr);
    }
    self.ram[addr as usize]
  }
  fn write(&mut self, addr: u16, value: u8) {
    if addr > 0x1fff {
      panic!("Illegal 8k RAM address {:x}", addr);
    }
    self.ram[addr as usize] = value;
  }
}

pub struct Ram160 {
  ram: [u8; 0xa0],
}
impl Ram160 {
  fn new() -> Ram160 {
    Ram160 {ram: [0u8; 0xa0]}
  }
}
impl MemDevice for Ram160 {
  fn read(&self, addr: u16) -> u8 {
    if addr > 0x9f {
      panic!("Illegal 160 RAM address {:x}", addr);
    }
    self.ram[addr as usize]
  }
  fn write(&mut self, addr: u16, value: u8) {
    if addr > 0x9f {
      panic!("Illegal 160 RAM address {:x}", addr);
    }
    self.ram[addr as usize] = value;
  }
}

pub struct Ram127 {
  ram: [u8; 0x7f],
}
impl Ram127 {
  fn new() -> Ram127 {
    Ram127 {ram: [0u8; 0x7f]}
  }
}
impl MemDevice for Ram127 {
  fn read(&self, addr: u16) -> u8 {
    if addr > 0x7e {
      panic!("Illegal 127 RAM address {:x}", addr);
    }
    self.ram[addr as usize]
  }
  fn write(&mut self, addr: u16, value: u8) {
    if addr > 0x7e {
      panic!("Illegal 127 RAM address {:x}", addr);
    }
    self.ram[addr as usize] = value;
  }
}

/*
 * Memory-management unit
 */
pub struct GbMmu {
  rom: Box<MemDevice>,
  vram: Box<MemDevice>,
  wram: Box<MemDevice>,
  oam: Box<MemDevice>,
  zram: Box<MemDevice>,
  bios: Box<MemDevice>,
  nullrom: Box<MemDevice>,
  bios_enabled: bool,
}

impl GbMmu {
  pub fn new() -> GbMmu {
    GbMmu {
      rom: Box::new( NullRom {}),
      vram: Box::new( Ram8k::new()),
      wram: Box::new( Ram8k::new()),
      oam: Box::new( Ram160::new()),
      zram: Box::new( Ram127::new()),
      bios: Box::new( NullRom {}),
      nullrom: Box::new( NullRom {}),
      bios_enabled: true,
    }
  }
}

impl MemDevice for GbMmu {
  fn read(&self, addr: u16) -> u8 {
    let (ref mut device, addr) =
        match addr {
          0x0 ..= 0xff if self.bios_enabled => (&self.bios, addr),
          0x0 ..= 0x7fff => (&self.rom, addr),
          0x8000 ..= 0x9fff => (&self.vram, addr & 0x1fff),
          0xa000 ..= 0xbfff => (&self.rom, addr & 0x9fff),
          0xc000 ..= 0xfdff => (&self.wram, addr & 0x1fff),
          0xfe00 ..= 0xfe9f => (&self.oam, addr & 0xff),
          0xfea0 ..= 0xfeff => (&self.nullrom, addr & 0xff),
          0xff00 ..= 0xff7f => (&self.nullrom, addr & 0x7f),
          0xff80 ..= 0xfffe => (&self.zram, addr & 0x7f),
          0xffff => (&self.nullrom, 0x0)
        };

    device.read(addr)
  }

  fn write(&mut self, addr: u16, value: u8) {
    if addr == 0xff50 {
      if value == 0x1 {
        self.bios_enabled = false;
      }
      return;
    }
    let (ref mut device, addr) =
        match addr {
          0x0 ..= 0xff if self.bios_enabled => (&mut self.bios, addr),
          0x0 ..= 0x7fff => (&mut self.rom, addr),
          0x8000 ..= 0x9fff => (&mut self.vram, addr & 0x1fff),
          0xa000 ..= 0xbfff => (&mut self.rom, addr & 0x9fff),
          0xc000 ..= 0xfdff => (&mut self.wram, addr & 0x1fff),
          0xfe00 ..= 0xfe9f => (&mut self.oam, addr & 0xff),
          0xfea0 ..= 0xfeff => (&mut self.nullrom, addr & 0xff),
          0xff00 ..= 0xff7f => (&mut self.nullrom, addr & 0x7f),
          0xff80 ..= 0xfffe => (&mut self.zram, addr & 0x7f),
          0xffff => (&mut self.nullrom, 0x0)
        };

    device.write(addr, value);
  }
}
