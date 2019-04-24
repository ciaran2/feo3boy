use feo3boy::memdev;

mod feo3boy:rom {
  struct Mbc1Rom {
    rom_banks: [[u8;16384];128],
    ram_banks: [[u8;8192];4],
    ram_enable: bool,
    rom_bank: u8,
    bank_set: u8,
    ram_mode: bool,
  }
  
  impl feo3boy::MemDevice for Mbc1Rom {
    type Output = u8;
    fn read(&self, addr: u16) -> u8 {
      0
    }
    fn write(&self, addr: u16, value: u8) {
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
        self.ram_mode = value & 0b1;
      }
      else if addr < 0xA000 && ram_enable {
        ram_bank = if ram_mode {bank_set} else {0}
        self.ram_banks[ram_bank][addr - 0x8000] = value;
      }
    }
  }
}
