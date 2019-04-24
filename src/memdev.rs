mod feo3boy::memdev {

  pub trait MemDevice {
    fn read(&self, addr: u16) -> u8;
    fn write(&self, addr: u16, data: u8);
  }

}
