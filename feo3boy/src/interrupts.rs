use bitflags::bitflags;

use crate::memdev::{Addr, MemDevice};

bitflags! {
    /// Available set of interrupt flags.
    #[derive(Default)]
    pub struct InterruptFlags: u8 {
        /// Vertical blanking of the display.
        const VBLANK = 0b00001;

        /// Stat interrupt, has a couple different triggers from PPU mode changes I think?
        const STAT = 0b00010;

        /// Triggered when the timer counter register overflows.
        const TIMER = 0b00100;

        /// Triggered after each byte transferred over the serial port.
        const SERIAL = 0b01000;

        /// Triggered when a button is *pressed* but not released (and depends on which buttons are
        /// currently enabled in I/O registers).
        const JOYPAD = 0b10000;
    }
}

impl InterruptFlags {
    /// Reads the byte at 0xffff and converts it to `InterruptFlags`. Normally 0xffff is the
    /// location of the interrupt enable register.
    pub fn get_interrupt_enable(mem: &impl MemDevice) -> Self {
        Self::from_bits_truncate(mem.read(0xffff.into()))
    }

    /// Sets the byte at 0xffff to these `InterruptFlags`. Normally 0xffff is the location of the
    /// interrupt enable register.
    pub fn set_interrupt_enable(self, mem: &mut impl MemDevice) {
        mem.write(0xffff.into(), self.bits);
    }
}

/// The Interrupt Enable (IE) register. Implements MemDevice.
#[derive(Copy, Clone, Debug, Default)]
pub struct InterruptEnable(pub InterruptFlags);

impl MemDevice for InterruptEnable {
    fn read(&self, addr: Addr) -> u8 {
        assert!(
            addr.relative() == 0,
            "Address {}  out of range for Interrupt Enable Register",
            addr
        );
        self.0.bits
    }

    fn write(&mut self, addr: Addr, value: u8) {
        assert!(
            addr.relative() == 0,
            "Address {}  out of range for Interrupt Enable Register",
            addr
        );
        self.0 = InterruptFlags::from_bits_truncate(value);
    }
}
