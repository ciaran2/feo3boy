use std::mem;

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

/// Trait for accessing the (usually memory-mapped) registers for InterruptFlags and
/// InterruptEnable.
pub trait Interrupts {
    /// Get the pending interrupt flags from the context.
    fn queued(&self) -> InterruptFlags;

    /// Set the pending flags for the context.
    fn set_queued(&mut self, flags: InterruptFlags);

    /// Sends the specified interrupts by adding them to the active interrupt flags.
    #[inline]
    fn send(&mut self, flags: InterruptFlags) {
        self.set_queued(self.queued() | flags);
    }

    /// Get the enabled interrupt flags.
    fn enabled(&self) -> InterruptFlags;

    /// Set the enabled interrupt flags for the context.
    fn set_enabled(&mut self, flags: InterruptFlags);
}

/// Wraps a mem-device, providing access to memory-mapped interrupt registers by reading
/// and writing memory. This type assumes the interrupt vector is memory mapped at 0xff0f,
/// and the interrupt enable register is memory mapped at 0xffff.
#[repr(transparent)]
pub struct MemInterrupts<M>(M);

impl<M: MemDevice> MemInterrupts<M> {
    /// Convert a reference to a memory device into a mem-interrupts accessor.
    #[inline]
    pub fn wrap<'a>(device: &'a M) -> &'a MemInterrupts<M> {
        // This is safe because MemInterrupts is repr(transparent) so the layout of
        // MemInterupts<M> is the same as M and because the returned reference has the
        // same lifetime as the passed reference, so memory safety rules for M are upheld.
        unsafe { mem::transmute(device) }
    }

    /// Convert a mutable reference to a memory device into a mem-interrupts accessor.
    #[inline]
    pub fn wrap_mut<'a>(device: &'a mut M) -> &'a mut MemInterrupts<M> {
        // This is safe because MemInterrupts is repr(transparent) so the layout of
        // MemInterupts<M> is the same as M and because the returned reference has the
        // same lifetime as the passed reference, so memory safety rules for M are upheld.
        unsafe { mem::transmute(device) }
    }
}

impl<M: MemDevice> Interrupts for MemInterrupts<M> {
    fn queued(&self) -> InterruptFlags {
        InterruptFlags::from_bits_truncate(self.0.read(0xff0f.into()))
    }

    fn set_queued(&mut self, flags: InterruptFlags) {
        self.0.write(0xff0f.into(), flags.bits);
    }

    fn enabled(&self) -> InterruptFlags {
        InterruptFlags::from_bits_truncate(self.0.read(0xffff.into()))
    }

    fn set_enabled(&mut self, flags: InterruptFlags) {
        self.0.write(0xffff.into(), flags.bits);
    }
}
