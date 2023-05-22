use bitflags::bitflags;

use crate::memdev::MemDevice;

bitflags! {
    /// Available set of interrupt flags.
    #[derive(Default, Debug, Copy, Clone, Eq, PartialEq, MemDevice)]
    #[memdev(bitflags)]
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
    /// Gets the handler address for this particular interrupt. Panics if more than one
    /// flag is set.
    pub fn handler_addr(self) -> u16 {
        const FIRST_INTERRUPT: u16 = 0x40;
        const INTERRUPT_GAP: u16 = 0x08;

        assert!(
            self.bits().count_ones() == 1,
            "mut have exactly on interrupt handler set"
        );
        FIRST_INTERRUPT + self.bits().trailing_zeros() as u16 * INTERRUPT_GAP
    }
}

/// The Interrupt Enable (IE) register. Implements MemDevice.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, MemDevice)]
#[memdev(passthrough)]
pub struct InterruptEnable(pub InterruptFlags);

/// Context trait for accessing interrupts.
pub trait InterruptContext {
    /// The type which provides access to the interrupts.
    type Interrupts: Interrupts;

    /// Provides read access to the interrupt flags.
    fn interrupts(&self) -> &Self::Interrupts;

    /// Provides write-access to the interrupt flags.
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts;
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

    /// Clears the specified interrupts by removing them from the active interrupt flags.
    #[inline]
    fn clear(&mut self, flags: InterruptFlags) {
        self.set_queued(self.queued() - flags);
    }

    /// Get the enabled interrupt flags.
    fn enabled(&self) -> InterruptFlags;

    /// Set the enabled interrupt flags for the context.
    fn set_enabled(&mut self, flags: InterruptFlags);

    /// Gets the set of active interrupts, that is those which are enabled and in the
    /// interrupt vector.
    #[inline]
    fn active(&self) -> InterruptFlags {
        self.enabled() & self.queued()
    }
}
