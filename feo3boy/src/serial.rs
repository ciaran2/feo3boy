use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};

use log::{debug, trace};
use std::collections::VecDeque;

/// Stream of bytes being sent to/from the Gameboy.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct SerialStream {
    /// Bytes to be sent to the gameboy. When serial transfer is requested, one byte will
    /// be popped off this input and moved to the `remote_byte` slot. If no data is
    /// availabe, `remote_byte` will be set to zero.
    to_gb_bytes: VecDeque<u8>,
    /// Bytes read from the gameboy. When a serial transfer completes transfering a byte,
    /// that byte will be pushed to this queue.
    from_gb_bytes: VecDeque<u8>,
}

impl SerialStream {
    /// Send a byte to the gameboy.
    pub fn send_byte(&mut self, byte: u8) {
        self.to_gb_bytes.push_back(byte);
    }

    /// Send a slice of bytes to the gameboy.
    pub fn send_slice(&mut self, bytes: &[u8]) {
        self.to_gb_bytes.extend(bytes);
    }

    /// Send all the bytes in the given iterator to the gameboy.
    pub fn send_from<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = u8>,
    {
        self.to_gb_bytes.extend(iter);
    }

    /// Receive a byte from the gameboy, if one was sent.
    pub fn receive_byte(&mut self) -> Option<u8> {
        self.from_gb_bytes.pop_front()
    }

    /// Receive all pending bytes from the gameboy, if any were sent.
    pub fn receive_bytes(&mut self) -> impl '_ + Iterator<Item = u8> + ExactSizeIterator {
        self.from_gb_bytes.drain(..)
    }

    /// Get the next pending byte to send to the gameboy, or 0 if no byte is pending.
    fn pop_next_to_gb(&mut self) -> u8 {
        self.to_gb_bytes.pop_front().unwrap_or_default()
    }

    /// Set a byte from the gameboy.
    fn set_from_gb(&mut self, byte: u8) {
        self.from_gb_bytes.push_back(byte);
    }
}

/// Context trait providing access to fields needed to service Serial.
pub trait SerialContext: InterruptContext {
    /// Get the serial state.
    fn serial(&self) -> &SerialState;

    /// Get mutable access to the serial state.
    fn serial_mut(&mut self) -> &mut SerialState;

    fn serial_regs(&self) -> &SerialRegs;

    fn serial_regs_mut(&mut self) -> &mut SerialRegs;
}

const SLOW_TPERIOD: u64 = 512;
#[allow(unused)]
const FAST_TPERIOD: u64 = 16;

/// Memory-mapped IO registers used by Serial connections.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SerialRegs {
    pub serial_data: u8,
    pub serial_control: u8,
}

memdev_fields!(SerialRegs, len: 2, {
    0x00 => serial_data,
    0x01 => serial_control,
});

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SerialState {
    t_progress: u64,
    bit_progress: u8,
    /// TODO: This isn't sufficient to link two emulators together, but supporting two
    /// linked devices will take further refactoring.
    remote_byte: u8,
    pub stream: SerialStream,
}

impl SerialState {
    /// Create a new SerialContext.
    pub fn new() -> SerialState {
        Default::default()
    }
}

pub fn tick(ctx: &mut impl SerialContext, tcycles: u64) {
    trace!(
        "Serial tick. Serial control field: {:#6x}, Serial data:{:#6x}",
        ctx.serial_regs().serial_control,
        ctx.serial_regs().serial_data,
    );
    if ctx.serial_regs().serial_control == 0x81 {
        debug!(
            "Serial transfer active. Progress: {} cycles",
            ctx.serial().t_progress
        );

        if ctx.serial().bit_progress == 0 {
            let serial = ctx.serial_mut();
            serial.remote_byte = serial.stream.pop_next_to_gb();
        }
        ctx.serial_mut().t_progress += tcycles;
        //check if we've completed a serial tick
        while ctx.serial().t_progress > SLOW_TPERIOD {
            ctx.serial_mut().t_progress -= SLOW_TPERIOD;
            ctx.serial_mut().bit_progress += 1;

            // rotate local and remote serial bytes about each other
            let [new_local_byte, new_remote_byte] =
                u16::from_ne_bytes([ctx.serial_regs().serial_data, ctx.serial().remote_byte])
                    .rotate_left(1)
                    .to_ne_bytes();

            ctx.serial_regs_mut().serial_data = new_local_byte;
            ctx.serial_mut().remote_byte = new_remote_byte;

            if ctx.serial().bit_progress == 8 {
                ctx.serial_regs_mut().serial_control = 0x01; //clear transfer start flag
                ctx.interrupts_mut().send(InterruptFlags::SERIAL);

                let serial = ctx.serial_mut();
                serial.bit_progress = 0;
                serial.stream.set_from_gb(serial.remote_byte);
            }
        }
    }
}
