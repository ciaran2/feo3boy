use crate::memdev::MemDevice;

use log::{debug, trace};
use std::io;
use std::io::Write;

// serial interrupt at bit 3
const INTERRUPT_MASK: u8 = 0b1000;
const SLOW_TPERIOD: u64 = 512;
const FAST_TPERIOD: u64 = 16;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct SerialContext {
    t_progress: u64,
    bit_progress: u8,
    remote_byte: u8,
}

impl SerialContext {
    /// Create a new SerialContext.
    pub fn new() -> SerialContext {
        Default::default()
    }
}

pub fn tick(serial_ctx: &mut SerialContext, mappedIO: &mut MemDevice, tcycles: u64) {
    trace!(
        "Serial tick. Serial control field: {:#6x}, Serial data:{:#6x}",
        mappedIO.read(0x02.into()),
        mappedIO.read(0x01.into())
    );
    if mappedIO.read(0x02.into()) == 0x81 {
        debug!(
            "Serial transfer active. Progress: {} cycles",
            serial_ctx.t_progress
        );
        serial_ctx.t_progress += tcycles;

        //check if we've completed a serial tick
        if serial_ctx.t_progress > SLOW_TPERIOD {
            serial_ctx.t_progress -= SLOW_TPERIOD;
            serial_ctx.bit_progress += 1;

            //rotate local and remote serial bytes about each other
            let local_high_bit = (mappedIO.read(0x01.into()) & 0x80) >> 7;
            let remote_high_bit = (serial_ctx.remote_byte & 0x80) >> 7;
            mappedIO.write(
                0x01.into(),
                (mappedIO.read(0x01.into()) << 1) | remote_high_bit,
            );
            serial_ctx.remote_byte = (serial_ctx.remote_byte << 1) | local_high_bit;

            if serial_ctx.bit_progress == 8 {
                mappedIO.write(0x02.into(), 0x01); //clear transfer start flag
                mappedIO.write(0x0f.into(), mappedIO.read(0x0f.into()) | INTERRUPT_MASK);
                serial_ctx.bit_progress = 0;

                //abstract this into something more modular later
                print!("{}", serial_ctx.remote_byte as char);
                io::stdout().flush().unwrap();
            }
        }
    }
}
