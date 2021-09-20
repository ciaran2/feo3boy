use crate::memdev::MemDevice;

// serial interrupt at bit 3
const INTERRUPT_MASK: u8 = 0b1000;
const SLOW_TPERIOD: u16 = 512;
const FAST_TPERIOD: u16 = 16;

struct SerialContext {
    t_period: u16,
    t_progress: u16,
    bit_progress: u8,
    remote_byte: u8,
}

pub fn tick(serial_ctx: &mut SerialContext, mappedIO: &mut MemDevice, tcycles: u64) {
    if mappedIO[0x02] == 0x81 {
        serial_ctx.t_progress += tcycles;

        //check if we've completed a serial tick
        if serial_ctx.t_progress > SLOW_TPERIOD {
            serial_ctx.t_progress -= SLOW_TPERIOD;
            serial_ctx.bit_progress += 1;

            //rotate local and remote serial bytes about each other
            let local_high_bit = (mappedIO[0x01] & 0x80) >> 7;
            let remote_high_bit = (serial_ctx.remote_byte & 0x80) >> 7;
            mappedIO[0x01] = (mappedIO[0x01] << 1) | remote_high_bit;
            serial_ctx.remote_byte = (serial_ctx.remote_byte << 1) | local_high_bit;

            if serial_ctx.bit_progress == 8 {
                mappedIO[0x02] = 0x01;
                mappedIO[0x0f] |= INTERRUPT_MASK;
                serial_ctx.bit_progress = 0;
            }
        }
    }
}
