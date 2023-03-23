use std::error::Error;
use std::collections::{VecDeque};
use bitflags::bitflags;
use crate::memdev::{Addr, MemDevice, IoRegs, IoRegsContext};
use log::{debug, trace, info};

bitflags! {
    #[derive(Default)]
    pub struct SoundEnable : u8 {
        const ALL = 0b10000000;
        const CH4 = 0b00001000;
        const CH3 = 0b00000100;
        const CH2 = 0b00000010;
        const CH1 = 0b00000001;
        const WRITEABLE = 0b10000000;
    }
}


bitflags! {
    #[derive(Default)]
    pub struct SoundPan : u8 {
        const CH4_LEFT  = 0b10000000;
        const CH3_LEFT  = 0b01000000;
        const CH2_LEFT  = 0b00100000;
        const CH1_LEFT  = 0b00010000;
        const CH4_RIGHT = 0b00001000;
        const CH3_RIGHT = 0b00000100;
        const CH2_RIGHT = 0b00000010;
        const CH1_RIGHT = 0b00000001;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct SoundVolume : u8 {
        const VIN_LEFT  = 0b10000000;
        const VOL_LEFT  = 0b01110000;
        const VIN_RIGHT = 0b00001000;
        const VOL_RIGHT = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct PulseSweep : u8 {
        const PACE       = 0b01110000;
        const SLOPE_DIR  = 0b00001000;
        const SLOPE_CTRL = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct PulseTimer : u8 {
        const DUTY_CYCLE = 0b11000000;
        const INIT_TIMER = 0b00111111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct Envelope : u8 {
        const INIT_VOL  = 0b11110000;
        const DIRECTION = 0b00001000;
        const PACE      = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct WavetableLevel : u8 {
        const LEVEL = 0b01100000;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct NoiseControl : u8 {
        const CLOCK_SHIFT = 0b11110000;
        const LSFR_WIDTH  = 0b00001000;
        const CLOCK_DIV   = 0b00000111;
    }
}

bitflags! {
    #[derive(Default)]
    pub struct ChannelControl : u8 {
        const TRIGGER         = 0b10000000;
        const LENGTH_ENABLE   = 0b01000000;
        const WAVELENGTH_HIGH = 0b00000111;
        const READABLE        = 0b01000000;
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ApuState {
    sample_rate: u32,
}

impl ApuState {
    pub fn new() -> Self {
        ApuState {
            sample_rate: 0,
        }
    }
}

pub trait ApuContext: IoRegsContext {
    fn apu(&self) -> &ApuState;
    fn apu_mut(&mut self) -> &mut ApuState;
}

pub fn tick(ctx: &mut impl ApuContext, tcycles: u64) {
}
