use std::error::Error;
use bitflags::bitflags;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};
use log::{debug, trace, info};


bitflags! {
    /// Lcd control status flags
    #[derive(Default)]
    pub struct TimerControl: u8 {
        const TIMER_PERIOD = 0b0000011;
        const TIMER_ENABLE = 0b0000100;
    }
}

impl TimerControl {

    pub fn get_period(&self) -> u64 {
        match (*self & TimerControl::TIMER_PERIOD).bits() {
            0 => 1024,
            1 => 16,
            2 => 64,
            3 => 256,
            _ => panic!("Illegal timer period number encountered. This should be impossible.")
        }
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait TimerContext: IoRegsContext + InterruptContext {
    /// Get the timer state.
    fn timer(&self) -> &TimerState;

    /// Get mutable access to the timer state.
    fn timer_mut(&mut self) -> &mut TimerState;
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TimerState {
    timer_ticks: u64,
    overflow: bool,
}

impl TimerState {
    pub fn new() -> TimerState {
        Default::default()
    }
}

pub fn tick(ctx: &mut impl TimerContext, tcycles: u64) {
}
