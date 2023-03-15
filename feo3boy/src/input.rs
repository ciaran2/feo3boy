use std::error::Error;
use bitflags::bitflags;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};
use log::{debug, trace, info};


bitflags! {
    /// Lcd control status flags
    #[derive(Default)]
    pub struct ButtonStates: u8 {
        const START      = 0b00000001;
        const SELECT     = 0b00000010;
        const B          = 0b00000100;
        const A          = 0b00001000;
        const DOWN       = 0b00010000;
        const UP         = 0b00100000;
        const LEFT       = 0b01000000;
        const RIGHT      = 0b10000000;
        const ACTIONS     = 0b00001111;
        const DIRECTIONS = 0b11110000;
    }
}

impl ButtonStates {

    pub fn select_buttons (&self, directions: bool, actions: bool) -> u8 {
        (if directions { (*self & ButtonStates::DIRECTIONS).bits() >> 4 } else { 0 as u8 })
         | (if actions { (*self & ButtonStates::ACTIONS).bits() } else { 0 as u8 })
    }
}

bitflags! {
    /// Register for control and status of buttons
    /// Counterintuitively 0 is true because the buttons pull the circuit to ground
    #[derive(Default)]
    pub struct ButtonRegister: u8 {
        const SELECT_ACTIONS = 0b100000;
        const SELECT_DIRS    = 0b010000;
        const DOWN_START     = 0b001000;
        const UP_SELECT      = 0b000100;
        const LEFT_B         = 0b000010;
        const RIGHT_A        = 0b000001;
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait InputContext: IoRegsContext + InterruptContext {
    /// get the button state
    fn button_states(&self) -> ButtonStates;

    /// set the button state
    fn set_button_states(&mut self, button_states: ButtonStates);
}

pub fn update(ctx: &mut impl InputContext) {
    let button_reg = ctx.ioregs().buttons();

    ctx.ioregs_mut().set_buttons(button_reg);
}
