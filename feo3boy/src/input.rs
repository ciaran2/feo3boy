use bitflags::bitflags;

use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};

bitflags! {
    /// Lcd control status flags
    #[derive(Default)]
    pub struct ButtonStates: u8 {
        const A          = 0b00000001;
        const B          = 0b00000010;
        const SELECT     = 0b00000100;
        const START      = 0b00001000;
        const RIGHT      = 0b00010000;
        const LEFT       = 0b00100000;
        const UP         = 0b01000000;
        const DOWN       = 0b10000000;
        const ACTIONS    = 0b00001111;
        const DIRECTIONS = 0b11110000;
    }
}

impl ButtonStates {
    /// Get an inverted bitfield nybble for the selected buttons
    pub fn select_buttons(&self, no_directions: bool, no_actions: bool) -> u8 {
        (if no_directions {
            0xf
        } else {
            !(self.bits() >> 4)
        }) & (if no_actions {
            0xf
        } else {
            !(*self & ButtonStates::ACTIONS).bits()
        })
    }
}

bitflags! {
    /// Register for control and status of buttons
    /// Counterintuitively 0 is true because the buttons pull the circuit to ground
    #[derive(Default)]
    pub struct ButtonRegister: u8 {
        const NO_ACTIONS       = 0b100000;
        const NO_DIRECTIONS    = 0b010000;
        const BUTTON_SELECTORS = 0b110000;
        const DOWN_START       = 0b001000;
        const UP_SELECT        = 0b000100;
        const LEFT_B           = 0b000010;
        const RIGHT_A          = 0b000001;
        const BUTTONS          = 0b001111;
    }
}

impl ButtonRegister {
    pub fn set_writable(&self, data: u8) -> ButtonRegister {
        (*self & ButtonRegister::BUTTONS)
            | (ButtonRegister::from_bits_truncate(data) & ButtonRegister::BUTTON_SELECTORS)
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait InputContext: IoRegsContext + InterruptContext {
    /// get the button state
    fn button_states(&self) -> ButtonStates;

    /// set the button state
    fn set_button_states(&mut self, button_states: ButtonStates);
}

/// "update" rather than "tick" here because the input is not on a clock like the
/// other components and can theoretically be updated at any time
pub fn update(ctx: &mut impl InputContext) {
    let button_reg = ctx.ioregs().buttons();

    let no_directions = button_reg.contains(ButtonRegister::NO_DIRECTIONS);
    let no_actions = button_reg.contains(ButtonRegister::NO_ACTIONS);

    let old_button_status = button_reg & ButtonRegister::BUTTONS;
    let new_button_status = ButtonRegister::from_bits_truncate(
        ctx.button_states()
            .select_buttons(no_directions, no_actions),
    );

    // trigger interrupt on falling edge
    if !(!new_button_status & old_button_status).is_empty() {
        ctx.interrupts_mut().send(InterruptFlags::JOYPAD);
    }

    ctx.ioregs_mut()
        .set_buttons((button_reg & ButtonRegister::BUTTON_SELECTORS) | new_button_status);
}
