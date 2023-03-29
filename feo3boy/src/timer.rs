use crate::apu::{self, ApuContext};
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{IoRegs, IoRegsContext};
use bitflags::bitflags;

bitflags! {
    /// Lcd control status flags
    #[derive(Default)]
    pub struct TimerControl: u8 {
        const TIMER_PERIOD = 0b0000011;
        const TIMER_ENABLE = 0b0000100;
    }
}

impl TimerControl {
    pub fn get_period(&self) -> u16 {
        match (*self & TimerControl::TIMER_PERIOD).bits() {
            0 => 1024,
            1 => 16,
            2 => 64,
            3 => 256,
            _ => panic!("Illegal timer period number encountered. This should be impossible."),
        }
    }
}

/// Context trait providing access to fields needed to service graphics.
pub trait TimerContext: IoRegsContext + InterruptContext + ApuContext {
    /// Get the timer state.
    fn timer(&self) -> &TimerState;

    /// Get mutable access to the timer state.
    fn timer_mut(&mut self) -> &mut TimerState;
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TimerState {
    timer_ticks: u64,
    old_divider: u16,
    interrupt_queued: bool,
    interrupt_delay: u64,
}

impl TimerState {
    pub fn new() -> TimerState {
        Default::default()
    }

    fn queue_interrupt(&mut self) {
        self.interrupt_queued = true;
        self.interrupt_delay = 4;
    }
}

fn increment_timer(ctx: &mut impl TimerContext) {
    let (timer, overflow) = ctx.ioregs().timer().overflowing_add(1);

    if overflow {
        ctx.timer_mut().queue_interrupt();
    }

    ctx.ioregs_mut().set_timer(timer)
}

fn check_interrupt(ctx: &mut impl TimerContext, tcycles: u64) {
    // an intervening write to the timer will discard the interrupt
    if ctx.timer().interrupt_queued && ctx.ioregs().timer() == 0 {
        if tcycles >= ctx.timer().interrupt_delay as u64 {
            ctx.timer_mut().interrupt_queued = false;
            let timer_mod = ctx.ioregs_mut().timer_mod();
            ctx.ioregs_mut().set_timer(timer_mod);
            ctx.interrupts_mut().send(InterruptFlags::TIMER);
        } else {
            ctx.timer_mut().interrupt_delay -= tcycles;
        }
    } else {
        ctx.timer_mut().interrupt_queued = false;
    }
}

pub fn tick(ctx: &mut impl TimerContext, tcycles: u64) {
    let mut divider = ctx.ioregs().divider();

    check_interrupt(ctx, tcycles);

    if ctx
        .ioregs()
        .timer_control()
        .contains(TimerControl::TIMER_ENABLE)
    {
        let period = ctx.ioregs().timer_control().get_period();

        // falling edge case when divider has been reset
        if (ctx.timer().old_divider & (period >> 1)) != 0 && (divider & (period >> 1)) == 0 {
            increment_timer(ctx);
        } else {
            let mask = period - 1;

            if divider & mask + tcycles as u16 & mask > period || tcycles > period as u64 {
                increment_timer(ctx)
            }
        }
    }

    // Update APU at ~512 Hz
    // period will have to be dynamic to support GBC double speed
    let period = 0x200;
    // falling edge case when divider has been reset
    if (ctx.timer().old_divider & (period >> 1)) != 0 && (divider & (period >> 1)) == 0 {
        apu::apu_tick(ctx);
    } else {
        let mask = period - 1;

        if divider & mask + tcycles as u16 & mask > period || tcycles > period as u64 {
            apu::apu_tick(ctx);
        }
    }

    divider = divider.wrapping_add(tcycles as u16);
    ctx.timer_mut().old_divider = ctx.ioregs().divider();
    ctx.ioregs_mut().set_divider(divider);
}
