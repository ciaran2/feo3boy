use crate::apu::{self, ApuContext};
use crate::bits::BitGroup;
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{MemDevice, RelativeAddr};

/// Represends sound and volume settings.
#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits, readable = TimerControl::RW_BITS, writable = TimerControl::RW_BITS)]
#[repr(transparent)]
pub struct TimerControl(u8);

impl TimerControl {
    const TIMER_PERIOD: BitGroup = BitGroup(0b0000_0011);
    const TIMER_ENABLE: BitGroup = BitGroup(0b0000_0100);
    const RW_BITS: u8 = 0b0000_0111;

    pub fn bits(&self) -> u8 {
        self.0
    }

    /// Get the timer period selecton number, this is *not* the actual period value
    /// selected.
    #[inline]
    pub fn period_idx(&self) -> u8 {
        Self::TIMER_PERIOD.extract(self.0)
    }

    /// Get the actual period selected.
    #[inline]
    pub fn period(&self) -> u16 {
        match self.period_idx() {
            0 => 1024,
            1 => 16,
            2 => 64,
            3 => 256,
            _ => panic!("Illegal timer period number encountered. This should be impossible."),
        }
    }

    /// Set the timer period selecton number, this is *not* the actual period value
    /// selected.
    #[inline]
    pub fn set_period_idx(&mut self, val: u8) {
        Self::TIMER_PERIOD.apply(&mut self.0, val);
    }

    /// Get the timer enable
    #[inline]
    pub fn timer_enable(&self) -> bool {
        Self::TIMER_ENABLE.extract_bool(self.0)
    }

    /// Set the timer enable
    #[inline]
    pub fn set_timer_enable(&mut self, val: bool) {
        Self::TIMER_ENABLE.apply(&mut self.0, val as u8);
    }
}

/// Memory-mapped IO registers used by the Timer.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TimerRegs {
    pub divider: u16,
    pub timer: u8,
    pub timer_mod: u8,
    pub timer_control: TimerControl,
}

impl MemDevice for TimerRegs {
    const LEN: usize = 4;

    fn read_byte_relative(&self, addr: RelativeAddr) -> u8 {
        match addr.relative() {
            0x00 => (self.divider / 0x100) as u8,
            0x01 => self.timer,
            0x02 => self.timer_mod,
            0x03 => self.timer_control.read_byte_relative(addr.offset_by(0x03)),
            _ => panic!("Address {} out of range for TimerRegs", addr),
        }
    }

    fn write_byte_relative(&mut self, addr: RelativeAddr, val: u8) {
        match addr.relative() {
            0x00 => self.divider = 0,
            0x01 => self.timer = val,
            0x02 => self.timer_mod = val,
            0x03 => self
                .timer_control
                .write_byte_relative(addr.offset_by(0x03), val),
            _ => panic!("Address {} out of range for TimerRegs", addr),
        }
    }

    memdev_bytes_from_byte!(TimerRegs);
}

/// Context trait providing access to fields needed to service graphics.
pub trait TimerContext: InterruptContext + ApuContext {
    /// Get the timer state.
    fn timer(&self) -> &TimerState;

    /// Get mutable access to the timer state.
    fn timer_mut(&mut self) -> &mut TimerState;

    fn timer_regs(&self) -> &TimerRegs;
    fn timer_regs_mut(&mut self) -> &mut TimerRegs;
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
    let (timer, overflow) = ctx.timer_regs().timer.overflowing_add(1);

    if overflow {
        ctx.timer_mut().queue_interrupt();
    }

    ctx.timer_regs_mut().timer = timer;
}

fn check_interrupt(ctx: &mut impl TimerContext, tcycles: u64) {
    // an intervening write to the timer will discard the interrupt
    if ctx.timer().interrupt_queued && ctx.timer_regs().timer == 0 {
        if tcycles >= ctx.timer().interrupt_delay as u64 {
            ctx.timer_mut().interrupt_queued = false;
            let timer_mod = ctx.timer_regs().timer_mod;
            ctx.timer_regs_mut().timer = timer_mod;
            ctx.interrupts_mut().send(InterruptFlags::TIMER);
        } else {
            ctx.timer_mut().interrupt_delay -= tcycles;
        }
    } else {
        ctx.timer_mut().interrupt_queued = false;
    }
}

pub fn tick(ctx: &mut impl TimerContext, tcycles: u64) {
    let mut divider = ctx.timer_regs().divider;

    check_interrupt(ctx, tcycles);

    if ctx.timer_regs().timer_control.timer_enable() {
        let period = ctx.timer_regs().timer_control.period();

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
    ctx.timer_mut().old_divider = ctx.timer_regs().divider;
    ctx.timer_regs_mut().divider = divider;
}
