use std::mem;

use crate::bits::BitGroup;
use crate::clock::{ClockSnapshot, MCycle, SystemClockContext, TimedChangeTracker};
use crate::interrupts::{InterruptContext, InterruptFlags, Interrupts};
use crate::memdev::{MemDevice, ReadCtx, RelativeAddr, WriteCtx};

/// Represends sound and volume settings.
#[derive(Default, Debug, Clone, Eq, PartialEq, Hash, MemDevice)]
#[memdev(bits, readable = TimerControl::RW_BITS, writable = TimerControl::RW_BITS)]
#[repr(transparent)]
pub struct TimerControl(u8);

impl TimerControl {
    const TIMER_PERIOD: BitGroup = BitGroup(0b0000_0011);
    const TIMER_ENABLE: BitGroup = BitGroup(0b0000_0100);
    const RW_BITS: u8 = 0b0000_0111;

    pub const fn bits(&self) -> u8 {
        self.0
    }

    /// Get the timer period selecton number, this is *not* the actual period value
    /// selected.
    #[inline]
    pub const fn period_idx(&self) -> u8 {
        Self::TIMER_PERIOD.extract(self.0)
    }

    /// Get the actual period selected.
    #[inline]
    pub const fn period(&self) -> u16 {
        match self.period_idx() {
            0 => 1024,
            1 => 16,
            2 => 64,
            3 => 256,
            _ => panic!("Illegal timer period number encountered. This should be impossible."),
        }
    }

    /// Get a mask which extracts the bit from the divider which corresponds to the
    /// selected period.
    #[inline]
    const fn divider_bit(&self) -> u16 {
        self.period() >> 1
    }

    /// Set the timer period selecton number, this is *not* the actual period value
    /// selected.
    #[inline]
    pub fn set_period_idx(&mut self, val: u8) {
        Self::TIMER_PERIOD.apply(&mut self.0, val);
    }

    /// Get the timer enable
    #[inline]
    pub const fn timer_enable(&self) -> bool {
        Self::TIMER_ENABLE.extract_bool(self.0)
    }

    /// Set the timer enable
    #[inline]
    pub fn set_timer_enable(&mut self, val: bool) {
        Self::TIMER_ENABLE.apply(&mut self.0, val as u8);
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq)]
struct TimerCounter(TimedChangeTracker<u8>);

impl TimerCounter {
    /// Increment the timer and return true if it overflowed.
    fn increment(&mut self) -> bool {
        let (timer, overflow) = self.0.overflowing_add(1);
        self.0.set_untracked(timer);
        overflow
    }

    /// Get the value of the timer counter.
    pub fn value(&self) -> u8 {
        *self.0
    }
}

#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct TimerDivider(TimedChangeTracker<()>);

impl TimerDivider {
    /// Clear the divider to zero and reset the modification time.
    fn reset(&mut self, now: ClockSnapshot) {
        self.0.set(now, ());
    }

    /// Get the portion of the divider register that is visible to the CPU.
    fn cpu_visible_portion(&self, ctx: &ReadCtx) -> u8 {
        // Is this faster than using a shift or division? Do all those options optimize to
        // the same thing? No idea!
        let [_, high] = self.value(ctx.atime().elapsed_cycles()).to_le_bytes();
        high
    }

    /// Get the raw value of the divider at the specified time. The time must be after the time when
    /// the divider was last reset.
    pub fn value(&self, now: MCycle) -> u16 {
        debug_assert!(self.0.changed_cycle() <= now);
        (now - self.0.changed_cycle()).as_tcycle().as_u64() as u16
    }

    /// Get the clock cycle when the divider was last reset by the CPU.
    pub fn reset_cycle(&self) -> MCycle {
        self.0.changed_cycle()
    }
}

/// Memory-mapped IO registers used by the Timer.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TimerRegs {
    /// Divider is incremented at 16 KiHz or 32 KiHz at double speed. This register
    /// controls when the other timer registers are incremented.
    divider: TimerDivider,
    /// Main counter register for the timer, incremented whenever the bit of `divider`
    /// selected by `timer_period` goes from 1 to 0.
    timer_counter: TimerCounter,
    /// Timer modulo is the value that the timer counter gets reset to whenever it
    /// overflows.
    timer_mod: u8,
    /// Control register for the timer which controls whether the timer is enabled and
    /// what the period is.
    timer_control: TimerControl,
    /// Whether the timer is enabled, cached from timer_control.
    timer_enable: bool,
    /// The bit to be selected from the divider, derived from the timer period (cached
    /// value of timer_control.period() >> 1).
    selected_divider_bit: u16,
}

impl TimerRegs {
    /// Set the value in the counter to the value in the `timer_mod`.
    fn reset_counter_to_mod(&mut self) {
        self.timer_counter.0.set_untracked(self.timer_mod);
    }

    /// Get the divider register.
    pub fn divider(&self) -> &TimerDivider {
        &self.divider
    }

    /// Get the value of the timer counter register.
    pub fn counter(&self) -> u8 {
        self.timer_counter.value()
    }

    /// Get the value of the timer mod register.
    pub fn modulus(&self) -> u8 {
        self.timer_mod
    }

    /// Get the internal value of the timer control register (all bits, readable or otherwise).
    pub fn control(&self) -> u8 {
        self.timer_control.bits()
    }
}

impl Default for TimerRegs {
    fn default() -> Self {
        let timer_control = TimerControl::default();
        let timer_enable = timer_control.timer_enable();
        let selected_divider_bit = timer_control.divider_bit();
        Self {
            divider: Default::default(),
            timer_counter: Default::default(),
            timer_mod: Default::default(),
            timer_control,
            timer_enable,
            selected_divider_bit,
        }
    }
}

impl MemDevice for TimerRegs {
    const LEN: usize = 4;

    fn read_byte_relative(&self, ctx: &ReadCtx, addr: RelativeAddr) -> u8 {
        match addr.relative() {
            0x00 => self.divider.cpu_visible_portion(ctx),
            0x01 => *self.timer_counter.0,
            0x02 => self.timer_mod,
            0x03 => self
                .timer_control
                .read_byte_relative(ctx, addr.offset_by(0x03)),
            _ => panic!("Address {} out of range for TimerRegs", addr),
        }
    }

    fn write_byte_relative(&mut self, ctx: &WriteCtx, addr: RelativeAddr, val: u8) {
        match addr.relative() {
            0x00 => self.divider.reset(*ctx.atime()),
            0x01 => {
                self.timer_counter.0.set(*ctx.atime(), val);
            }
            0x02 => self.timer_mod = val,
            0x03 => {
                self.timer_control
                    .write_byte_relative(ctx, addr.offset_by(0x03), val);
                self.timer_enable = self.timer_control.timer_enable();
                self.selected_divider_bit = self.timer_control.divider_bit();
            }
            _ => panic!("Address {} out of range for TimerRegs", addr),
        }
    }

    memdev_bytes_from_byte!(TimerRegs);
}

/// Context trait providing access to fields needed to service graphics.
pub trait TimerContext: InterruptContext + SystemClockContext {
    /// Get the timer state.
    fn timer(&self) -> &TimerState;

    /// Get mutable access to the timer state.
    fn timer_mut(&mut self) -> &mut TimerState;

    fn timer_regs(&self) -> &TimerRegs;
    fn timer_regs_mut(&mut self) -> &mut TimerRegs;
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TimerState {
    /// The bit selected from the divider last cycle. The timer increments whenever the
    /// bit selected from the divider goes from 1 to 0.
    old_divider_bit: bool,
    /// Tracks when the timer overflow
    overflow_timing: Option<TimerOverflowTiming>,
}

impl TimerState {
    pub fn new() -> TimerState {
        Default::default()
    }

    /// Set the interrupt to be triggered after appropriate delay.
    fn queue_interrupt(&mut self, now: MCycle) {
        self.overflow_timing = Some(TimerOverflowTiming {
            overflowed_at: now,
            interrupt_at: now + 1,
        });
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
struct TimerOverflowTiming {
    /// The clock cycle when the timer overflowed. This is the cycle where the TIMA
    /// register is set to 00 and readable as 00 by the CPU.
    overflowed_at: MCycle,
    /// The clock cycle when the interrupt should be sent.
    interrupt_at: MCycle,
}

pub fn tick(ctx: &mut impl TimerContext) {
    let now = ctx.clock().elapsed_cycles();

    // We need to handle weird behavior related to cycles when the timer overflows (copied
    // from https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html).
    //
    //           [Z] [A] [B][C]
    // SYS  FD FE FF |00| 01 02 03
    // TIMA FF FF FF |00| 23 23 23
    // TMA  23 23 23 |23| 23 23 23
    // IF   E0 E0 E0 |E0| E4 E4 E4

    if let Some(TimerOverflowTiming {
        overflowed_at,
        interrupt_at,
    }) = ctx.timer().overflow_timing
    {
        if ctx.timer_regs().timer_counter.0.changed_cycle() == overflowed_at {
            // The timer register written on the same cycle that it overflowed. This
            // aborts the interrupt.
            ctx.timer_mut().overflow_timing = None;
        } else if now == interrupt_at {
            // We are on the interrupt cycle.
            ctx.timer_regs_mut().reset_counter_to_mod();
            ctx.interrupts_mut().send(InterruptFlags::TIMER);
        } else if now > interrupt_at {
            debug_assert!(
                now == interrupt_at + 1,
                "This assumes tick is called every m-cycle"
            );
            // We have to wait until the next cycle to clear overflow_timing because we
            // need to patch some of the timer registers if the CPU does a write in the
            // same cycle as we reset the counter to the modulus.
            ctx.timer_mut().overflow_timing = None;
            // If the CPU wrote to the timer_mod register on the same cycle that we reset
            // it to the mod, we should take the value written by the CPU.
            // If the CPU wrote to the timer_counter register on the same cycle that we
            // reset it to the mod, we should ignore the value written by the CPU and set
            // to the mod anyway.
            // Both of these are equivalent to overwriting timer_counter with the
            // timer_mod the cycle after we queued the interrupt (in addition to doing so
            // on the same cycle as we queued the interrupt, which we do in case the CPU
            // is reading the timer_counter that cycle rather than writing).
            //
            // As long as tick is called every m-cycle, we don't have to worry about the
            // timer being incremented in the interum, because this can only happen if the
            // selected bit from the divider just went to zero and there's no way for it
            // to become 1 and then 0 again in fewer than 3 m-cycles:
            //
            // - The fastest `period` is every-other m-cycle, so since the bit became zero
            //   on `overflowed_at`, the earliest it can become 1 is interrupt_at + 1
            //   (which occurs below, so this code executes before that increment anyway).
            // - The CPU could not be reseting that bit to 0 and changing timer_mod in the
            //   same cycle. CPU is too slow to do both of those things in the space
            //   between these timer ticks.
            ctx.timer_regs_mut().reset_counter_to_mod();
        }
    }

    let divider_bit = {
        let &TimerRegs {
            timer_enable,
            selected_divider_bit,
            ref divider,
            ..
        } = ctx.timer_regs();
        let divider = divider.value(now);
        timer_enable && (divider & selected_divider_bit) != 0
    };
    let old_divider_bit = mem::replace(&mut ctx.timer_mut().old_divider_bit, divider_bit);

    if old_divider_bit && !divider_bit {
        if ctx.timer_regs_mut().timer_counter.increment() {
            ctx.timer_mut().queue_interrupt(now);
        }
    }
}
