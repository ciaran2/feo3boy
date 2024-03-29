pub use feo3boy_opcodes::gbz80types::Flags;

use crate::gbz80core::executor::ExecutorState;
use crate::interrupts::{InterruptContext, MemInterrupts};
use crate::memdev::{MemContext, MemDevice, RootExtend};

pub mod direct_executor;
pub mod direct_executor_v2;
pub mod executor;
mod externdefs;
pub mod microcode_executor;
pub mod stepping_executor;

/// CPU registers on the GB Z80 processor.
///
/// Note that there are a few other registers on a GameBoy, but those are memory mapped.
#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct Regs {
    // Registers are paired in little-endian order (though we aren't using any specific #[repr], so
    // compiler is free to reorder them).
    /// Register F.
    pub flags: Flags,
    /// Register A.
    pub acc: u8,
    /// Register C.
    pub c: u8,
    /// Register B.
    pub b: u8,
    /// Register E.
    pub e: u8,
    /// Register D.
    pub d: u8,
    /// Register L.
    pub l: u8,
    /// Register H.
    pub h: u8,
    /// Stack pointer.
    pub sp: u16,
    /// Program counter.
    pub pc: u16,
}

macro_rules! reg_pair_access {
    ($name:ident, $get:ident, $set:ident, $h:ident, $l:ident) => {
        /// Gets the value of register pair $name.
        pub fn $get(&self) -> u16 {
            u16::from_le_bytes([self.$l, self.$h])
        }

        /// Sets the value of register pair $name.
        pub fn $set(&mut self, val: u16) {
            let [low, high] = val.to_le_bytes();
            self.$l = low;
            self.$h = high;
        }
    };
}

impl Regs {
    /// Gets the value of register pair AF.
    pub fn af(&self) -> u16 {
        u16::from_le_bytes([self.flags.bits(), self.acc])
    }

    /// Sets the value of register pair AF. Any low-order bits will be truncated from F.
    pub fn set_af(&mut self, val: u16) {
        let [f, a] = val.to_le_bytes();
        self.flags = Flags::from_bits_truncate(f);
        self.acc = a;
    }

    reg_pair_access!(BC, bc, set_bc, b, c);
    reg_pair_access!(DE, de, set_de, d, e);
    reg_pair_access!(HL, hl, set_hl, h, l);
}

/// State of interrupts on the CPU.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum InterruptMasterState {
    /// Interrupts are disabled.
    Disabled,
    /// EI was just run, but the effect is delayed until after the next instruction.
    Pending,
    /// Interrupts are enabled.
    Enabled,
}

impl InterruptMasterState {
    /// Sets interrupts immediately.
    pub fn set(&mut self) {
        *self = Self::Enabled;
    }

    /// Disables interrupts immediately.
    pub fn clear(&mut self) {
        *self = Self::Disabled;
    }

    /// Sets interrupts after the next instruction. If interrupts are already enabled, does nothing.
    pub fn set_next_instruction(&mut self) {
        if *self != Self::Enabled {
            *self = Self::Pending;
        }
    }

    /// Returns true if IME is enabled.
    pub fn enabled(self) -> bool {
        self == Self::Enabled
    }

    /// Ticks the interrupt master state after an instruction has executed.
    fn tick(&mut self, previous_state: Self) {
        // If it was pending, and was not disabled by the instruction run in the mean time, set to
        // enabled.
        if previous_state == Self::Pending && *self != Self::Disabled {
            *self = Self::Enabled;
        }
    }
}

impl Default for InterruptMasterState {
    fn default() -> Self {
        Self::Disabled
    }
}

/// Internal state of the CPU.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Gbz80State {
    /// Cpu registers.
    pub regs: Regs,
    /// Interrupt master enable flag, controlled by EI, DI, RETI, and interrupts.
    pub interrupt_master_enable: InterruptMasterState,
    /// Whether the CPU is halted.
    pub halted: bool,
    /// Set to true when the halt bug is tripped until the double-read of the program
    /// counter.
    pub halt_bug: bool,
}

impl Gbz80State {
    /// Create a new Gbz80State.
    pub fn new() -> Gbz80State {
        Default::default()
    }
}

/// Context trait which encapsulates everything that the CPU needs in order to execute.
///
/// The purpose of this trait is to encapsulate the components needed to run the GB Z80 CPU,
/// independently of any other component of the GameBoy system. That allows the CPU to be run for
/// other purposes, by swapping in a memory controller that behaves differently.
pub trait ExecutorContext: CpuContext + MemContext + InterruptContext {
    type State: ExecutorState;

    /// Get the state of the executor.
    fn executor(&self) -> &Self::State;

    /// Mutably gets the state of the executor.
    fn executor_mut(&mut self) -> &mut Self::State;

    /// Yields from CPU execution for 1 M clock cycle (4 T). This callback should step the clock
    /// forward and perform any work that needs to happen faster than instructions execute.
    /// Warning: It is undefined behavior to call `tick` again during a context yield.
    fn yield1m(&mut self);
}

/// Context trait that provides access to just the CPU state.
pub trait CpuContext {
    /// Gets the CPU state.
    fn cpu(&self) -> &Gbz80State;

    /// Gets a mutable reference to the CPU state.
    fn cpu_mut(&mut self) -> &mut Gbz80State;
}

/////////////////////////////////////////
// Utility implementations of CpuContext.
/////////////////////////////////////////

/// Allows a tuple of Gbz80State and any MemDevice to be used as an [`ExecutorContext`].
impl<M: MemDevice> ExecutorContext for (Gbz80State, M) {
    type State = ();

    #[inline]
    fn executor(&self) -> &Self::State {
        &()
    }

    #[inline]
    fn executor_mut(&mut self) -> &mut Self::State {
        // since () is a ZST, we can just return our address as a mutable pointer to one.
        assert!(std::mem::size_of::<()>() == 0);
        unsafe { std::mem::transmute(self) }
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M> CpuContext for (Gbz80State, M) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        &self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        &mut self.0
    }
}

impl<M: MemDevice> MemContext for (Gbz80State, M) {
    type Mem = RootExtend<M>;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        RootExtend::wrap_ref(&self.1)
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        RootExtend::wrap_mut(&mut self.1)
    }
}

impl<M: MemDevice> InterruptContext for (Gbz80State, M) {
    type Interrupts = MemInterrupts<RootExtend<M>>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap_ref(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}

/// Allows a tuple of references to Gbz80State and any MemDevice to be used as CpuContext.
impl<M: MemDevice> ExecutorContext for (&mut Gbz80State, &mut M) {
    type State = ();

    #[inline]
    fn executor(&self) -> &Self::State {
        &()
    }

    #[inline]
    fn executor_mut(&mut self) -> &mut Self::State {
        // since () is a ZST, we can just return our address as a mutable pointer to one.
        assert!(std::mem::size_of::<()>() == 0);
        unsafe { std::mem::transmute(self) }
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M> CpuContext for (&mut Gbz80State, &mut M) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        self.0
    }
}

impl<M: MemDevice> MemContext for (&mut Gbz80State, &mut M) {
    type Mem = RootExtend<M>;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        RootExtend::wrap_ref(self.1)
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        RootExtend::wrap_mut(self.1)
    }
}

impl<M: MemDevice> InterruptContext for (&mut Gbz80State, &mut M) {
    type Interrupts = MemInterrupts<RootExtend<M>>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap_ref(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}

/// Allows a tuple of Gbz80State and any MemDevice and [`ExecutorState`] to be used as an
/// [`ExecutorContext`].
impl<M: MemDevice, S: ExecutorState> ExecutorContext for (Gbz80State, M, S) {
    type State = S;

    #[inline]
    fn executor(&self) -> &Self::State {
        &self.2
    }

    #[inline]
    fn executor_mut(&mut self) -> &mut Self::State {
        &mut self.2
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M, S> CpuContext for (Gbz80State, M, S) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        &self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        &mut self.0
    }
}

impl<M: MemDevice, S> MemContext for (Gbz80State, M, S) {
    type Mem = RootExtend<M>;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        RootExtend::wrap_ref(&self.1)
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        RootExtend::wrap_mut(&mut self.1)
    }
}

impl<M: MemDevice, S> InterruptContext for (Gbz80State, M, S) {
    type Interrupts = MemInterrupts<RootExtend<M>>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap_ref(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}

/// Allows a tuple of references to [`Gbz80State`] and any [`MemDevice`] and
/// [`ExecutorState`] to be used as CpuContext.
impl<M: MemDevice, S: ExecutorState> ExecutorContext for (&mut Gbz80State, &mut M, &mut S) {
    type State = S;

    #[inline]
    fn executor(&self) -> &Self::State {
        &self.2
    }

    #[inline]
    fn executor_mut(&mut self) -> &mut Self::State {
        &mut self.2
    }

    /// With just a Gbz80State and arbitrary MemDevice, yielding actually does nothing.
    #[inline]
    fn yield1m(&mut self) {}
}

impl<M: MemDevice, S> CpuContext for (&mut Gbz80State, &mut M, &mut S) {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        self.0
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        self.0
    }
}

impl<M: MemDevice, S> MemContext for (&mut Gbz80State, &mut M, &mut S) {
    type Mem = RootExtend<M>;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        RootExtend::wrap_ref(self.1)
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        RootExtend::wrap_mut(self.1)
    }
}

impl<M: MemDevice, S> InterruptContext for (&mut Gbz80State, &mut M, &mut S) {
    type Interrupts = MemInterrupts<RootExtend<M>>;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        MemInterrupts::wrap_ref(self.mem())
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        MemInterrupts::wrap_mut(self.mem_mut())
    }
}
