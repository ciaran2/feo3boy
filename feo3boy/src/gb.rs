use std::io::{self, Read, Write};
use std::marker::PhantomData;

use crate::apu::{self, ApuContext, ApuRegs, ApuState};
use crate::clock::{SystemClock, SystemClockContext};
use crate::gbz80core::direct_executor::DirectExecutor;
use crate::gbz80core::direct_executor_v2::DirectExecutorV2;
use crate::gbz80core::executor::{Executor, ExecutorConfig};
use crate::gbz80core::microcode_executor::MicrocodeExecutor;
use crate::gbz80core::stepping_executor::SteppingExecutor;
use crate::gbz80core::{CpuContext, ExecutorContext, Gbz80State};
use crate::input::{self, ButtonRegister, ButtonStates, InputContext};
use crate::interrupts::InterruptContext;
use crate::memdev::{BiosRom, Cartridge, GbMmu, MemContext, Oam, SaveData, Vram};
use crate::ppu::{self, PpuContext, PpuRegs, PpuState};
use crate::serial::{self, SerialContext, SerialRegs, SerialState};
use crate::timer::{self, TimerContext, TimerDivider, TimerRegs, TimerState};

/// Represents a "real" gameboy, by explicitly using the GbMmu for memory.
#[derive(Clone, Debug)]
pub struct Gb<E: Executor = DirectExecutor> {
    /// State of the CPU in the system.
    pub cpustate: Gbz80State,
    /// MMU for the system.
    pub mmu: Box<GbMmu>,
    /// Joypad button states
    pub button_states: ButtonStates,
    /// State of serial data transfer.
    pub serial: SerialState,
    /// State of timer updates
    pub timer: TimerState,
    /// State of audio rendering
    pub apu: ApuState,
    /// State of video rendering
    pub ppu: PpuState,
    /// Whether display is ready to be sent to the outside world
    display_ready: bool,
    /// State of the executor.
    executor_state: E::State,
    /// System clock tracking cycles and real time in the emulator.
    clock: SystemClock,
}

impl Gb {
    /// Create a `Gb` with the given bios and cartridge, using the default executor.
    pub fn new(bios: BiosRom, cart: Cartridge) -> Self {
        Self::for_executor(bios, cart)
    }
}

impl Gb<MicrocodeExecutor> {
    /// Create a `Gb` with the given bios and cartridge, using the microcode executor
    pub fn new_microcode(bios: BiosRom, cart: Cartridge) -> Self {
        Self::for_executor(bios, cart)
    }
}

impl Gb<DirectExecutorV2> {
    /// Create a `Gb` with the given bios and cartridge, using the V2 direct executor.
    pub fn new_v2(bios: BiosRom, cart: Cartridge) -> Self {
        Self::for_executor(bios, cart)
    }
}

impl Gb<SteppingExecutor> {
    /// Create a `Gb` with the given bios and cartridge, using the stepping executor
    pub fn new_stepping(bios: BiosRom, cart: Cartridge) -> Self {
        Self::for_executor(bios, cart)
    }
}

impl<E> Gb<E>
where
    E: Executor,
    E::State: Default,
{
    /// Create a `Gb` with the given bios and cartridge, using the executor specified by
    /// this type. Works for executors with state types that implement `Default`.
    #[inline]
    pub fn for_executor(bios: BiosRom, cart: Cartridge) -> Self {
        struct DefaultConfig<E>(PhantomData<*const E>);
        impl<E> DefaultConfig<E> {
            const CONFIG: Self = Self(PhantomData);
        }
        impl<E> ExecutorConfig for DefaultConfig<E>
        where
            E: Executor,
            E::State: Default,
        {
            type Executor = E;

            #[inline]
            fn create_initial_state(&self) -> <Self::Executor as Executor>::State {
                E::State::default()
            }
        }
        Self::for_config(bios, cart, &DefaultConfig::<E>::CONFIG)
    }
}

impl<E: Executor> Gb<E> {
    /// Create a `Gb` with the given bios and cartridge, using the provided config to
    /// create the initial state of the executor.
    pub fn for_config<C>(bios: BiosRom, cart: Cartridge, config: &C) -> Self
    where
        C: ExecutorConfig<Executor = E>,
    {
        Gb {
            cpustate: Gbz80State::new(),
            mmu: Box::new(GbMmu::new(bios, cart)),
            button_states: ButtonStates::empty(),
            serial: SerialState::new(),
            timer: TimerState::new(),
            apu: ApuState::new(),
            ppu: PpuState::new(),
            display_ready: false,
            executor_state: config.create_initial_state(),
            clock: SystemClock::new(),
        }
    }

    /// Tick forward by one instruction, executing background and graphics processing
    /// operations as needed.
    pub fn tick(&mut self) {
        self.display_ready = false;
        E::run_single_instruction(self);
    }

    /// Set the sample rate for the APU.
    #[inline]
    pub fn set_sample_rate(&mut self, sample_rate: u32) {
        self.apu.set_output_sample_rate(sample_rate);
    }

    /// Returns true if the PPU finished a frame this tick and the display is ready to be
    /// sent out.
    #[inline]
    pub fn display_ready(&self) -> bool {
        self.display_ready
    }

    /// If the display is ready, get a reference to the PPU frame buffer.
    pub fn get_ready_frame(&self) -> Option<&[(u8, u8, u8)]> {
        if self.display_ready {
            Some(self.ppu.screen_buffer())
        } else {
            None
        }
    }

    /// Get a read-only reference to the GameBoy's internal system clock.
    pub fn clock(&self) -> &SystemClock {
        &self.clock
    }
}

impl<E: Executor> SaveData for Gb<E> {
    fn write_save_data(&self, writer: impl Write) -> Result<(), io::Error> {
        self.mmu.cart.write_save_data(writer)
    }

    fn load_save_data(&mut self, reader: impl Read) -> Result<(), io::Error> {
        self.mmu.cart.load_save_data(reader)
    }

    fn has_save_data(&self) -> bool {
        self.mmu.cart.has_save_data()
    }
}

impl<E: Executor> ExecutorContext for Gb<E> {
    type State = E::State;

    #[inline]
    fn executor(&self) -> &E::State {
        &self.executor_state
    }

    #[inline]
    fn executor_mut(&mut self) -> &mut E::State {
        &mut self.executor_state
    }

    fn yield1m(&mut self) {
        self.mmu.update_if_dirty(self.clock.snapshot());
        // TODO: run background processing while yielded.
        // Continue processing serial while yielded.
        input::update(self);
        serial::tick(self, 4);
        // apu must update before timer to catch falling edges from CPU writes
        apu::tick(self);
        ppu::tick(self, 4);

        self.clock.advance1m();
        // Within each m-cycle, the timer tick must run before the CPU tick.
        // Timer behavior with simultaneous CPU writes is a bit strange and somewhat
        // difficult to do correctly, since we have to treat things as sort of happening
        // at the same time even though we do these two things sequentially. In effect, we
        // want the timer to tick first so that the CPU will observe the correct state if
        // it does a read, and then if the CPU did a write, we can patch the timer state
        // based on the CPU's write on the next tick.
        timer::tick(self);
    }
}

impl<E: Executor> CpuContext for Gb<E> {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        &self.cpustate
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        &mut self.cpustate
    }
}

impl<E: Executor> MemContext for Gb<E> {
    type Mem = GbMmu;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        self.mmu.as_ref()
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        self.mmu.as_mut()
    }
}

impl<E: Executor> InterruptContext for Gb<E> {
    type Interrupts = <GbMmu as InterruptContext>::Interrupts;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        self.mmu.interrupts()
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        self.mmu.interrupts_mut()
    }
}

impl<E: Executor> InputContext for Gb<E> {
    #[inline]
    fn button_states(&self) -> ButtonStates {
        self.button_states
    }

    #[inline]
    fn set_button_states(&mut self, button_states: ButtonStates) {
        self.button_states = button_states;
    }

    #[inline]
    fn button_reg(&self) -> ButtonRegister {
        self.mmu.io.buttons
    }

    #[inline]
    fn set_button_reg(&mut self, buttons: ButtonRegister) {
        self.mmu.io.buttons = buttons;
    }
}

impl<E: Executor> SerialContext for Gb<E> {
    #[inline]
    fn serial(&self) -> &SerialState {
        &self.serial
    }

    #[inline]
    fn serial_mut(&mut self) -> &mut SerialState {
        &mut self.serial
    }

    #[inline]
    fn serial_regs(&self) -> &SerialRegs {
        &self.mmu.io.serial_regs
    }

    #[inline]
    fn serial_regs_mut(&mut self) -> &mut SerialRegs {
        &mut self.mmu.io.serial_regs
    }
}

impl<E: Executor> TimerContext for Gb<E> {
    #[inline]
    fn timer(&self) -> &TimerState {
        &self.timer
    }

    #[inline]
    fn timer_mut(&mut self) -> &mut TimerState {
        &mut self.timer
    }

    #[inline]
    fn timer_regs(&self) -> &TimerRegs {
        &self.mmu.io.timer_regs
    }

    #[inline]
    fn timer_regs_mut(&mut self) -> &mut TimerRegs {
        &mut self.mmu.io.timer_regs
    }
}

impl<E: Executor> ApuContext for Gb<E> {
    #[inline]
    fn apu(&self) -> &ApuState {
        &self.apu
    }

    #[inline]
    fn apu_mut(&mut self) -> &mut ApuState {
        &mut self.apu
    }

    #[inline]
    fn apu_regs(&self) -> &ApuRegs {
        &self.mmu.io.apu_regs
    }

    #[inline]
    fn apu_regs_mut(&mut self) -> &mut ApuRegs {
        &mut self.mmu.io.apu_regs
    }

    fn divider(&self) -> &TimerDivider {
        self.mmu.io.timer_regs.divider()
    }
}

impl<E: Executor> PpuContext for Gb<E> {
    #[inline]
    fn ppu(&self) -> &PpuState {
        &self.ppu
    }

    #[inline]
    fn ppu_mut(&mut self) -> &mut PpuState {
        &mut self.ppu
    }

    #[inline]
    fn ppu_regs(&self) -> &PpuRegs {
        &self.mmu.io.ppu_regs
    }

    #[inline]
    fn ppu_regs_mut(&mut self) -> &mut PpuRegs {
        &mut self.mmu.io.ppu_regs
    }

    #[inline]
    fn vram(&self) -> &Vram {
        &self.mmu.vram
    }
    #[inline]
    fn vram_mut(&mut self) -> &mut Vram {
        &mut self.mmu.vram
    }

    #[inline]
    fn oam(&self) -> &Oam {
        &self.mmu.oam
    }
    #[inline]
    fn oam_mut(&mut self) -> &mut Oam {
        &mut self.mmu.oam
    }

    #[inline]
    fn display_ready(&mut self) {
        self.display_ready = true;
    }
}

impl<E: Executor> SystemClockContext for Gb<E> {
    #[inline]
    fn clock(&self) -> &SystemClock {
        &self.clock
    }
}
