use std::io::{self, Read, Write};
use std::time::Duration;

use crate::apu::{self, ApuContext, ApuRegs, ApuState};
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
use crate::timer::{self, TimerContext, TimerRegs, TimerState};

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
    /// Total number of m-cycles that have passed since the Gb started (not saved,
    /// restarts when starting from a savestate or cartrige ram save).
    mcycles: u64,
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
    pub fn for_executor(bios: BiosRom, cart: Cartridge) -> Self {
        Gb {
            cpustate: Gbz80State::new(),
            mmu: Box::new(GbMmu::new(bios, cart)),
            button_states: ButtonStates::empty(),
            serial: SerialState::new(),
            timer: TimerState::new(),
            apu: ApuState::new(),
            ppu: PpuState::new(),
            display_ready: false,
            executor_state: E::State::default(),
            mcycles: 0,
        }
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
            mcycles: 0,
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

    /// Get the total number of mcycles elapsed since emulator start.
    #[inline]
    pub fn elapsed_mcycles(&self) -> u64 {
        self.mcycles
    }

    /// Get the total duration since the emulator started (based on the number of m-cycles
    /// elapsed.
    pub fn elapsed_time(&self) -> Duration {
        const MCYCLES_PER_SEC: u64 = 1_048_576;
        const NANOS_SPER_SEC: u64 = 1_000_000_000;
        let secs = self.mcycles / MCYCLES_PER_SEC;
        let rem = self.mcycles % MCYCLES_PER_SEC;
        let nanos = rem * NANOS_SPER_SEC / MCYCLES_PER_SEC;
        Duration::new(secs, nanos as u32)
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
        self.mcycles = self.mcycles.wrapping_add(1);
        // TODO: run background processing while yielded.
        // Continue processing serial while yielded.
        input::update(self);
        serial::tick(self, 4);
        // apu must update before timer to catch falling edges from CPU writes
        apu::tick(self, 4);
        timer::tick(self, 4);
        ppu::tick(self, 4);
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

    fn set_button_states(&mut self, button_states: ButtonStates) {
        self.button_states = button_states;
    }

    fn button_reg(&self) -> ButtonRegister {
        self.mmu.io.buttons
    }

    fn set_button_reg(&mut self, buttons: ButtonRegister) {
        self.mmu.io.buttons = buttons;
    }
}

impl<E: Executor> SerialContext for Gb<E> {
    #[inline]
    fn serial(&self) -> &SerialState {
        &self.serial
    }

    fn serial_mut(&mut self) -> &mut SerialState {
        &mut self.serial
    }

    fn serial_regs(&self) -> &SerialRegs {
        &self.mmu.io.serial_regs
    }

    fn serial_regs_mut(&mut self) -> &mut SerialRegs {
        &mut self.mmu.io.serial_regs
    }
}

impl<E: Executor> TimerContext for Gb<E> {
    #[inline]
    fn timer(&self) -> &TimerState {
        &self.timer
    }

    fn timer_mut(&mut self) -> &mut TimerState {
        &mut self.timer
    }

    fn timer_regs(&self) -> &TimerRegs {
        &self.mmu.io.timer_regs
    }

    fn timer_regs_mut(&mut self) -> &mut TimerRegs {
        &mut self.mmu.io.timer_regs
    }
}

impl<E: Executor> ApuContext for Gb<E> {
    #[inline]
    fn apu(&self) -> &ApuState {
        &self.apu
    }

    fn apu_mut(&mut self) -> &mut ApuState {
        &mut self.apu
    }

    fn apu_regs(&self) -> &ApuRegs {
        &self.mmu.io.apu_regs
    }

    fn apu_regs_mut(&mut self) -> &mut ApuRegs {
        &mut self.mmu.io.apu_regs
    }
}

impl<E: Executor> PpuContext for Gb<E> {
    #[inline]
    fn ppu(&self) -> &PpuState {
        &self.ppu
    }

    fn ppu_mut(&mut self) -> &mut PpuState {
        &mut self.ppu
    }

    fn ppu_regs(&self) -> &PpuRegs {
        &self.mmu.io.ppu_regs
    }

    fn ppu_regs_mut(&mut self) -> &mut PpuRegs {
        &mut self.mmu.io.ppu_regs
    }

    fn vram(&self) -> &Vram {
        &self.mmu.vram
    }
    fn vram_mut(&mut self) -> &mut Vram {
        &mut self.mmu.vram
    }

    fn oam(&self) -> &Oam {
        &self.mmu.oam
    }
    fn oam_mut(&mut self) -> &mut Oam {
        &mut self.mmu.oam
    }

    fn display_ready(&mut self) {
        self.display_ready = true;
    }
}
