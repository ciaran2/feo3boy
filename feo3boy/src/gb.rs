use std::io::{self, Read, Write};

use crate::apu::{self, ApuContext, ApuRegs, ApuState};
use crate::gbz80core::direct_executor::DirectExecutor;
use crate::gbz80core::executor::Executor;
use crate::gbz80core::microcode_executor::MicrocodeExecutor;
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
    pub display_ready: bool,
    /// State of the executor.
    pub executor_state: E::State,
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

impl<E: Executor> Gb<E> {
    /// Create a `Gb` with the given bios and cartridge, using the executor specified by
    /// this type.
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
        }
    }

    /// Tick forward by one instruction, executing background and graphics processing
    /// operations as needed.
    pub fn tick(&mut self) -> (Option<&[(u8, u8, u8)]>, Option<(i16, i16)>) {
        E::run_single_instruction(self);

        if self.display_ready {
            self.display_ready = false;
            (
                Some(self.ppu.screen_buffer()),
                self.apu.consume_output_sample(),
            )
        } else {
            (None, self.apu.consume_output_sample())
        }
    }

    pub fn set_sample_rate(&mut self, sample_rate: u32) {
        self.apu.set_output_sample_rate(sample_rate);
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
