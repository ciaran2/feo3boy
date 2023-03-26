use crate::gbz80core::{self, CpuContext, Gbz80State};
use crate::interrupts::InterruptContext;
use crate::memdev::{BiosRom, Cartridge, GbMmu, MaskableMem, IoRegsContext, MemContext, Vram, Oam};
use crate::input::{self, InputContext, ButtonStates};
use crate::serial::{self, SerialContext, SerialState};
use crate::apu::{self, ApuContext, ApuState};
use crate::ppu::{self, PpuState, PpuContext};
use crate::timer::{self, TimerState, TimerContext};

/// Represents a "real" gameboy, by explicitly using the GbMmu for memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Gb {
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
}

impl Gb {
    /// Create a `Gb` with the given bios and cartridge.
    pub fn new(bios: BiosRom, cart: Cartridge) -> Self {
        Gb {
            cpustate: Gbz80State::new(),
            mmu: Box::new(GbMmu::new(bios, cart)),
            button_states: ButtonStates::empty(),
            serial: SerialState::new(),
            timer: TimerState::new(),
            apu: ApuState::new(),
            ppu: PpuState::new(),
            display_ready: false,
        }
    }

    /// Tick forward by one instruction, executing background and graphics processing
    /// operations as needed.
    pub fn tick(&mut self) -> (Option<&[(u8, u8, u8)]>, Option<(i16, i16)>) {
        gbz80core::tick(self);

        if self.display_ready {
            self.display_ready = false;
            (Some(self.ppu.screen_buffer()), self.apu.consume_output_sample())
        }
        else {
            (None, self.apu.consume_output_sample())
        }
    }

    pub fn set_sample_rate(&mut self, sample_rate: u32) {
        self.apu.set_output_sample_rate(sample_rate);
    }
}

impl CpuContext for Gb {
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        &self.cpustate
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        &mut self.cpustate
    }

    fn yield1m(&mut self) {
        // TODO: run background processing while yielded.
        // Continue processing serial while yielded.
        input::update(self);
        self.mmu.tick(4);
        serial::tick(self, 4);
        // apu must update before timer to catch falling edges from CPU writes
        apu::tick(self, 4);
        timer::tick(self, 4);
        ppu::tick(self, 4);
    }
}

impl MemContext for Gb {
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

impl InterruptContext for Gb {
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

impl IoRegsContext for Gb {
    type IoRegs = <GbMmu as IoRegsContext>::IoRegs;

    #[inline]
    fn ioregs(&self) -> &Self::IoRegs {
        self.mmu.ioregs()
    }

    #[inline]
    fn ioregs_mut(&mut self) -> &mut Self::IoRegs {
        self.mmu.ioregs_mut()
    }
}

impl InputContext for Gb {
    #[inline]
    fn button_states(&self) -> ButtonStates {
        self.button_states
    }

    fn set_button_states(&mut self, button_states: ButtonStates) {
        self.button_states = button_states;
    }
}

impl SerialContext for Gb {
    #[inline]
    fn serial(&self) -> &SerialState {
        &self.serial
    }

    fn serial_mut(&mut self) -> &mut SerialState {
        &mut self.serial
    }
}

impl TimerContext for Gb {
    #[inline]
    fn timer(&self) -> &TimerState {
        &self.timer
    }

    fn timer_mut(&mut self) -> &mut TimerState {
        &mut self.timer
    }
}

impl ApuContext for Gb {
    #[inline]
    fn apu(&self) -> &ApuState {
        &self.apu
    }

    fn apu_mut(&mut self) -> &mut ApuState {
        &mut self.apu
    }
}

impl PpuContext for Gb {
    #[inline]
    fn ppu(&self) -> &PpuState {
        &self.ppu
    }

    fn ppu_mut(&mut self) -> &mut PpuState {
        &mut self.ppu
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
