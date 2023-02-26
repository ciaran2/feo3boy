use crate::gbz80core::{self, CpuContext, Gbz80State};
use crate::interrupts::InterruptContext;
use crate::memdev::{BiosRom, Cartridge, GbMmu, MaskableMem, IoRegsContext, MemContext};
use crate::serial::{self, SerialContext, SerialState};
use crate::ppu::{self, PpuState, PpuContext};

/// Represents a "real" gameboy, by explicitly using the GbMmu for memory.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Gb {
    /// State of the CPU in the system.
    pub cpustate: Gbz80State,
    /// MMU for the system.
    pub mmu: Box<GbMmu>,
    /// State of serial data transfer.
    pub serial: SerialState,
    /// State of video rendering
    pub ppu: PpuState,
}

impl Gb {
    /// Create a `Gb` with the given bios and cartridge.
    pub fn new(bios: BiosRom, cart: Cartridge) -> Self {
        Gb {
            cpustate: Gbz80State::new(),
            mmu: Box::new(GbMmu::new(bios, cart)),
            serial: SerialState::new(),
            ppu: PpuState::new(),
        }
    }

    /// Tick forward by one instruction, executing background and graphics processing
    /// operations as needed.
    pub fn tick(&mut self) {
        gbz80core::tick(self);
        serial::tick(self, 4);
        ppu::tick(self, 4);
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
        serial::tick(self, 4);
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

impl SerialContext for Gb {
    #[inline]
    fn serial(&self) -> &SerialState {
        &self.serial
    }

    fn serial_mut(&mut self) -> &mut SerialState {
        &mut self.serial
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

    fn vram(&self) -> &MaskableMem<0x2000> {
        &self.mmu.vram
    }
    fn vram_mut(&mut self) -> &mut MaskableMem<0x2000> {
        &mut self.mmu.vram
    }
    
    fn oam(&self) -> &MaskableMem<160> {
        &self.mmu.oam
    }
    fn oam_mut(&mut self) -> &mut MaskableMem<160> {
        &mut self.mmu.oam
    }
    
}
