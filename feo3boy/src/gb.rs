use crate::gbz80core::{self, CpuContext, Gbz80State};
use crate::memdev::{BiosRom, Cartridge, GbMmu};

/// Represents a "real" gameboy, by explicitly using the GbMmu for memory.
pub struct Gb {
    /// State of the CPU in the system.
    cpustate: Gbz80State,
    /// MMU for the system.
    mmu: Box<GbMmu>,
}

impl Gb {
    /// Create a `Gb` with the given bios and cartridge.
    pub fn new(bios: BiosRom, cart: Cartridge) -> Self {
        Gb {
            cpustate: Gbz80State::new(),
            mmu: Box::new(GbMmu::new(bios, cart)),
        }
    }

    /// Tick forward by one instruction, executing background and graphics processing
    /// operations as needed.
    pub fn tick(&mut self) {
        gbz80core::tick::<_, Self>(self);
    }
}

impl CpuContext for Gb {
    type Mem = GbMmu;
    type Interrupts = GbMmu;

    #[inline]
    fn cpustate(&self) -> &Gbz80State {
        &self.cpustate
    }

    #[inline]
    fn cpustate_mut(&mut self) -> &mut Gbz80State {
        &mut self.cpustate
    }

    #[inline]
    fn mem(&self) -> &Self::Mem {
        self.mmu.as_ref()
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        self.mmu.as_mut()
    }

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        self.mmu.as_ref()
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        self.mmu.as_mut()
    }

    fn yield1m(&mut self) {
        // TODO: run background processing while yielded.
    }
}