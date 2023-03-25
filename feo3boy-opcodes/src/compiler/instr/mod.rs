//! Provides instructions, which are the definitions of what opcodes do.

use std::ops::Index;
use std::slice::SliceIndex;

use crate::microcode::Microcode;
use crate::opcode::{CBOpcode, Opcode};

pub mod builder;

/// Which opcode this instruction implements.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InstrId {
    /// Instruction implements a normal opcode.
    Opcode(Opcode),
    /// Instruction implements the second byte of a CB opcode.
    CBOpcode(CBOpcode),
    /// Instruction implements the CPU internal-fetch routine.
    InternalFetch,
}

/// Definition of an instruction in the gbz80 CPU. Opcodes identify particular
/// instructions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstrDef {
    /// ID of the instruction being defined.
    id: InstrId,
    /// Sequence of microcodes which make up this instruction definition.
    microcode: Vec<Microcode>,
}

impl InstrDef {
    /// Get the number of microcode instructions in this Instr.
    pub fn len(&self) -> usize {
        self.microcode.len()
    }

    /// Get the label applied to this InstrDef.
    pub fn id(&self) -> InstrId {
        self.id
    }
}

/// Get the microcode at the given index.
impl<T> Index<T> for InstrDef
where
    T: SliceIndex<[Microcode]>,
{
    type Output = <T as SliceIndex<[Microcode]>>::Output;

    fn index(&self, index: T) -> &Self::Output {
        &self.microcode[index]
    }
}
