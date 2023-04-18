//! Provides instructions, which are the definitions of what opcodes do.

use std::fmt;
use std::ops::Index;
use std::slice::SliceIndex;

use crate::compiler::instr::flow::Element;
use crate::microcode::Microcode;
use crate::opcode::{CBOpcode, InternalFetch, Opcode};

pub mod builder;
pub mod flow;

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

impl fmt::Display for InstrId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Opcode(opcode) => write!(f, "{}", opcode),
            Self::CBOpcode(cbopcode) => write!(f, "{}", cbopcode),
            Self::InternalFetch => write!(f, "{}", InternalFetch),
        }
    }
}

/// Definition of an instruction in the gbz80 CPU. Opcodes identify particular
/// instructions.
#[derive(Debug, Clone)]
pub struct InstrDef {
    /// ID of the instruction being defined.
    id: InstrId,
    /// Sequence of microcodes which make up this instruction definition.
    microcode: Vec<Microcode>,
    /// Code-flow of this InstrDef.
    flow: Element,
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

    /// Get the code-flow of this instruction definition.
    pub fn flow(&self) -> &Element {
        &self.flow
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
