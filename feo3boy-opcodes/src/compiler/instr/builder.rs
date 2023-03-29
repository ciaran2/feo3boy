//! Provides the [`InstrBuilder`] for building [`InstrDef`s][InstrDef] as well as helpers
//! treating values as multiple microcde steps.

use crate::compiler::instr::{InstrDef, InstrId};
use crate::microcode::Microcode;

/// Builder for microcode.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct InstrBuilder {
    microcode: Vec<Microcode>,
}

impl InstrBuilder {
    /// Create a new empty builder.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new builder with the given initial instructions.
    pub fn first(code: impl Into<InstrBuilder>) -> Self {
        code.into()
    }

    /// Create a builder containing just a yield.
    pub fn r#yield() -> Self {
        Self::first(Microcode::Yield)
    }

    // Create a block of microcode which will pop a single byte boolean off the microcode
    // stack and then execute one of two branches depending on whether it is nonzero
    // (true) or zero (false).
    pub fn cond(
        code_if_true: impl Into<InstrBuilder>,
        code_if_false: impl Into<InstrBuilder>,
    ) -> Self {
        // We will build code that is:
        // SkipIf { steps: false_branch.len() + 1 }
        // false_branch
        // Skip { steps: true_branch.len() }
        // true_branch
        //
        // Because SkipIf runs on a true condition, we put the false branch first.
        // We only add the extra unconditional skip if the true_branch is non-empty.

        let code_if_true = code_if_true.into();
        let code_if_false = code_if_false.into();

        // Both branches are empty, so the condition is irrelevant, just discard it. We
        // still need to pop the condition off the stack though for consistency.
        if code_if_true.microcode.is_empty() && code_if_false.microcode.is_empty() {
            Microcode::Discard8.into()
        } else if code_if_true.microcode.is_empty() {
            // The true-case is empty, so just skip the false case if the value is true.
            InstrBuilder::first(Microcode::SkipIf {
                steps: code_if_false.len(),
            })
            .then(code_if_false)
        } else if code_if_false.microcode.is_empty() {
            // The false-case is empty, so just skip the true case if the value is false.
            InstrBuilder::first(Microcode::Not)
                .then(Microcode::SkipIf {
                    steps: code_if_true.len(),
                })
                .then(code_if_true)
        } else {
            // Both true and false cases are non-empty. Modify code_if_false by adding a
            // step to skip the length of code_if_true.
            let code_if_false = code_if_false.then(Microcode::Skip {
                steps: code_if_true.len(),
            });
            InstrBuilder::first(Microcode::SkipIf {
                steps: code_if_false.len(),
            })
            .then(code_if_false)
            .then(code_if_true)
        }
    }

    /// Pop a single byte boolean off the microcode stack and run this if it is true.
    pub fn if_true(code_if_true: impl Into<InstrBuilder>) -> Self {
        InstrBuilder::cond(code_if_true, InstrBuilder::new())
    }

    /// Pop a single byte boolean off the microcode stack and run this if it is false.
    pub fn if_false(code_if_false: impl Into<InstrBuilder>) -> Self {
        InstrBuilder::cond(InstrBuilder::new(), code_if_false)
    }

    /// Chooses between two branches depending on whether the u8 on top of the stack is
    /// non-zero.
    pub fn then_cond(
        self,
        code_if_true: impl Into<InstrBuilder>,
        code_if_false: impl Into<InstrBuilder>,
    ) -> Self {
        self.then(InstrBuilder::cond(code_if_true, code_if_false))
    }

    /// Run the given code if the u8 on top of the stack is non-zero.
    pub fn then_if_true(self, code_if_true: impl Into<InstrBuilder>) -> Self {
        self.then_cond(code_if_true, InstrBuilder::new())
    }

    /// Run the given code if the u8 on top of the stack is zero.
    pub fn then_if_false(self, code_if_false: impl Into<InstrBuilder>) -> Self {
        self.then_cond(InstrBuilder::new(), code_if_false)
    }

    /// Create a single-step microcode that reads from the specified source.
    pub fn read<R: MicrocodeReadable>(from: R) -> Self {
        from.to_read()
    }

    /// Create a single-step microcode that writes to the specified source.
    pub fn write<W: MicrocodeWritable>(to: W) -> Self {
        to.to_write()
    }

    /// Add the given instruction or instructions to the end of this microcode.
    pub fn then(mut self, code: impl Into<InstrBuilder>) -> Self {
        let other = code.into();
        self.microcode.extend(other.microcode);
        self
    }

    /// Add a yield to the end of the builder.
    pub fn then_yield(mut self) -> Self {
        self.microcode.push(Microcode::Yield);
        self
    }

    /// Add a read to the end of the builder.
    pub fn then_read<R: MicrocodeReadable>(self, from: R) -> Self {
        self.then(from.to_read())
    }

    /// Add a write to the end of the builder.
    pub fn then_write<W: MicrocodeWritable>(self, to: W) -> Self {
        self.then(to.to_write())
    }

    /// Gest the number of steps currently in this microcode builder.
    pub fn len(&self) -> usize {
        self.microcode.len()
    }

    /// Build an instruction from this microcode, adding the id for the instruction.
    pub fn build(self, id: InstrId) -> InstrDef {
        InstrDef {
            id,
            microcode: self.microcode,
        }
    }
}

impl<T: Into<InstrBuilder>> From<Option<T>> for InstrBuilder {
    fn from(value: Option<T>) -> Self {
        value.map(Into::into).unwrap_or_default()
    }
}

impl From<Microcode> for InstrBuilder {
    fn from(value: Microcode) -> Self {
        Self {
            microcode: vec![value],
        }
    }
}

/// Allows a type that represents a target that can be read or written to be used with
/// [`.read`](InstrBuilder::read) or [`.then_read`][InstrBuilder::then_read].
pub trait MicrocodeReadable {
    fn to_read(self) -> InstrBuilder;
}

/// Allows a type that represents a target that can be read or written to be used with
/// [`.write`](InstrBuilder::write) or [`.then_write`][InstrBuilder::then_write].
pub trait MicrocodeWritable {
    fn to_write(self) -> InstrBuilder;
}
