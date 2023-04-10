//! Provides the [`InstrBuilder`] for building [`InstrDef`s][InstrDef] as well as helpers
//! treating values as multiple microcde steps.

use crate::compiler::instr::flow::{Branch, Element};
use crate::compiler::instr::{InstrDef, InstrId};
use crate::microcode::Microcode;

/// Builder for microcode.
#[derive(Default, Debug, Clone)]
pub struct InstrBuilder {
    /// Code-flow being built.
    flow: Element,
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
        Self {
            flow: Element::Branch(Branch {
                code_if_true: Box::new(code_if_true.into().flow),
                code_if_false: Box::new(code_if_false.into().flow),
            }),
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
        self.flow.extend_block(other.flow);
        self
    }

    /// Add a yield to the end of the builder.
    pub fn then_yield(self) -> Self {
        self.then(Microcode::Yield)
    }

    /// Add a read to the end of the builder.
    pub fn then_read<R: MicrocodeReadable>(self, from: R) -> Self {
        self.then(from.to_read())
    }

    /// Add a write to the end of the builder.
    pub fn then_write<W: MicrocodeWritable>(self, to: W) -> Self {
        self.then(to.to_write())
    }

    /// Build an instruction from this microcode, adding the id for the instruction.
    pub fn build(mut self, id: InstrId) -> InstrDef {
        add_fetch_next_to_end(&mut self.flow);
        InstrDef {
            id,
            microcode: self.flow.flatten(),
            flow: self.flow,
        }
    }
}

/// Add a fetch-next to every tail of the instruction.
fn add_fetch_next_to_end(elem: &mut Element) {
    match elem {
        Element::Microcode(microcode) => {
            match microcode {
                // If the code is a terminal op, no need to add a fetch.
                Microcode::FetchNextInstruction
                | Microcode::ParseOpcode
                | Microcode::ParseCBOpcode => {}
                // Add a FetchNextInstruction after all other operations.
                _ => elem.extend_block(Element::Microcode(Microcode::FetchNextInstruction)),
            }
        }
        Element::Block(block) => match block.elements.last_mut() {
            Some(last) => {
                if let Element::Microcode(microcode) = last {
                    // Avoid creating a nested block.
                    match microcode {
                        // If the code is a terminal op, no need to add a fetch.
                        Microcode::FetchNextInstruction
                        | Microcode::ParseOpcode
                        | Microcode::ParseCBOpcode => {}
                        // Add a FetchNextInstruction after all other operations.
                        _ => block
                            .elements
                            .push(Element::Microcode(Microcode::FetchNextInstruction)),
                    }
                } else {
                    add_fetch_next_to_end(last)
                }
            }
            None => {
                // If the block is empty, replace it with just the fetch instruction.
                *elem = Element::Microcode(Microcode::FetchNextInstruction)
            }
        },
        Element::Branch(Branch {
            code_if_true,
            code_if_false,
        }) => {
            let true_end_terminal = ends_with_terminal(code_if_true);
            let false_end_terminal = ends_with_terminal(code_if_false);
            if !true_end_terminal && !false_end_terminal {
                // neither branch ends in a terminal, so just append the terminal after
                // this branch.
                elem.extend_block(Element::Microcode(Microcode::FetchNextInstruction));
            } else if !true_end_terminal {
                // Only true branch lacks a terminal, so put the fetch next there.
                add_fetch_next_to_end(code_if_true);
            } else if !false_end_terminal {
                // Only false branch lacks a terminal, so put the fetch next there.
                add_fetch_next_to_end(code_if_false);
            }
            // Both branches already had a terminal, no need to add a fetch.
        }
    }
}

/// Return true if the given element ends with FetchNextInstruction, ParseOpcode, or
/// ParseCBOpcode. For Branches, only returns true if all branches end with one of those
/// opcodes.
fn ends_with_terminal(elem: &Element) -> bool {
    match elem {
        Element::Microcode(microcode) => match microcode {
            Microcode::FetchNextInstruction | Microcode::ParseOpcode | Microcode::ParseCBOpcode => {
                true
            }
            _ => false,
        },
        Element::Block(block) => match block.elements.last() {
            Some(last) => ends_with_terminal(last),
            None => false,
        },
        Element::Branch(Branch {
            code_if_true,
            code_if_false,
        }) => ends_with_terminal(&code_if_true) && ends_with_terminal(&code_if_false),
    }
}

impl<T: Into<InstrBuilder>> From<Option<T>> for InstrBuilder {
    fn from(value: Option<T>) -> Self {
        value.map(Into::into).unwrap_or_default()
    }
}

impl From<Microcode> for InstrBuilder {
    fn from(value: Microcode) -> Self {
        if let Microcode::Skip { .. } | Microcode::SkipIf { .. } = value {
            panic!("InstrBuilder does not permit explicit skips. Use `cond`.");
        }

        Self {
            flow: Element::Microcode(value),
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
