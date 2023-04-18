//! Provides tools for generating the Direct Executor.

use crate::compiler::args::Arg;
use crate::compiler::instr::flow::{Block, Branch, Element};
use crate::compiler::variables::{Conversion, StackTracker, VarAssigner, VarAssignment, VarId};
use crate::microcode::{Microcode, ValType};

use super::instr::InstrDef;

/// [`Element`] with variable assignments and type conversions computed.
#[derive(Debug)]
pub enum FuncElement {
    /// A microcode instruction with variable assignments.
    Microcode(AssignedMicrocode),
    /// A code block with variable assignments computed.
    Block(AssignedBlock),
    /// A branch with variable assignments.
    Branch(AssignedBranch),
}

impl FuncElement {
    /// Construct a root Funcelement from an InstrDef.
    pub fn new(instr: InstrDef) -> Self {
        let assigner = VarAssigner::default();
        let mut stack = StackTracker::new_root(&assigner);
        let assigned = FuncElement::build_assignment(instr.flow(), &mut stack);
        assert!(
            stack.total_bytes() == 0,
            "Stack was not empty at the end of instruction execution."
        );
        assigned
    }

    /// Assign variables for this FuncElement.
    pub fn build_assignment(elem: &Element, parent_stack: &mut StackTracker) -> Self {
        match elem {
            Element::Microcode(microcode) => FuncElement::Microcode(
                AssignedMicrocode::build_assignment(*microcode, parent_stack),
            ),
            Element::Block(block) => {
                FuncElement::Block(AssignedBlock::build_assignment(block, parent_stack))
            }
            Element::Branch(branch) => {
                FuncElement::Branch(AssignedBranch::build_assignment(branch, parent_stack))
            }
        }
    }
}

#[derive(Debug)]
pub struct AssignedMicrocode {
    /// The microcode to execute.
    pub microcode: Microcode,
    /// Type conversions to apply before running the microcode for this operation.
    pub conversions: Vec<Conversion>,
    /// Arguments to the microcode function, with the variables they will be pulled from.
    pub args: Vec<Arg<VarAssignment>>,
    /// Variables that will be assigned by the result.
    pub returns: Vec<VarAssignment>,
}

impl AssignedMicrocode {
    /// Assign variables for this microcode instruction. Result variable assignments will
    /// be applied directly to the parent stack since Microcode does not produce a new
    /// scope.
    fn build_assignment(microcode: Microcode, parent_stack: &mut StackTracker) -> Self {
        let desc = microcode.descriptor();
        let mut conversions = vec![];
        let args = desc
            .args
            .into_iter()
            .map(|arg| match arg {
                Arg::Literal(lit) => Arg::Literal(lit),
                Arg::StackValue(val) => {
                    let (assignment, c) = parent_stack.pop(val);
                    conversions.extend_from_slice(&c);
                    Arg::StackValue(assignment)
                }
            })
            .collect();
        let returns = desc
            .returns
            .into_iter()
            .map(|ret| parent_stack.push(ret))
            .collect();

        Self {
            microcode,
            conversions,
            args,
            returns,
        }
    }
}

/// A "block" of func elements with variable assignments computed. Note that this block
/// does not create a new scope, unlike a Rust `{` block `}`.
#[derive(Debug)]
pub struct AssignedBlock {
    /// Assigned elements.
    pub elements: Vec<FuncElement>,
}

impl AssignedBlock {
    fn build_assignment(block: &Block, parent_stack: &mut StackTracker) -> Self {
        let elements = block
            .elements
            .iter()
            .map(|elem| FuncElement::build_assignment(elem, parent_stack))
            .collect();

        Self { elements }
    }
}

#[derive(Debug)]
pub struct AssignedBranch {
    /// Conversions applied to acquire the branch condition.
    pub cond_conversion: Vec<Conversion>,
    /// Variable which will contain the bool branch condition.
    pub cond: VarId,

    /// Code to execute if the condition is true.
    pub code_if_true: Box<FuncElement>,

    /// Assignments of the pushed values from the true branch. These will be collected
    /// into a tuple and assigned to the `pushes` of the branch in the outer block.
    pub true_returns: Vec<VarAssignment>,

    /// Conversions to apply at the end of the true branch to align its types to the false
    /// branch.
    pub true_end_conv: Vec<Conversion>,

    /// Code to execute if the condition is false.
    pub code_if_false: Box<FuncElement>,

    /// Conversions to apply at the end of the false branch to align its types to the true
    /// branch.
    pub false_end_conv: Vec<Conversion>,

    /// Assignments of the pushed values from the false branch. These will be collected
    /// into a tuple and assigned to the `pushes` of the branch in the outer block.
    pub false_returns: Vec<VarAssignment>,

    /// Values pushed onto the parent's stack in the scope outside of the branch.
    pub pushes: Vec<VarAssignment>,
}

impl AssignedBranch {
    fn build_assignment(branch: &Branch, parent_stack: &mut StackTracker) -> Self {
        let (cond, cond_conversion) = parent_stack.pop(ValType::Bool);

        let mut true_stack = parent_stack.make_child();
        let code_if_true = Box::new(FuncElement::build_assignment(
            &branch.code_if_true,
            &mut true_stack,
        ));

        let mut false_stack = parent_stack.make_child();
        let code_if_false = Box::new(FuncElement::build_assignment(
            &branch.code_if_false,
            &mut false_stack,
        ));

        assert!(
            true_stack.total_bytes() == false_stack.total_bytes(),
            "True and false branches have different byte counts"
        );

        let true_terminal = branch.code_if_true.ends_with_terminal();
        let false_terminal = branch.code_if_false.ends_with_terminal();

        let mut true_end_conv = vec![];
        let mut false_end_conv = vec![];
        let pushes = if true_terminal && false_terminal {
            // Both branches are terminals, so there's no result from the `if` in either
            // case. No need to compute pushes, just check that both stacks are empty.
            assert!(
                true_stack.total_bytes() == 0,
                "Terminal branch has non-empty stack"
            );
            assert!(
                false_stack.total_bytes() == 0,
                "Terminal branch has non-empty stack"
            );
            vec![]
        } else if true_terminal {
            // True branch is a terminal operation, but false is not. Take our pushes from
            // the false stack, and update the parent stack (the result of our operation)
            // from the end state of the parent of the false branch.
            assert!(
                true_stack.total_bytes() == 0,
                "Terminal branch has non-empty stack"
            );
            *parent_stack = *false_stack.parent.unwrap();
            false_stack
                .local_stack
                .iter()
                .map(|asgn| parent_stack.push(asgn.val))
                .collect()
        } else if false_terminal {
            // False branch is a terminal operation, but false is not. Take our pushes from
            // the true stack, and update the parent stack (the result of our operation)
            // from the end state of the parent of the true branch.
            assert!(
                false_stack.total_bytes() == 0,
                "Terminal branch has non-empty stack"
            );
            *parent_stack = *true_stack.parent.unwrap();
            true_stack
                .local_stack
                .iter()
                .map(|asgn| parent_stack.push(asgn.val))
                .collect()
        } else {
            // Both true and false branches continue to further code.

            // Make the true and false branches have the same return length and types.
            if true_stack.local_bytes() < false_stack.local_bytes() {
                // If the true_stack is smaller, pop values off of true_stack matching
                // false_stack's types to do the type conversion and pull more values from the
                // parent.
                let mut true_pushes_rev = Vec::with_capacity(false_stack.local_stack.len());
                for VarAssignment { val, .. } in false_stack.local_stack.iter().rev().copied() {
                    let (var, conv) = true_stack.pop(val);
                    true_end_conv.extend_from_slice(&conv);
                    true_pushes_rev.push(var);
                }
                true_pushes_rev.reverse();
                assert!(true_stack.local_stack.is_empty());
                true_stack.local_stack = true_pushes_rev;
            } else if true_stack.local_bytes() > false_stack.local_bytes() {
                // If the false_stack's local stack is smaller, pop values off the false_stack
                // matching true_stacks' types to do type conversion and pull more values from
                // the parent if needed.
                let mut false_pushes_rev = Vec::with_capacity(true_stack.local_stack.len());
                for VarAssignment { val, .. } in true_stack.local_stack.iter().rev().copied() {
                    let (var, conv) = false_stack.pop(val);
                    false_end_conv.extend_from_slice(&conv);
                    false_pushes_rev.push(var);
                }
                false_pushes_rev.reverse();
                assert!(false_stack.local_stack.is_empty());
                false_stack.local_stack = false_pushes_rev;
            }

            // Check that we've put both parent stacks in the same state.
            assert!(true_stack.parent == false_stack.parent);
            assert!(
                true_stack
                    .local_stack
                    .iter()
                    .map(|asgn| asgn.val)
                    .collect::<Vec<_>>()
                    == false_stack
                        .local_stack
                        .iter()
                        .map(|asgn| asgn.val)
                        .collect::<Vec<_>>()
            );

            *parent_stack = *true_stack.parent.unwrap();

            true_stack
                .local_stack
                .iter()
                .map(|asgn| parent_stack.push(asgn.val))
                .collect()
        };

        Self {
            cond_conversion,
            cond: cond.var,
            code_if_true,
            true_end_conv,
            true_returns: true_stack.local_stack,
            code_if_false,
            false_returns: false_stack.local_stack,
            false_end_conv,
            pushes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::{CBOpcode, InternalFetch, Opcode};

    #[test]
    fn assign_all_instrs() {
        FuncElement::new(InternalFetch.into());
        for opcode in 0..=0xffu8 {
            FuncElement::new(Opcode::decode(opcode).into());
        }
        for cbopcode in 0..=0xffu8 {
            FuncElement::new(CBOpcode::decode(cbopcode).into());
        }
    }
}
