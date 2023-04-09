//! Provides tools for generating the Direct Executor.

use std::sync::atomic::{AtomicUsize, Ordering};

use crate::compiler::args::Arg;
use crate::compiler::instr::flow::{Block, Branch, Element};
use crate::microcode::{Microcode, ValType};

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

/// Type conversion operation. Enumerates the allowed type conversions.
#[derive(Debug, Copy, Clone)]
pub enum Conversion {
    /// Conversion between types which are the same size.
    SameSize {
        from: VarAssignment,
        to: VarAssignment,
    },
    /// Conversion from a U16 to two U8.
    Split {
        from: VarId,
        low: VarId,
        high: VarId,
    },
    /// Conversion from two one-byte values to a single two-byte value.
    Merge { low: VarId, high: VarId, to: VarId },
}

#[derive(Default)]
pub struct VarAssigner {
    next_var: AtomicUsize,
}

impl VarAssigner {
    /// Get the next variable assignment.
    fn take_next(&self) -> VarId {
        VarId(self.next_var.fetch_add(1, Ordering::Relaxed))
    }

    /// Create an assignment with the specified type and the next available variable ID.
    fn assign(&self, val: ValType) -> VarAssignment {
        VarAssignment {
            var: self.take_next(),
            val,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
/// ID of a variable.
pub struct VarId(usize);

/// Tracks the stack of assignments.
#[derive(Clone)]
pub struct StackTracker<'a> {
    /// Stack from the parent.
    parent: Option<Box<StackTracker<'a>>>,
    /// Variable assigner used for this stack.
    assigner: &'a VarAssigner,
    /// Variables assigned in the local space.
    local_stack: Vec<VarAssignment>,
}

impl<'a> PartialEq for StackTracker<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.parent == other.parent
            && self.assigner as *const _ == other.assigner as *const _
            && self.local_stack == other.local_stack
    }
}

impl<'a> StackTracker<'a> {
    /// Create a new root StackTracker.
    pub fn new_root(assigner: &'a VarAssigner) -> Self {
        Self {
            parent: None,
            assigner,
            local_stack: Vec::new(),
        }
    }

    /// Create a stack tracker with a clone of this one as its parent.
    fn make_child(&self) -> Self {
        Self {
            parent: Some(Box::new(self.clone())),
            assigner: self.assigner,
            local_stack: vec![],
        }
    }

    /// Remove a value of the specified type from this stack, returning both the variable
    /// assignment corresponding to the value as well as any conversions required to get
    /// the value, in the order they must be performed. Any excess bytes that had to be
    /// popped from the parent's stack to get down to the correct type but aren't used in
    /// this value are pushed to the local stack.
    fn pop(&mut self, val: ValType) -> (VarAssignment, Vec<Conversion>) {
        let (assignment, conversions, extra) = self.pop_with_extra(val);
        if let Some(extra) = extra {
            self.local_stack.push(extra);
        }
        (assignment, conversions)
    }

    /// Push a value onto the local stack, returning the variable that it will be assigned
    /// to.
    fn push(&mut self, val: ValType) -> VarAssignment {
        let assignment = self.assigner.assign(val);
        self.local_stack.push(assignment);
        assignment
    }

    /// Pop a value from the local stack and parent stacks, returning the assignment for
    /// the popped value, conversions needed to reach the correct type, any excess byte
    /// that had to be popped for type conversion, which sould be pushed to the local
    /// stack at the top level that requested the pop.
    fn pop_with_extra(
        &mut self,
        val: ValType,
    ) -> (VarAssignment, Vec<Conversion>, Option<VarAssignment>) {
        // No need to reach into the parent's stack for this one.
        if val.bytes() <= self.local_bytes() {
            let top = self.local_stack.pop().unwrap();
            if top.val.bytes() == val.bytes() {
                if top.val == val {
                    (top, vec![], None)
                } else {
                    let new_assignment = self.assigner.assign(val);
                    (
                        new_assignment,
                        vec![Conversion::SameSize {
                            from: top,
                            to: new_assignment,
                        }],
                        None,
                    )
                }
            } else if top.val.bytes() < val.bytes() {
                assert!(val == ValType::U16);
                assert!(top.val.bytes() == 1);
                let second = self.local_stack.pop().unwrap();
                let mut conversions = vec![];
                let high = if top.val == ValType::U8 {
                    top
                } else {
                    let converted_top = self.assigner.assign(ValType::U8);
                    conversions.push(Conversion::SameSize {
                        from: top,
                        to: converted_top,
                    });
                    converted_top
                };

                let (low, extra) = match second.val {
                    ValType::Bool | ValType::Flags => {
                        let converted_second = self.assigner.assign(ValType::U8);
                        conversions.push(Conversion::SameSize {
                            from: top,
                            to: converted_second,
                        });
                        (converted_second, None)
                    }
                    ValType::U8 => (second, None),
                    ValType::U16 => {
                        let second_low = self.assigner.assign(ValType::U8);
                        let second_high = self.assigner.assign(ValType::U8);
                        conversions.push(Conversion::Split {
                            from: second.var,
                            low: second_low.var,
                            high: second_high.var,
                        });
                        (second_high, Some(second_low))
                    }
                };

                let final_assignment = self.assigner.assign(ValType::U16);
                conversions.push(Conversion::Merge {
                    low: low.var,
                    high: high.var,
                    to: final_assignment.var,
                });

                (final_assignment, conversions, extra)
            } else {
                assert!(top.val == ValType::U16);
                assert!(val.bytes() == 1);
                let mut conversions = Vec::with_capacity(2);
                let low = self.assigner.assign(ValType::U8);
                let high = self.assigner.assign(ValType::U8);
                conversions.push(Conversion::Split {
                    from: top.var,
                    low: low.var,
                    high: high.var,
                });
                // We need another step to go from U8 to the correct type.
                let final_assignment = if val == ValType::U8 {
                    high
                } else {
                    let assignment = self.assigner.assign(val);
                    conversions.push(Conversion::SameSize {
                        from: high,
                        to: assignment,
                    });
                    assignment
                };

                (final_assignment, conversions, Some(low))
            }
        } else if let Some(parent) = self.parent.as_deref_mut() {
            // We need to retrieve all of the bytes for the operation from the parent.
            if self.local_stack.is_empty() {
                parent.pop_with_extra(val)
            } else {
                let top = self.local_stack.pop().unwrap();
                assert!(top.val.bytes() == 1);
                assert!(val == ValType::U16);
                let mut conversions = vec![];
                let high = if top.val == ValType::U8 {
                    top
                } else {
                    let high = self.assigner.assign(ValType::U8);
                    conversions.push(Conversion::SameSize {
                        from: top,
                        to: high,
                    });
                    high
                };

                let (low, parent_conversions, extra) = parent.pop_with_extra(ValType::U8);
                conversions.extend_from_slice(&parent_conversions);

                let final_assignment = self.assigner.assign(ValType::U16);
                conversions.push(Conversion::Merge {
                    low: low.var,
                    high: high.var,
                    to: final_assignment.var,
                });

                (final_assignment, conversions, extra)
            }
        } else {
            panic!("Stack underflow -- not enough bytes at this level, and no parent available");
        }
    }

    /// Get the number of bytes in the local portion of the stack.
    pub fn local_bytes(&self) -> usize {
        self.local_stack
            .iter()
            .map(|assignment| assignment.val.bytes())
            .sum()
    }

    /// Get the total number of bytes in the stack including the local stack and the
    /// parent's stack.
    pub fn total_bytes(&self) -> usize {
        self.local_bytes()
            + self
                .parent
                .as_deref()
                .map(|p| p.total_bytes())
                .unwrap_or_default()
    }
}

/// A variable assignment number and the type of the microcode value held there.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VarAssignment {
    /// ID of the variable that is assigned.
    var: VarId,
    /// Type of the value assigned to that variable ID.
    val: ValType,
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

        let mut true_end_conv = vec![];
        let mut false_end_conv = vec![];
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

        let pushes = true_stack
            .local_stack
            .iter()
            .map(|asgn| parent_stack.push(asgn.val))
            .collect();

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
    use crate::compiler::instr::InstrDef;
    use crate::opcode::{CBOpcode, InternalFetch, Opcode};

    #[test]
    fn assign_all_instrs() {
        {
            let instr: InstrDef = InternalFetch.into();
            let assigner = VarAssigner::default();
            let mut stack = StackTracker::new_root(&assigner);
            FuncElement::build_assignment(instr.flow(), &mut stack);
            assert_eq!(stack.total_bytes(), 0);
        }
        for opcode in 0..=0xffu8 {
            let instr: InstrDef = Opcode::decode(opcode).into();
            let assigner = VarAssigner::default();
            let mut stack = StackTracker::new_root(&assigner);
            FuncElement::build_assignment(instr.flow(), &mut stack);
            assert_eq!(stack.total_bytes(), 0);
        }
        for cbopcode in 0..=0xffu8 {
            let instr: InstrDef = CBOpcode::decode(cbopcode).into();
            let assigner = VarAssigner::default();
            let mut stack = StackTracker::new_root(&assigner);
            FuncElement::build_assignment(instr.flow(), &mut stack);
            assert_eq!(stack.total_bytes(), 0);
        }
    }
}
