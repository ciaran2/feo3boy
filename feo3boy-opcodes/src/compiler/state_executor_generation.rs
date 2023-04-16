//! Provides utilities for codegenerating the state-machine executor.
//!
//! Although the types used here are very similar to those used in the direct executor, we
//! don't share types because we need slightly different state types.
//!
//! We also don't share the variable assignment logic because we have slightly different
//! constraints -- every time we yield, any stack nesting gets reset and we resume the
//! next state with all variables in the local stack.

use std::collections::BTreeMap;
use std::iter::FusedIterator;
use std::mem;

use crate::compiler::args::Arg;
use crate::compiler::instr::flow::{Element, ElementPath, ElementPathBuf, ElementSelector};
use crate::compiler::instr::InstrDef;
use crate::compiler::variables::{Conversion, StackTracker, VarAssigner, VarAssignment, VarId};
use crate::microcode::{Microcode, ValType};

/// Elements of a state.
#[derive(Debug)]
pub enum StateElement {
    Microcode(StateMicrocode),
    Block(StateBlock),
    Branch(StateBranch),
    /// Change to the state in InstrStates with the given index.
    ChangeState(StateChange),
}

impl StateElement {
    /// Run `other` after `self`. Transforms `self` into a `Block` if necessary to append
    /// `other` and adds its elements after the current value.
    pub fn extend_block(&mut self, other: StateElement) {
        match (self, other) {
            // self is a block and other is a block, so just append the elements from
            // other onto self.
            (Self::Block(ref mut self_block), Self::Block(mut other_block)) => {
                self_block.elements.append(&mut other_block.elements)
            }
            // self is a block but other is not, so just push other on to this block
            // directly.
            (Self::Block(ref mut self_block), other) => self_block.elements.push(other),
            // other is a block but self is not. Swap self and other, and then insert self
            // at the front of other.
            (this, mut other @ Self::Block(_)) => {
                mem::swap(this, &mut other);
                match this {
                    Self::Block(ref mut old_other_block) => {
                        old_other_block
                            .elements
                            .insert(0, /* used to be self */ other)
                    }
                    _ => unreachable!(),
                }
            }
            // Neither self nor other are blocks. Create a new block and append both.
            (this, other) => {
                let old_self = mem::replace(
                    this,
                    StateElement::Block(StateBlock {
                        elements: Vec::with_capacity(2),
                    }),
                );
                match this {
                    Self::Block(ref mut self_block) => {
                        self_block.elements.push(old_self);
                        self_block.elements.push(other);
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Returns true if every path out of this element ends in a state-change or a
    /// terminal operation.
    ///
    /// Only checks the end of a block, so if a state transition or terminal is
    /// accidentally placed in the middle of a block, this won't find it.
    pub fn ends_in_state_change_or_terminal(&self) -> bool {
        match self {
            StateElement::Microcode(code) => code.microcode.is_terminal(),
            StateElement::Block(StateBlock { elements }) => match elements.last() {
                Some(last) => last.ends_in_state_change_or_terminal(),
                None => false,
            },
            StateElement::Branch(StateBranch {
                code_if_true,
                code_if_false,
                ..
            }) => {
                // If either branch doesn't result in a state-change, we say we can still
                // continue.
                code_if_true.ends_in_state_change_or_terminal()
                    && code_if_false.ends_in_state_change_or_terminal()
            }
            StateElement::ChangeState(_) => true,
        }
    }
}

impl Default for StateElement {
    fn default() -> Self {
        StateElement::Block(Default::default())
    }
}

/// Microcode with required type conversions and variable assignments provided.
#[derive(Debug)]
pub struct StateMicrocode {
    /// The microcode to execute.
    pub microcode: Microcode,
    /// Type conversions to apply before running the microcode for this operation.
    pub conversions: Vec<Conversion>,
    /// Arguments to the microcode function, with the variables they will be pulled from.
    pub args: Vec<Arg<VarAssignment>>,
    /// Variables that will be assigned by the result.
    pub returns: Vec<VarAssignment>,
}

impl StateMicrocode {
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

/// A block of state elements with variable assignments computed. Note that this block
/// does not create a new scope, unlike a Rust `{` block `}`.
#[derive(Debug, Default)]
pub struct StateBlock {
    /// Assigned elements.
    pub elements: Vec<StateElement>,
}

/// A branch between two state elements with variable assignments computed.
#[derive(Debug)]
pub struct StateBranch {
    /// Conversions applied to acquire the branch condition.
    pub cond_conversion: Vec<Conversion>,
    /// Variable which will contain the bool branch condition.
    pub cond: VarId,

    /// Code to execute if the condition is true.
    pub code_if_true: Box<StateElement>,

    /// Assignments of the pushed values from the true branch. These will be collected
    /// into a tuple and assigned to the `pushes` of the branch in the outer block.
    pub true_returns: Vec<VarAssignment>,

    /// Conversions to apply at the end of the true branch to align its types to the false
    /// branch.
    pub true_end_conv: Vec<Conversion>,

    /// Code to execute if the condition is false.
    pub code_if_false: Box<StateElement>,

    /// Conversions to apply at the end of the false branch to align its types to the true
    /// branch.
    pub false_end_conv: Vec<Conversion>,

    /// Assignments of the pushed values from the false branch. These will be collected
    /// into a tuple and assigned to the `pushes` of the branch in the outer block.
    pub false_returns: Vec<VarAssignment>,

    /// Values pushed onto the parent's stack in the scope outside of the branch.
    pub pushes: Vec<VarAssignment>,
}

/// Describes the target of a state change.
#[derive(Debug, Clone)]
pub struct StateChange {
    /// Path that the next state begins at.
    pub next_state: ElementPathBuf,
    /// Stack values saved by this state.
    pub saved_vars: Vec<VarAssignment>,
}

/// Describes a single state in an instruction.
#[derive(Debug)]
pub struct State {
    /// Sequential state number of this state.
    pub num: usize,
    /// Variables which are stored coming into this state.
    pub stored_stack: Vec<VarAssignment>,
    /// Element which defines the code to run in this state.
    pub element: StateElement,
}

/// The full set of states for an instruction.
pub struct InstrStates {
    /// Collection of states used in a single instruction.
    states: BTreeMap<ElementPathBuf, State>,
}

impl InstrStates {
    /// Build InstrStates for the given [`InstrDef`].
    pub fn new(instr: &InstrDef) -> Self {
        Self::from_element(instr.flow())
    }

    /// Break the given root element down into a set of states.
    fn from_element(root: &Element) -> Self {
        let mut states: BTreeMap<ElementPathBuf, State> = Default::default();

        let mut traversal = vec![StateChange {
            next_state: ElementPathBuf::new(),
            saved_vars: Vec::new(),
        }];

        let assigner = VarAssigner::default();

        while let Some(prev_transition) = traversal.pop() {
            if states.contains_key(&prev_transition.next_state) {
                continue;
            }

            let mut stack =
                StackTracker::root_with_stack(&assigner, prev_transition.saved_vars.clone());
            let element = build_state_from(
                prev_transition.next_state.clone(),
                root,
                &mut stack,
                &mut traversal,
            );
            let state = State {
                num: states.len(),
                stored_stack: prev_transition.saved_vars,
                element,
            };

            states.insert(prev_transition.next_state, state);
        }

        Self { states }
    }

    /// Get a state based on the path where that state starts.
    #[inline]
    pub fn get_state(&self, path: &ElementPath) -> Option<&State> {
        self.states.get(path)
    }

    /// Get an iterator over the paths where the various states start.
    pub fn state_starts(
        &self,
    ) -> impl Iterator<Item = &ElementPath> + DoubleEndedIterator + ExactSizeIterator + FusedIterator
    {
        self.states.keys().map(|path| path.as_path())
    }

    /// Get an iterator over all of the states along with their starting paths.
    pub fn states(
        &self,
    ) -> impl Iterator<Item = (&ElementPath, &State)>
           + DoubleEndedIterator
           + ExactSizeIterator
           + FusedIterator {
        self.states.iter().map(|(k, v)| (k.as_path(), v))
    }

    /// Consume this InstrStates producing an iterator over the paths and states.
    #[inline]
    pub fn into_states(self) -> <BTreeMap<ElementPathBuf, State> as IntoIterator>::IntoIter {
        self.states.into_iter()
    }
}

impl IntoIterator for InstrStates {
    type IntoIter = <BTreeMap<ElementPathBuf, State> as IntoIterator>::IntoIter;
    type Item = <BTreeMap<ElementPathBuf, State> as IntoIterator>::Item;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.into_states()
    }
}

/// Construct a new state starting from the given path. Any time a state transition is
/// encountered, push the transition onto the `traversal` stack.
///
/// This operation can start in the middle of a Branch and will linearize the branch into
/// its parent block.
fn build_state_from(
    mut path: ElementPathBuf,
    root: &Element,
    parent_stack: &mut StackTracker,
    traversal: &mut Vec<StateChange>,
) -> StateElement {
    let mut state = StateElement::default();
    loop {
        // We don't need to worry about traversing down into children, as build_state_only
        // will handle that. All this method needs to do is handle traversing back into
        // parents whenever we operate from an index which is the middle of a branch.
        state.extend_block(build_state_only(&path, root, parent_stack, traversal));
        // If we've reached a point where all paths lead to terminals, stop.
        if state.ends_in_state_change_or_terminal() {
            break;
        }
        path = next_in_block_or_parent(path, root)
            .expect("Hit end of root element without a terminal");
    }

    state
}

/// Build out a state consisting only of the element at `path` and its children. Never
/// jump back to a parent. If new states are encountered, they are pushed to the
/// `traversal`.
fn build_state_only(
    path: &ElementPath,
    root: &Element,
    parent_stack: &mut StackTracker,
    traversal: &mut Vec<StateChange>,
) -> StateElement {
    match &root[path] {
        Element::Microcode(Microcode::Yield) => {
            let next_state = next_in_block_or_parent(path.to_buf(), root)
                .expect("Yield found at end of instruction");
            let state_change = StateChange {
                next_state,
                saved_vars: parent_stack.snapshot(),
            };
            traversal.push(state_change.clone());
            StateElement::ChangeState(state_change)
        }
        // Normal microcode: compute assignments for this instruction.
        Element::Microcode(code) => {
            StateElement::Microcode(StateMicrocode::build_assignment(*code, parent_stack))
        }
        Element::Block(block) => {
            let mut child_path = path.to_buf();
            child_path.push(0);

            // For blocks, process as long as the block doesn't hit a state change or
            // terminal.
            let mut elements = Vec::with_capacity(block.elements.len());
            for idx in 0..block.elements.len() {
                *child_path.last_mut().unwrap() = ElementSelector::Block(idx);
                let element = build_state_only(&child_path, root, parent_stack, traversal);
                let stop = element.ends_in_state_change_or_terminal();
                elements.push(element);
                if stop {
                    break;
                }
            }
            StateElement::Block(StateBlock { elements })
        }
        Element::Branch(_) => {
            let (cond, cond_conversion) = parent_stack.pop(ValType::Bool);

            let mut child_path = path.to_buf();
            child_path.push(true);

            let mut true_stack = parent_stack.make_child();
            let code_if_true = Box::new(build_state_only(
                &child_path,
                root,
                &mut true_stack,
                traversal,
            ));

            *child_path.last_mut().unwrap() = ElementSelector::Branch(false);
            let mut false_stack = parent_stack.make_child();
            let code_if_false = Box::new(build_state_only(
                &child_path,
                root,
                &mut false_stack,
                traversal,
            ));

            let true_terminal_or_yield = code_if_true.ends_in_state_change_or_terminal();
            let false_terminal_or_yield = code_if_false.ends_in_state_change_or_terminal();

            let mut true_end_conv = vec![];
            let mut false_end_conv = vec![];

            let pushes = if true_terminal_or_yield && false_terminal_or_yield {
                // Both branches are terminal or yield, so don't push anything to the
                // parent stack. Also clear the local stacks so they don't try to return
                // anything. (Unlike in the direct executor, there may still be things on
                // the stack, but those things get stored in the new state on a yield).
                true_stack.local_stack.clear();
                false_stack.local_stack.clear();
                vec![]
            } else if true_terminal_or_yield {
                // True branch is a terminal operation or yield, but false is not. Take
                // our pushes from the false stack, and update the parent stack (the
                // result of our operation) from the end state of the parent of the false
                // branch.
                *parent_stack = *false_stack.parent.unwrap();
                // Also clear the local stack of true so it doesn't try to return
                // anything. (Unlike in the direct executor, there may still be things on
                // the stack, but those things get stored in the new state on a yield).
                true_stack.local_stack.clear();
                false_stack
                    .local_stack
                    .iter()
                    .map(|asgn| parent_stack.push(asgn.val))
                    .collect()
            } else if false_terminal_or_yield {
                // False branch is a terminal operation or yield, but true is not. Take
                // our pushes from the true stack, and update the parent stack (the result
                // of our operation) from the end state of the parent of the true branch.
                *parent_stack = *true_stack.parent.unwrap();
                // Also clear the local stack of false so it doesn't try to return
                // anything. (Unlike in the direct executor, there may still be things on
                // the stack, but those things get stored in the new state on a yield).
                false_stack.local_stack.clear();

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

            StateElement::Branch(StateBranch {
                cond_conversion,
                cond: cond.var,
                code_if_true,
                true_end_conv,
                true_returns: true_stack.local_stack,
                code_if_false,
                false_end_conv,
                false_returns: false_stack.local_stack,
                pushes,
            })
        }
    }
}

/// Find the path where the next state begins after the specified path.
fn next_in_block_or_parent(mut path: ElementPathBuf, root: &Element) -> Option<ElementPathBuf> {
    // `path` points to a particular element, but what we are actually incrementing
    // relative to is the parent. Basically we are saying "when `path` is done, what comes
    // next?" This means that if path points to an entire block, we should go to the next
    // whole block in the parent.
    //
    // We need to know what the parent is because the parent will tell us if we are going
    // out of bounds or not.
    //
    // If this path is already the root path, we are done, since nothing comes after the
    // root.
    match &root[path.parent()?] {
        Element::Microcode(_) => panic!(
            "Microcode instructions don't have children. Previous path must have been invalid."
        ),
        // If the parent is a block, try to increment the last element of the path.
        // path must have a `last` because it has a parent.
        Element::Block(block) => {
            if path.last().unwrap().unwrap_block() + 1 < block.elements.len() {
                *path.last_mut().unwrap().unwrap_block_mut() += 1;
                Some(path)
            } else {
                path.pop().unwrap();
                next_in_block_or_parent(path, root)
            }
        }
        // For a branch, we want the code after the branch, so just check the next level
        // of parent.
        Element::Branch(_) => {
            path.pop().unwrap();
            next_in_block_or_parent(path, root)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::{CBOpcode, InternalFetch, Opcode};

    #[test]
    fn assign_all_instrs() {
        InstrStates::new(&InternalFetch.into());
        for opcode in 0..=0xffu8 {
            InstrStates::new(&Opcode::decode(opcode).into());
        }
        for cbopcode in 0..=0xffu8 {
            InstrStates::new(&CBOpcode::decode(cbopcode).into());
        }
    }
}
