//! Types for managing variables in generated executor implementations.

use std::sync::atomic::{AtomicUsize, Ordering};

use quote::{format_ident, ToTokens};

use crate::microcode::ValType;

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

/// Generate a variable name from this id.
impl ToTokens for VarId {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        format_ident!("val{}", self.0).to_tokens(tokens)
    }
}

/// Tracks the stack of assignments.
#[derive(Clone)]
pub struct StackTracker<'a> {
    /// Stack from the parent.
    pub parent: Option<Box<StackTracker<'a>>>,
    /// Variable assigner used for this stack.
    assigner: &'a VarAssigner,
    /// Variables assigned in the local space.
    pub local_stack: Vec<VarAssignment>,
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
    pub fn make_child(&self) -> Self {
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
    pub fn pop(&mut self, val: ValType) -> (VarAssignment, Vec<Conversion>) {
        let (assignment, conversions, extra) = self.pop_with_extra(val);
        if let Some(extra) = extra {
            self.local_stack.push(extra);
        }
        (assignment, conversions)
    }

    /// Push a value onto the local stack, returning the variable that it will be assigned
    /// to.
    pub fn push(&mut self, val: ValType) -> VarAssignment {
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
    pub var: VarId,
    /// Type of the value assigned to that variable ID.
    pub val: ValType,
}
