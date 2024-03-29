//! Defines types for instruction control flow. This makes it easer to deal with
//! instruction code flow than directly using Skip/SkipIf.

use std::mem;
use std::ops::{Index, IndexMut};

use crate::microcode::Microcode;

pub use path::{ElementIndex, ElementPath, ElementPathBuf, ElementSelector};

mod path;

/// Defines elements that make up the code flow of an instruction.
#[derive(Debug, Clone)]
pub enum Element {
    /// Execute this single microcode instruction. The instruction must not be `Skip` or
    /// `SkipIf`.
    Microcode(Microcode),
    /// Execute a sequence of elements in order.
    Block(Block),
    /// Branch and execute one of two sequences of elements.
    Branch(Branch),
}

impl Element {
    /// Convert this Element to a flat list of microcode instructions.
    pub fn flatten(&self) -> Vec<Microcode> {
        match self {
            Self::Microcode(microcode) => vec![*microcode],
            Self::Block(block) => block.flatten(),
            Self::Branch(branch) => branch.flatten(),
        }
    }

    /// Run `other` after `self`. Transforms `self` into a `Block` if necessary to append
    /// `other` and adds its elements after the current value.
    pub fn extend_block(&mut self, other: Element) {
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
                    Element::Block(Block {
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

    /// Return true if the given element ends with FetchNextInstruction, ParseOpcode, or
    /// ParseCBOpcode. For Branches, only returns true if all branches end with one of
    /// those opcodes.
    pub fn ends_with_terminal(&self) -> bool {
        match self {
            Element::Microcode(microcode) => microcode.is_terminal(),
            Element::Block(block) => match block.elements.last() {
                Some(last) => last.ends_with_terminal(),
                None => false,
            },
            Element::Branch(Branch {
                code_if_true,
                code_if_false,
            }) => code_if_true.ends_with_terminal() && code_if_false.ends_with_terminal(),
        }
    }

    /// Get an element by index relative to self.
    pub fn get<I>(&self, index: I) -> Option<&Element>
    where
        I: ElementIndex,
    {
        let mut elem = self;
        for selector in index.path_selectors() {
            match elem {
                Element::Microcode(_) => panic!("Cannot index into Microcode elements"),
                Element::Block(block) => elem = block.get(selector)?,
                Element::Branch(branch) => elem = branch.get(selector),
            }
        }
        Some(elem)
    }

    /// Get an element by index relative to self.
    pub fn get_mut<I>(&mut self, index: I) -> Option<&mut Element>
    where
        I: ElementIndex,
    {
        let mut elem = self;
        for selector in index.path_selectors() {
            match elem {
                Element::Microcode(_) => panic!("Cannot index into Microcode elements"),
                Element::Block(block) => elem = block.get_mut(selector)?,
                Element::Branch(branch) => elem = branch.get_mut(selector),
            }
        }
        Some(elem)
    }
}

impl Default for Element {
    /// Default Element is an empty block.
    #[inline]
    fn default() -> Self {
        Self::Block(Default::default())
    }
}

impl<I: ElementIndex> Index<I> for Element {
    type Output = Element;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        self.get(index).expect("Index out of Bounds")
    }
}

impl<I: ElementIndex> IndexMut<I> for Element {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.get_mut(index).expect("Index out of Bounds")
    }
}

/// A block of elements.
#[derive(Default, Debug, Clone)]
pub struct Block {
    /// Elements to execute.
    pub elements: Vec<Element>,
}

impl Block {
    /// Flatten this block into a list of microcode instructions.
    pub fn flatten(&self) -> Vec<Microcode> {
        self.elements
            .iter()
            .flat_map(|elem| elem.flatten().into_iter())
            .collect()
    }

    /// Get the child [`Element`] matching the given selector, or None if the index is out
    /// of bounds. Panics if given a `Branch` selector.
    pub fn get(&self, selector: ElementSelector) -> Option<&Element> {
        match selector {
            ElementSelector::Block(idx) => self.elements.get(idx),
            ElementSelector::Branch(_) => panic!("Cannot index a Block with a Branch selector"),
        }
    }

    /// Get the child [`Element`] matching the given selector, or None if the index is out
    /// of bounds. Panics if given a `Branch` selector.
    pub fn get_mut(&mut self, selector: ElementSelector) -> Option<&mut Element> {
        match selector {
            ElementSelector::Block(idx) => self.elements.get_mut(idx),
            ElementSelector::Branch(_) => panic!("Cannot index a Block with a Branch selector"),
        }
    }
}

/// Execute one element if the condition value is true and the other if the condition
/// value is false.
#[derive(Debug, Clone)]
pub struct Branch {
    /// Code to execute if the condition is true.
    pub code_if_true: Box<Element>,
    /// Code to execute if the condition is false.
    pub code_if_false: Box<Element>,
}

impl Branch {
    /// Flatten this branch into a list of microcode instructions.
    pub fn flatten(&self) -> Vec<Microcode> {
        let code_if_true = self.code_if_true.flatten();
        let code_if_false = self.code_if_false.flatten();

        // Both branches are empty, so the branch condition is irrelevant, just discard
        // it. We still need to pop the condition off the stack though for consistency.
        if code_if_true.is_empty() && code_if_false.is_empty() {
            vec![Microcode::Discard8]
        } else if code_if_true.is_empty() {
            let steps = code_if_false.len();
            let mut res = code_if_false;
            res.insert(0, Microcode::SkipIf { steps });
            res
        } else if code_if_false.is_empty() {
            let steps = code_if_true.len();
            let mut res = code_if_true;
            res.splice(0..0, [Microcode::Not, Microcode::SkipIf { steps }]);
            res
        } else {
            let true_steps = code_if_true.len();
            // Add one because we will insert a skip instruction at the end to skip over
            // the true portion.
            let false_steps = code_if_false.len() + 1;
            let mut res = code_if_false;
            res.insert(0, Microcode::SkipIf { steps: false_steps });
            res.push(Microcode::Skip { steps: true_steps });
            res.extend_from_slice(&code_if_true);
            res
        }
    }

    /// Get the child [`Element`] matching the given selector. Panics if given a `Block` selector.
    pub fn get(&self, selector: ElementSelector) -> &Element {
        match selector {
            ElementSelector::Block(_) => panic!("Cannot index a Branch with a Block selector"),
            ElementSelector::Branch(true) => &self.code_if_true,
            ElementSelector::Branch(false) => &self.code_if_false,
        }
    }

    /// Get the child [`Element`] matching the given selector, or None if the index is out
    /// of bounds. Panics if given a `Branch` selector.
    pub fn get_mut(&mut self, selector: ElementSelector) -> &mut Element {
        match selector {
            ElementSelector::Block(_) => panic!("Cannot index a Branch with a Block selector"),
            ElementSelector::Branch(true) => &mut self.code_if_true,
            ElementSelector::Branch(false) => &mut self.code_if_false,
        }
    }
}
