//! Provides utilities for working with args to the microcode.

use std::any::{Any, TypeId};
use std::fmt;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

/// Function type used to look up crate names for literal values.
pub type CrateFetcher = fn(&str) -> TokenStream;

/// Helper for converting a type to a literal representation.
pub trait Literal: fmt::Debug + Any {
    /// Generate output representing this literal as a stream of tokens. Takes a function
    /// that can be used to look up crate names.
    fn constant_value(&self, crates: CrateFetcher) -> TokenStream;
}

impl Literal for u8 {
    fn constant_value(&self, _crates: CrateFetcher) -> TokenStream {
        proc_macro2::Literal::u8_suffixed(*self).into_token_stream()
    }
}

impl Literal for u16 {
    fn constant_value(&self, _crates: CrateFetcher) -> TokenStream {
        proc_macro2::Literal::u16_suffixed(*self).into_token_stream()
    }
}

impl Literal for usize {
    fn constant_value(&self, _crates: CrateFetcher) -> TokenStream {
        proc_macro2::Literal::usize_suffixed(*self).into_token_stream()
    }
}

impl Literal for bool {
    fn constant_value(&self, _crates: CrateFetcher) -> TokenStream {
        if *self {
            quote! { true }
        } else {
            quote! { false }
        }
    }
}

/// A literal with object-safe clone-ability.
pub struct DynLiteral {
    literal: Box<dyn Literal>,
    clone: fn(&dyn Literal) -> Box<dyn Literal>,
}

impl DynLiteral {
    pub fn new<L: Literal + Clone>(val: L) -> Self {
        Self {
            literal: Box::new(val),
            clone: Self::dyn_clone::<L>,
        }
    }

    // Helper function to clone the contained value. Safety relies on DynLiteral always
    // using the dyn_clone type corresponding to the real contained type.
    fn dyn_clone<L: Literal + Clone>(val: &dyn Literal) -> Box<dyn Literal> {
        assert!(TypeId::of::<L>() == val.type_id(), "Wrong type");
        // Change the pointer type back to type L. This is safe because we verified the
        // TypeId.
        let val = val as *const dyn Literal as *const L;
        // Deref the pointer to get a regular reference. This is safe because we got this
        // pointer from a & reference to the value, so it is definitely non-null, and we
        // verified the TypeId.
        let val = unsafe { &*val };
        Box::new(val.clone())
    }
}

impl Clone for DynLiteral {
    fn clone(&self) -> Self {
        let cloned = (self.clone)(self.literal.as_ref());
        Self {
            literal: cloned,
            clone: self.clone,
        }
    }
}

impl fmt::Debug for DynLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.literal.fmt(f)
    }
}

impl Literal for DynLiteral {
    fn constant_value(&self, crates: CrateFetcher) -> TokenStream {
        self.literal.constant_value(crates)
    }
}

/// An argument to a microcode operator.
#[derive(Debug, Clone)]
pub enum Arg<StackT> {
    /// A value popped off of the stack.
    StackValue(StackT),
    /// A literal value.
    Literal(DynLiteral),
}
