//! Provides utilities for working with args to the microcode.

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

/// Helper for converting a type to a literal representation.
pub trait AsLiteral {
    /// Produces a literal representation of this value, which could be used in code
    /// generation to bake the value into source code.
    fn as_literal(&self) -> Literal;
}

impl AsLiteral for u8 {
    fn as_literal(&self) -> Literal {
        proc_macro2::Literal::u8_suffixed(*self)
            .into_token_stream()
            .into()
    }
}

impl AsLiteral for u16 {
    fn as_literal(&self) -> Literal {
        proc_macro2::Literal::u16_suffixed(*self)
            .into_token_stream()
            .into()
    }
}

impl AsLiteral for usize {
    fn as_literal(&self) -> Literal {
        proc_macro2::Literal::usize_suffixed(*self)
            .into_token_stream()
            .into()
    }
}

impl AsLiteral for bool {
    fn as_literal(&self) -> Literal {
        if *self {
            quote! { true }
        } else {
            quote! { false }
        }
        .into()
    }
}

/// Literal representation of a value from a microcode field, which can be interpolated
/// into code to produce the same value.
#[derive(Debug, Clone)]
pub struct Literal(TokenStream);

impl From<TokenStream> for Literal {
    fn from(value: TokenStream) -> Self {
        Self(value)
    }
}

impl ToTokens for Literal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens)
    }

    fn into_token_stream(self) -> TokenStream {
        self.0
    }

    fn to_token_stream(&self) -> TokenStream {
        self.0.clone()
    }
}

/// An argument to a microcode operator.
#[derive(Debug, Clone)]
pub enum Arg<StackT> {
    /// A value popped off of the stack.
    StackValue(StackT),
    /// A literal value.
    Literal(Literal),
}
