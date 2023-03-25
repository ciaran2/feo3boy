//! This module provides utilities for analyzing and manipulating sequences of
//! [`Microcode`][crate::microcode::Microcode] operations in order to transform opcodes
//! into rust code that executes those opcodes.
use proc_macro2::TokenStream;

pub mod args;
pub mod instr;

/// Which type of microcode operation this is.
#[derive(Debug, Clone)]
pub enum OperationType<N> {
    /// Microcode operation is a pure function.
    Function {
        /// Path to the function that defines the microcode operation.
        path: TokenStream,
    },
    /// Microcode operation is externally defined.
    Extern {
        /// Identifies which extern this is.
        name: N,
    },
}
