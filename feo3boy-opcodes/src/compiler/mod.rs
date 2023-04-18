//! This module provides utilities for analyzing and manipulating sequences of
//! [`Microcode`][crate::microcode::Microcode] operations in order to transform opcodes
//! into rust code that executes those opcodes.
use proc_macro2::TokenStream;

pub mod args;
pub mod direct_executor_generation;
pub mod instr;
pub mod variables;

/// Which type of microcode operation this is.
#[derive(Debug, Clone)]
pub enum OperationType {
    /// Microcode operation is a pure function.
    Function {
        /// Path to the function that defines the microcode operation, relative to the
        /// module that the Microcode type is defined in.
        path: TokenStream,
    },
    /// Microcode operation is externally defined.
    Extern,
}
