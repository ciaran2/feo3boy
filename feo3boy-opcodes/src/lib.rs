//! Defines the opcodes for the `feo3boy` gameboy.
//!
//! This library is intended to provide the basic opcodes of the gbz80 CPU. Opcodes are
//! defined in terms of a microcode which allows instructions to be broken into smaller
//! steps.
//!
//! The [`microcode`] module supplies the [`Microcode`][microcode::Microcode] type, which
//! defines the steps that opcodes can be broken down into.
//!
//! The [`opcode`] module supplies [`Opcode`][opcode::Opcode] and
//! [`CBOpcode`][opcode::CBOpcode] which define the actual instructions the GameBoy CPU
//! can execute.

pub mod compiler;
pub mod gbz80types;
pub mod microcode;
pub mod opcode;

/// Helper function for getting the size of an array as a constant by reference. This is
/// used to implement `count_repetition`.
#[doc(hidden)]
pub const fn ___array_size_helper<T, const N: usize>(_: &[T; N]) -> usize {
    N
}

/// Counts the number of repetitions in a macro invocation. Produces a constant
/// expression.
///
/// Usage:
/// ```
/// # use feo3boy_opcodes::count_repetition;
/// count_repetition!(a, 3, (), {}, 4, foo);
/// ```
#[macro_export]
macro_rules! count_repetition {
    () => { 0 };
    ($($rep:tt),* $(,)?) => {
        $crate::___array_size_helper(&[
            $(count_repetition!(@__replace $rep)),*
        ])
    };

    (@__replace $v:tt) => { () };
}
