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
