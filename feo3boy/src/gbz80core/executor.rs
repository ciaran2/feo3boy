//! Defines the basic types needed to implement a GBZ80 Exeuctor.
use std::fmt::Debug;

use crate::gbz80core::ExecutorContext;

/// Trait for types which represent the state of a microcode executor.
pub trait ExecutorState: Clone + Debug + Eq + PartialEq {}

impl ExecutorState for () {}

/// Trait for an executor which implements the instructions on the GBZ80 CPU.
pub trait Executor {
    /// Internal state of the executor.
    type State: ExecutorState;

    /// Run a single instruction on the GBZ80 CPU, using `yield1m` on the context whenever
    /// a yield is required.
    fn run_single_instruction(ctx: &mut impl ExecutorContext<State = Self::State>);
}

pub trait ExecutorConfig {
    /// The executor that this configuration is for.
    type Executor: Executor;

    /// Creates the initial state for the executor based on the config.
    fn create_initial_state(&self) -> <Self::Executor as Executor>::State;
}

/// Which type of pause point was reached in a `tick_until_yield_or_fetch`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum PausePoint {
    /// The pause was at a yield.
    Yield,
    /// The pause was at a fetch.
    Fetch,
}

/// An executor which can pause partway through instruction execution.
pub trait SubInstructionExecutor: Executor {
    /// Runs the CPU for one m-cycle, executing until a 'yield' is encountered.
    fn tick(ctx: &mut impl ExecutorContext<State = Self::State>) {
        loop {
            match Self::tick_until_yield_or_fetch(ctx) {
                PausePoint::Yield => break,
                PausePoint::Fetch => continue,
            }
        }
    }

    /// Runs the CPU until either a yield is encountered or the current instruction ends.
    ///
    /// This can be useful when trying to get the CPU to the boundary between
    /// instructions, for example when trying to perform a save-state.
    fn tick_until_yield_or_fetch(ctx: &mut impl ExecutorContext<State = Self::State>)
        -> PausePoint;
}
