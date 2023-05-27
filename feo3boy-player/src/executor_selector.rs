//! Defines types for selecting between different executors.
use std::marker::PhantomData;
use std::mem;

use clap::ValueEnum;
use feo3boy::clock::{SystemClock, SystemClockContext};
use feo3boy::gbz80core::direct_executor::DirectExecutor;
use feo3boy::gbz80core::direct_executor_v2::DirectExecutorV2;
use feo3boy::gbz80core::executor::{Executor, ExecutorConfig, ExecutorState};
use feo3boy::gbz80core::microcode_executor::{MicrocodeExecutor, MicrocodeState};
use feo3boy::gbz80core::stepping_executor::{SteppingExecutor, SteppingExecutorState};
use feo3boy::gbz80core::{CpuContext, ExecutorContext, Gbz80State};
use feo3boy::interrupts::InterruptContext;
use feo3boy::memdev::MemContext;

/// Clap arg enum for selecting which executor to use.
#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, ValueEnum)]
pub enum ExecutorSelector {
    /// Use [`DirectExecutor`][feo3boy::gbz80core::direct_executor::DirectExecutor].
    Direct,
    /// Use [`DirectExecutorV2`][feo3boy::gbz80core::direct_executor_v2::DirectExecutorV2].
    DirectV2,
    /// Use [`MicrocodeExecutor`][feo3boy::gbz80core::microcode_executor::MicrocodeExecutor].
    Microcode,
    /// Use [`SteppingExecutor`][feo3boy::gbz80core::stepping_executor::SteppingExecutor].
    #[default]
    Stepping,
}

impl ExecutorConfig for ExecutorSelector {
    type Executor = MultiExecutor;
    fn create_initial_state(&self) -> <Self::Executor as Executor>::State {
        match *self {
            ExecutorSelector::Direct => MultiExecutorState::Direct(()),
            ExecutorSelector::DirectV2 => MultiExecutorState::DirectV2(()),
            ExecutorSelector::Microcode => MultiExecutorState::Microcode(Default::default()),
            ExecutorSelector::Stepping => MultiExecutorState::Stepping(Default::default()),
        }
    }
}

/// Executor that selects between the executors supported by the player.
pub struct MultiExecutor;

impl Executor for MultiExecutor {
    type State = MultiExecutorState;

    fn run_single_instruction(ctx: &mut impl ExecutorContext<State = Self::State>) {
        match ctx.executor() {
            MultiExecutorState::Direct(_) => {
                let ctx = MultiExecutorWrapperContext::<_, DirectExecutor>::wrap(ctx);
                DirectExecutor::run_single_instruction(ctx)
            }
            MultiExecutorState::DirectV2(_) => {
                let ctx = MultiExecutorWrapperContext::<_, DirectExecutorV2>::wrap(ctx);
                DirectExecutorV2::run_single_instruction(ctx)
            }
            MultiExecutorState::Microcode(_) => {
                let ctx = MultiExecutorWrapperContext::<_, MicrocodeExecutor>::wrap(ctx);
                MicrocodeExecutor::run_single_instruction(ctx)
            }
            MultiExecutorState::Stepping(_) => {
                let ctx = MultiExecutorWrapperContext::<_, SteppingExecutor>::wrap(ctx);
                SteppingExecutor::run_single_instruction(ctx)
            }
        }
    }
}

/// State for the MultiExecutor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MultiExecutorState {
    /// Use the Direct executor.
    ///
    /// Unit state is provided to simplify safely providing `&mut ()` for `executor_mut`.
    Direct(()),
    /// Use the V2 Direct executor (no state required).
    ///
    /// Unit state is provided to simplify safely providing `&mut ()` for `executor_mut`.
    DirectV2(()),
    /// Use the microcode executor (state provided here).
    Microcode(MicrocodeState),
    /// Use the stepping executor (state provided here).
    Stepping(SteppingExecutorState),
}

impl ExecutorState for MultiExecutorState {}

/// Type which wraps an ExecutorContext with `State = MultiExecutorState` and converts it
/// to one with the appropriate state subtype for the given executor type.
///
/// This type is `repr(transparent)` so it can be created from a context reference by
/// reference conversion.
#[derive(Debug)]
#[repr(transparent)]
struct MultiExecutorWrapperContext<C, E> {
    /// The context being wrapped.
    ctx: C,
    /// Marker for the executor we a converting to.
    executor: PhantomData<E>,
}

impl<C, E> MultiExecutorWrapperContext<C, E> {
    /// Produce a wraper context in-place from an existing context.
    fn wrap(ctx: &mut C) -> &mut Self {
        // SAFETY: This is safe because MultiExecutorWrapperContext is `repr(transparent)`
        // and has no drop impl so we can type convert by just casting the reference, and
        // it behaves as if we borrowed the context into the wrapper.
        unsafe { mem::transmute(ctx) }
    }
}

macro_rules! match_executor {
    ($ex:ty, $marker:ident) => {
        impl<C> ExecutorContext for MultiExecutorWrapperContext<C, $ex>
        where
            C: ExecutorContext<State = MultiExecutorState>,
        {
            type State = <$ex as Executor>::State;

            fn executor(&self) -> &Self::State {
                match self.ctx.executor() {
                    MultiExecutorState::$marker(ref state) => state,
                    _ => panic!(concat!("Wrong state type for ", stringify!($ex))),
                }
            }

            fn executor_mut(&mut self) -> &mut Self::State {
                match self.ctx.executor_mut() {
                    MultiExecutorState::$marker(ref mut state) => state,
                    _ => panic!(concat!("Wrong state type for ", stringify!($ex))),
                }
            }

            #[inline]
            fn yield1m(&mut self) {
                self.ctx.yield1m()
            }
        }
    };
}

match_executor!(DirectExecutor, Direct);
match_executor!(DirectExecutorV2, DirectV2);
match_executor!(MicrocodeExecutor, Microcode);
match_executor!(SteppingExecutor, Stepping);

impl<C, E> CpuContext for MultiExecutorWrapperContext<C, E>
where
    C: CpuContext,
{
    #[inline]
    fn cpu(&self) -> &Gbz80State {
        self.ctx.cpu()
    }

    #[inline]
    fn cpu_mut(&mut self) -> &mut Gbz80State {
        self.ctx.cpu_mut()
    }
}

impl<C, E> MemContext for MultiExecutorWrapperContext<C, E>
where
    C: MemContext,
{
    type Mem = <C as MemContext>::Mem;

    #[inline]
    fn mem(&self) -> &Self::Mem {
        self.ctx.mem()
    }

    #[inline]
    fn mem_mut(&mut self) -> &mut Self::Mem {
        self.ctx.mem_mut()
    }
}

impl<C, E> InterruptContext for MultiExecutorWrapperContext<C, E>
where
    C: InterruptContext,
{
    type Interrupts = <C as InterruptContext>::Interrupts;

    #[inline]
    fn interrupts(&self) -> &Self::Interrupts {
        self.ctx.interrupts()
    }

    #[inline]
    fn interrupts_mut(&mut self) -> &mut Self::Interrupts {
        self.ctx.interrupts_mut()
    }
}

impl<C, E> SystemClockContext for MultiExecutorWrapperContext<C, E>
where
    C: SystemClockContext,
{
    #[inline]
    fn clock(&self) -> &SystemClock {
        self.ctx.clock()
    }
}
