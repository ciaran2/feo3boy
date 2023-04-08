//! Provides an executor which runs by interpreting microcode directly.

use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut, Index};
use std::slice::SliceIndex;

use feo3boy_opcodes::compiler::instr::{InstrDef, InstrId};
use feo3boy_opcodes::gbz80types::Flags;
use feo3boy_opcodes::microcode::args::{Reg16, Reg8};
use feo3boy_opcodes::microcode::{self, Microcode};
use feo3boy_opcodes::opcode::{CBOpcode, InternalFetch, Opcode};
use log::{debug, trace};
use once_cell::sync::Lazy;

use crate::gbz80core::executor::{Executor, ExecutorState, PausePoint, SubInstructionExecutor};
use crate::gbz80core::oputils::halt;
use crate::gbz80core::{ExecutorContext, InterruptMasterState};
use crate::interrupts::Interrupts;
use crate::memdev::RootMemDevice;

mod tests;

/// State of the microcode executor in the CPU.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MicrocodeState {
    /// Stack for the microcode of the processor.
    pub stack: MicrocodeStack,
    /// Index into the currently executing microcode instruction where we are
    /// currently up to.
    pub pc: usize,
    /// Previous IME state. If set, will trigger an IME tick the next time there is a
    /// "fetch next instruction".
    pub prev_ime: Option<InterruptMasterState>,
    /// Currently executing instruction.
    pub instruction: Instr,
    /// Whether the next microcode to execute is the start of a new instruction fetch
    /// cycle. This is used to trigger breaking in some execution modes.
    pub is_fetch_start: bool,
}

impl MicrocodeState {
    /// Retrieve the current microcode instruction and advance the microcode program
    /// counter. Also clears is_fetch_start.
    fn next(&mut self) -> Microcode {
        if self.pc == 0 {
            debug!("Starting Instr {}", self.instruction.id());
        }
        // is_fetch_start is set by executing the FetchNextInstruction and cleared
        // when retrieveing the first microcode of the new instruction being fetched.
        self.is_fetch_start = false;
        if self.pc < self.instruction.len() {
            let ucode = self.instruction[self.pc];
            self.pc += 1;
            ucode
        } else {
            Microcode::FetchNextInstruction
        }
    }
}

impl Default for MicrocodeState {
    fn default() -> Self {
        Self {
            stack: Default::default(),
            pc: Default::default(),
            prev_ime: Default::default(),
            instruction: Instr::internal_fetch(),
            is_fetch_start: true,
        }
    }
}

impl ExecutorState for MicrocodeState {}

/// Convenience type for an ExecutorContext with a microcode executor state.
trait Ctx: ExecutorContext<State = MicrocodeState> {}

impl<E: ExecutorContext<State = MicrocodeState>> Ctx for E {}

/// Stack used to implement microcode operations.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MicrocodeStack {
    bytes: Vec<u8>,
}

impl MicrocodeStack {
    /// Check if the microcode stack is empty.
    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Push a u8 onto the microcode stack.
    pub fn pushu8(&mut self, val: u8) {
        self.bytes.push(val);
    }

    /// Pop a u8 from the microcode stack.
    pub fn popu8(&mut self) -> u8 {
        self.bytes
            .pop()
            .expect("Cannot pop u8, microcode stack empty")
    }

    /// Read the top u8 without popping.
    pub fn peeku8(&self) -> u8 {
        *self
            .bytes
            .last()
            .expect("Cannot peek u8, microcode stack empty")
    }

    /// Push a u16 onto the microcode stack. The low byte is pushed first followed by the
    /// high byte on top.
    pub fn pushu16(&mut self, val: u16) {
        let val = val.to_le_bytes();
        self.bytes.extend_from_slice(&val);
    }

    /// Pop a u16 from the microcode stack. The high byte is popped first followed by the
    /// low byte below it.
    pub fn popu16(&mut self) -> u16 {
        debug_assert!(
            self.bytes.len() >= 2,
            "Not enough bytes to pop u16, need 2, have {}",
            self.bytes.len()
        );
        let mut val = [0u8; 2];
        val.copy_from_slice(&self.bytes[self.bytes.len() - 2..]);
        self.bytes.truncate(self.bytes.len() - 2);
        u16::from_le_bytes(val)
    }

    /// Peek the top u16 without popping. The top byte is treated as the high byte and the
    /// second byte is treated as the low byte.
    pub fn peeku16(&self) -> u16 {
        debug_assert!(
            self.bytes.len() >= 2,
            "Not enough bytes to peek u16, need 2, have {}",
            self.bytes.len()
        );
        let mut val = [0u8; 2];
        val.copy_from_slice(&self.bytes[self.bytes.len() - 2..]);
        u16::from_le_bytes(val)
    }

    /// Duplicates the specified number of bytes on the top of the stack.
    pub fn dup(&mut self, count: usize) {
        debug_assert!(
            self.bytes.len() >= count,
            "Not enough bytes on the microcode stack to duplicated {} bytes, only have {}",
            count,
            self.bytes.len()
        );
        self.bytes.extend_from_within(self.bytes.len() - count..);
    }

    /// Swaps `top_count` bytes on the top of the stack with `second_count` bytes below
    /// it.
    pub fn swap(&mut self, top_count: usize, second_count: usize) {
        debug_assert!(
            self.bytes.len() >= top_count + second_count,
            "Not enough bytes on the microcode stack to swap {} bytes with {} bytes, only have {}",
            top_count,
            second_count,
            self.bytes.len()
        );
        let second_start = self.bytes.len() - (top_count + second_count);
        let second_end = self.bytes.len() - top_count;
        self.bytes.extend_from_within(second_start..second_end);
        self.bytes.drain(second_start..second_end);
    }

    /// Discards the specified number of bytes from the top of the stack.
    pub fn discard(&mut self, count: usize) {
        debug_assert!(
            self.bytes.len() >= count,
            "Not enough bytes on the microcode stack to discard {} bytes, only have {}",
            count,
            self.bytes.len()
        );
        self.bytes.drain(self.bytes.len() - count..);
    }
}

/// This is the Instr type that is normally used; it is a reference to a &'static InstrDef.
///
/// Note: because this is a reference to a static program value, if creating save-states,
/// you should probably only allow save-states between instructions, that way you don't
/// have to worry about how to serialize this in the Gbz80State and how to deal with if
/// the instructions change between emulator versions.
#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Instr(&'static InstrDef);

impl PartialEq for Instr {
    fn eq(&self, other: &Self) -> bool {
        // Since we use &'static definitions, we can compare Instrs using pointer
        // equality.
        self.0 as *const _ == other.0 as *const _
    }
}

impl Eq for Instr {}

impl Instr {
    /// Get the number of microcode instructions in this Instr.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Get the label applied to this Instr.
    #[inline]
    pub fn id(&self) -> InstrId {
        self.0.id()
    }

    /// Retrieve the microcode instruction for the CPU `[Internal Fetch]` operation.
    pub fn internal_fetch() -> Instr {
        /// Static for the CPU's internal fetch instruction.
        static INTERNAL_FETCH: Lazy<InstrDef> = Lazy::new(|| InternalFetch.into());
        Self(&*INTERNAL_FETCH)
    }

    /// Get the [`Instr`] for a given opcode.
    pub fn opcode(opcode: u8) -> Instr {
        /// Lookup table for Instrs for particular opcodes.
        static OPCODE_TABLE: Lazy<[InstrDef; 256]> = Lazy::new(|| {
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<InstrDef>; 256] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for i in 0..256 {
                data[i].write(Opcode::decode(i as u8).into());
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [InstrDef; 256]>(data) }
        });
        Instr(&OPCODE_TABLE[opcode as usize])
    }

    /// Get the [`Instr`] for a given cbopcode.
    pub fn cbopcode(cbopcode: u8) -> Instr {
        /// Lookup table for Instrs for particular opcodes.
        static CB_OPCODE_TABLE: Lazy<[InstrDef; 256]> = Lazy::new(|| {
            // Create an uninitialized array of `MaybeUninit`. The `assume_init` is
            // safe because the type we are claiming to have initialized here is a
            // bunch of `MaybeUninit`s, which do not require initialization.
            let mut data: [MaybeUninit<InstrDef>; 256] =
                unsafe { MaybeUninit::uninit().assume_init() };

            for i in 0..256 {
                data[i].write(CBOpcode::decode(i as u8).into());
            }

            // Everything is initialized. Transmute the array to the
            // initialized type.
            unsafe { mem::transmute::<_, [InstrDef; 256]>(data) }
        });

        Instr(&CB_OPCODE_TABLE[cbopcode as usize])
    }
}

/// Get the microcode at the given index.
impl<T> Index<T> for Instr
where
    T: SliceIndex<[Microcode]>,
{
    type Output = <T as SliceIndex<[Microcode]>>::Output;

    #[inline]
    fn index(&self, index: T) -> &Self::Output {
        &self.0[index]
    }
}

/// Trait for wrapping a value in its evaluator.
trait Runner {
    /// Wrap `self` in [`Eval`]`(self)`.
    #[inline]
    fn runner(self) -> Run<Self>
    where
        Self: Sized,
    {
        Run(self)
    }
}

impl Runner for Microcode {}
impl Runner for Reg8 {}
impl Runner for Reg16 {}

/// Helper to provide convenient eval/read/write functions for types deinfed in the
/// feo3boy-opcodes crate.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
struct Run<T>(T);

impl<T> Deref for Run<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Run<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Result of executing a microcode instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MicrocodeFlow {
    /// Microcode instruction requires a yield. CPU should stop executing microcode until
    /// 4 ticks have elapsed.
    Yield1m,
    /// Continue executing microcode.
    Continue,
}

/// Executes microcode by interpreting microcode opcodes individually.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct MicrocodeExecutor;

impl MicrocodeExecutor {
    /// Executes a single microcode instruction, returning the microcode flow result of
    /// that instruction.
    fn step(ctx: &mut impl ExecutorContext<State = <Self as Executor>::State>) -> MicrocodeFlow {
        let ucode = ctx.executor_mut().next();
        trace!("Running microcode {:?}", ucode);
        ucode.runner().run(ctx)
    }
}

impl Executor for MicrocodeExecutor {
    type State = MicrocodeState;

    fn run_single_instruction(ctx: &mut impl ExecutorContext<State = Self::State>) {
        loop {
            if Self::step(ctx) == MicrocodeFlow::Yield1m {
                ctx.yield1m();
            }
            if ctx.executor().is_fetch_start {
                break;
            }
        }
    }
}

impl SubInstructionExecutor for MicrocodeExecutor {
    fn tick(ctx: &mut impl ExecutorContext<State = Self::State>) {
        loop {
            match Self::step(ctx) {
                MicrocodeFlow::Continue => continue,
                MicrocodeFlow::Yield1m => break,
            }
        }
    }

    fn tick_until_yield_or_fetch(
        ctx: &mut impl ExecutorContext<State = Self::State>,
    ) -> PausePoint {
        loop {
            if Self::step(ctx) == MicrocodeFlow::Yield1m {
                return PausePoint::Yield;
            }
            if ctx.executor().is_fetch_start {
                return PausePoint::Fetch;
            }
        }
    }
}

impl Run<Microcode> {
    fn run(self, ctx: &mut impl Ctx) -> MicrocodeFlow {
        match *self {
            Microcode::Yield => return MicrocodeFlow::Yield1m,
            Microcode::ReadReg { reg } => reg.runner().read(ctx),
            Microcode::WriteReg { reg } => reg.runner().write(ctx),
            Microcode::ReadMem => read_mem8(ctx),
            Microcode::WriteMem => write_mem8(ctx),
            Microcode::Append { val } => microcode::defs::append.apply_arg(ctx, val),
            Microcode::GetFlagsMasked { mask } => get_flags_masked(ctx, mask),
            Microcode::SetFlagsMasked { mask } => set_flags_masked(ctx, mask),
            Microcode::Not => microcode::defs::not.apply(ctx),
            Microcode::Add => microcode::defs::add.apply(ctx),
            Microcode::Adc => microcode::defs::adc.apply(ctx),
            Microcode::Sub => microcode::defs::sub.apply(ctx),
            Microcode::Sbc => microcode::defs::sbc.apply(ctx),
            Microcode::And => microcode::defs::and.apply(ctx),
            Microcode::Or => microcode::defs::or.apply(ctx),
            Microcode::Xor => microcode::defs::xor.apply(ctx),
            Microcode::RotateLeft8 => microcode::defs::rotate_left8.apply(ctx),
            Microcode::RotateLeft9 => microcode::defs::rotate_left9.apply(ctx),
            Microcode::RotateRight8 => microcode::defs::rotate_right8.apply(ctx),
            Microcode::RotateRight9 => microcode::defs::rotate_right9.apply(ctx),
            Microcode::DecimalAdjust => microcode::defs::decmial_adjust.apply(ctx),
            Microcode::Compliment => microcode::defs::compliment.apply(ctx),
            Microcode::ShiftLeft => microcode::defs::shift_left.apply(ctx),
            Microcode::ShiftRight => microcode::defs::shift_right.apply(ctx),
            Microcode::ShiftRightSignExt => microcode::defs::shift_right_sign_ext.apply(ctx),
            Microcode::Swap => microcode::defs::swap.apply(ctx),
            Microcode::TestBit { bit } => microcode::defs::test_bit.apply_arg(ctx, bit),
            Microcode::SetBit { bit } => microcode::defs::set_bit.apply_arg(ctx, bit),
            Microcode::ResetBit { bit } => microcode::defs::reset_bit.apply_arg(ctx, bit),
            Microcode::ReadReg16 { reg } => reg.runner().read(ctx),
            Microcode::WriteReg16 { reg } => reg.runner().write(ctx),
            Microcode::Inc16 => microcode::defs::inc16.apply(ctx),
            Microcode::Dec16 => microcode::defs::dec16.apply(ctx),
            Microcode::Add16 => microcode::defs::add16.apply(ctx),
            Microcode::OffsetAddr => microcode::defs::offset_addr.apply(ctx),
            Microcode::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Microcode::Halt => halt(ctx),
            Microcode::EnableInterrupts { immediate } => enable_interrupts(ctx, immediate),
            Microcode::DisableInterrupts => ctx.cpu_mut().interrupt_master_enable.clear(),
            Microcode::CheckHalt => check_halt(ctx),
            Microcode::ClearHalt => ctx.cpu_mut().halted = false,
            Microcode::CheckIme => check_ime(ctx),
            Microcode::GetActiveInterrupts => get_active_interrupts(ctx),
            Microcode::PopHaltBug => pop_halt_bug(ctx),
            Microcode::PopInterrupt => pop_interrupt(ctx),
            Microcode::TickImeOnEnd => tick_ime_on_end(ctx),
            Microcode::Skip { steps } => skip(ctx, steps),
            Microcode::SkipIf { steps } => skip_if(ctx, steps),
            Microcode::FetchNextInstruction => fetch_next_instruction(ctx),
            Microcode::ParseOpcode => parse_opcode(ctx),
            Microcode::ParseCBOpcode => parse_cb_opcode(ctx),
            Microcode::Dup16 => microcode::defs::dup.apply(ctx),
            Microcode::Swap816 => microcode::defs::swap816.apply(ctx),
            Microcode::Intersperse => microcode::defs::intersperse.apply(ctx),
            Microcode::Discard8 => microcode::defs::discard8.apply(ctx),
            Microcode::Discard16 => microcode::defs::discard16.apply(ctx),
        }
        MicrocodeFlow::Continue
    }
}

/// Pop a 16 bit address and use it to read an 8 bit value from memory onto the stack.
fn read_mem8(ctx: &mut impl Ctx) {
    let addr = ctx.executor_mut().stack.popu16();
    let val = ctx.mem().read_byte(addr);
    ctx.executor_mut().stack.pushu8(val);
}

/// Pop a 16 bit address and an 8 bit value and write the value to the address.
fn write_mem8(ctx: &mut impl Ctx) {
    let addr = ctx.executor_mut().stack.popu16();
    let val = ctx.executor_mut().stack.popu8();
    ctx.mem_mut().write_byte(addr, val);
}

/// Internal implementation of microcode to fetch masked flags to the output.
fn get_flags_masked(ctx: &mut impl Ctx, mask: Flags) {
    let flags = ctx.cpu().regs.flags.intersection(mask);
    ctx.executor_mut().stack.pushu8(flags.bits());
}

/// Internal implementation of microcode to apply masked flags to the output.
fn set_flags_masked(ctx: &mut impl Ctx, mask: Flags) {
    let flags = Flags::from_bits_truncate(ctx.executor_mut().stack.popu8());
    ctx.cpu_mut().regs.flags.merge(flags, mask);
}

/// Internal implementation of the microcode enable interrupts instruction. Runs either
/// set or set_next_instruction, depending on whether the interrupt is to be set
/// immeidately or not.
fn enable_interrupts(ctx: &mut impl Ctx, immediate: bool) {
    if immediate {
        ctx.cpu_mut().interrupt_master_enable.set();
    } else {
        ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
    }
}

/// Pushes the value of whether the CPU is halted onto the microcode stack as a u8.
fn check_halt(ctx: &mut impl Ctx) {
    let halted = ctx.cpu().halted as u8;
    ctx.executor_mut().stack.pushu8(halted);
}

/// Pushes the value of whether the CPU IME is enabled onto the microcode stack as a
/// u8.
fn check_ime(ctx: &mut impl Ctx) {
    let ime = ctx.cpu().interrupt_master_enable.enabled() as u8;
    ctx.executor_mut().stack.pushu8(ime);
}

/// Pushes the set of interrupts which are both active and enabled onto the microcode
/// stack.
fn get_active_interrupts(ctx: &mut impl Ctx) {
    let active = ctx.interrupts().active().bits();
    ctx.executor_mut().stack.pushu8(active);
}

/// Pushes the value of the halt_bug flag onto the microcode stack, clearing the value.
fn pop_halt_bug(ctx: &mut impl Ctx) {
    let halt_bug = mem::replace(&mut ctx.cpu_mut().halt_bug, false) as u8;
    ctx.executor_mut().stack.pushu8(halt_bug);
}

/// Pushes the destination address of the next active and enabled interrupt onto the
/// microcode stack and clears that interrupt. Panics if no interrupts are active!.
fn pop_interrupt(ctx: &mut impl Ctx) {
    match ctx.interrupts().active().iter().next() {
            Some(interrupt) => {
                ctx.interrupts_mut().clear(interrupt);
                ctx.executor_mut().stack.pushu16(interrupt.handler_addr());
            }
            None => panic!("Must not use the PopInterrupt microcode instruction if there are no active interrupts."),
        }
}

/// Enables IME ticking on the next `FetchNextInstruction`.
fn tick_ime_on_end(ctx: &mut impl Ctx) {
    debug_assert!(
        ctx.executor().prev_ime.is_none(),
        "prev_ime is already set to {:?}, trying to set {:?}",
        ctx.executor().prev_ime.unwrap(),
        ctx.cpu().interrupt_master_enable
    );
    ctx.executor_mut().prev_ime = Some(ctx.cpu().interrupt_master_enable);
}

/// Skip the CPU forward by this number of microcode instruction steps.
fn skip(ctx: &mut impl Ctx, steps: usize) {
    // Only checked for overflow in debug.
    ctx.executor_mut().pc += steps;
    debug_assert!(ctx.executor().pc <= ctx.executor().instruction.len());
}

/// Skip the CPU forward by this number of microcode instruction steps if the u8 value on
/// top of the microcode stack is non-zero.
fn skip_if(ctx: &mut impl Ctx, steps: usize) {
    let cond = ctx.executor_mut().stack.popu8();
    if cond != 0 {
        // Only checked for overflow in debug.
        ctx.executor_mut().pc += steps;
        debug_assert!(ctx.executor().pc <= ctx.executor().instruction.len());
    }
}

/// Resets to the CPU Internal instruction.
// Shared with combo-codes.
fn fetch_next_instruction(ctx: &mut impl Ctx) {
    debug_assert!(
        ctx.executor().stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        ctx.executor().instruction.id()
    );
    if let Some(prev_ime) = ctx.executor_mut().prev_ime.take() {
        ctx.cpu_mut().interrupt_master_enable.tick(prev_ime);
    }
    ctx.executor_mut().pc = 0;
    ctx.executor_mut().instruction = Instr::internal_fetch();
    ctx.executor_mut().is_fetch_start = true;
}

/// Parses a regular opcode from the microcode stack and replaces the current
/// instruction with it.
fn parse_opcode(ctx: &mut impl Ctx) {
    let opcode = ctx.executor_mut().stack.popu8();
    debug_assert!(
        ctx.executor().stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        ctx.executor().instruction.id()
    );
    ctx.executor_mut().pc = 0;
    ctx.executor_mut().instruction = Instr::opcode(opcode);
}

/// Parses a cb opcode from the microcode stack and replaces the current
/// instruction with it.
fn parse_cb_opcode(ctx: &mut impl Ctx) {
    let opcode = ctx.executor_mut().stack.popu8();
    debug_assert!(
        ctx.executor().stack.is_empty(),
        "Previous Instruction {} failed to empty its stack",
        ctx.executor().instruction.id()
    );
    ctx.executor_mut().pc = 0;
    ctx.executor_mut().instruction = Instr::cbopcode(opcode);
}

impl Run<Reg8> {
    /// Read this register into the microcode stack.
    fn read(self, ctx: &mut impl Ctx) {
        let val = match self.0 {
            Reg8::Acc => ctx.cpu().regs.acc,
            Reg8::B => ctx.cpu().regs.b,
            Reg8::C => ctx.cpu().regs.c,
            Reg8::D => ctx.cpu().regs.d,
            Reg8::E => ctx.cpu().regs.e,
            Reg8::H => ctx.cpu().regs.h,
            Reg8::L => ctx.cpu().regs.l,
        };
        ctx.executor_mut().stack.pushu8(val);
    }

    /// Write this register from the microcode stack.
    fn write(self, ctx: &mut impl Ctx) {
        let val = ctx.executor_mut().stack.popu8();
        match self.0 {
            Reg8::Acc => ctx.cpu_mut().regs.acc = val,
            Reg8::B => ctx.cpu_mut().regs.b = val,
            Reg8::C => ctx.cpu_mut().regs.c = val,
            Reg8::D => ctx.cpu_mut().regs.d = val,
            Reg8::E => ctx.cpu_mut().regs.e = val,
            Reg8::H => ctx.cpu_mut().regs.h = val,
            Reg8::L => ctx.cpu_mut().regs.l = val,
        }
    }
}

impl Run<Reg16> {
    fn read(self, ctx: &mut impl Ctx) {
        let val = match self.0 {
            Reg16::AF => ctx.cpu().regs.af(),
            Reg16::BC => ctx.cpu().regs.bc(),
            Reg16::DE => ctx.cpu().regs.de(),
            Reg16::HL => ctx.cpu().regs.hl(),
            Reg16::Sp => ctx.cpu().regs.sp,
            Reg16::Pc => ctx.cpu().regs.pc,
        };
        ctx.executor_mut().stack.pushu16(val);
    }

    fn write(self, ctx: &mut impl Ctx) {
        let val = ctx.executor_mut().stack.popu16();
        match self.0 {
            Reg16::AF => ctx.cpu_mut().regs.set_af(val),
            Reg16::BC => ctx.cpu_mut().regs.set_bc(val),
            Reg16::DE => ctx.cpu_mut().regs.set_de(val),
            Reg16::HL => ctx.cpu_mut().regs.set_hl(val),
            Reg16::Sp => ctx.cpu_mut().regs.sp = val,
            Reg16::Pc => ctx.cpu_mut().regs.pc = val,
        }
    }
}

trait FromStack: Sized {
    fn from_stack(ctx: &mut impl Ctx) -> Self;
}

impl FromStack for u8 {
    fn from_stack(ctx: &mut impl Ctx) -> Self {
        ctx.executor_mut().stack.popu8()
    }
}

impl FromStack for bool {
    fn from_stack(ctx: &mut impl Ctx) -> Self {
        ctx.executor_mut().stack.popu8() != 0
    }
}

impl FromStack for Flags {
    fn from_stack(ctx: &mut impl Ctx) -> Self {
        Flags::from_bits_truncate(ctx.executor_mut().stack.popu8())
    }
}

impl FromStack for u16 {
    fn from_stack(ctx: &mut impl Ctx) -> Self {
        ctx.executor_mut().stack.popu16()
    }
}

trait ToStack: Sized {
    fn to_stack(self, ctx: &mut impl Ctx);
}

impl ToStack for u8 {
    fn to_stack(self, ctx: &mut impl Ctx) {
        ctx.executor_mut().stack.pushu8(self)
    }
}

impl ToStack for bool {
    fn to_stack(self, ctx: &mut impl Ctx) {
        ctx.executor_mut().stack.pushu8(self as u8)
    }
}

impl ToStack for Flags {
    fn to_stack(self, ctx: &mut impl Ctx) {
        ctx.executor_mut().stack.pushu8(self.bits());
    }
}

impl ToStack for u16 {
    fn to_stack(self, ctx: &mut impl Ctx) {
        ctx.executor_mut().stack.pushu16(self)
    }
}

/// Trait for applying a operator function to the context.
trait Apply<I, O> {
    fn apply(self, ctx: &mut impl Ctx);
}

macro_rules! applier {
    ([$($in:ident),*], [$($out:ident),*]) => {
        #[allow(unused_parens, non_snake_case)]
        impl<
            $($in: FromStack,)*
            $($out: ToStack,)*
            F: FnOnce($($in),*) -> ($($out),*),
        > Apply<($($in),*), ($($out),*)> for F {
            fn apply(self, ctx: &mut impl Ctx) {
                $(let $in = $in::from_stack(ctx);)*
                let ($($out),*) = self($($in),*);
                $($out.to_stack(ctx);)*
            }
        }
    }
}

applier!([In1], []);
applier!([In1], [Out1]);
applier!([In1, In2], [Out1]);
applier!([In1, In2, In3], [Out1]);
applier!([In1], [Out1, Out2]);
applier!([In1, In2], [Out1, Out2]);
applier!([In1, In2, In3], [Out1, Out2]);
applier!([In1], [Out1, Out2, Out3]);
applier!([In1, In2], [Out1, Out2, Out3]);
applier!([In1, In2, In3], [Out1, Out2, Out3]);
applier!([In1], [Out1, Out2, Out3, Out4]);
applier!([In1, In2], [Out1, Out2, Out3, Out4]);
applier!([In1, In2, In3], [Out1, Out2, Out3, Out4]);

trait ApplyArg<T, I, O> {
    fn apply_arg(self, ctx: &mut impl Ctx, arg: T);
}

macro_rules! arg_applier {
    ([$($in:ident),*], [$($out:ident),*]) => {
        #[allow(unused_parens, non_snake_case)]
        impl<
            T,
            $($in: FromStack,)*
            $($out: ToStack,)*
            F: FnOnce(T, $($in),*) -> ($($out),*),
        > ApplyArg<T, ($($in),*), ($($out),*)> for F {
            fn apply_arg(self, ctx: &mut impl Ctx, arg: T) {
                $(let $in = $in::from_stack(ctx);)*
                let ($($out),*) = self(arg, $($in),*);
                $($out.to_stack(ctx);)*
            }
        }
    }
}

arg_applier!([], [Out1]);
arg_applier!([], [Out1, Out2]);
arg_applier!([], [Out1, Out2, Out3]);
arg_applier!([In1], [Out1]);
arg_applier!([In1, In2], [Out1]);
arg_applier!([In1, In2, In3], [Out1]);
arg_applier!([In1], [Out1, Out2]);
arg_applier!([In1, In2], [Out1, Out2]);
arg_applier!([In1, In2, In3], [Out1, Out2]);
arg_applier!([In1], [Out1, Out2, Out3]);
arg_applier!([In1, In2], [Out1, Out2, Out3]);
arg_applier!([In1, In2, In3], [Out1, Out2, Out3]);
