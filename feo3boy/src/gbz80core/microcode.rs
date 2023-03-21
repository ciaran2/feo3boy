use std::ops::Index;
use std::slice::SliceIndex;

#[cfg(feature = "microcode")]
use once_cell::sync::Lazy;

#[cfg(feature = "microcode")]
use crate::gbz80core::oputils::{add8_flags, halt, rotate_left9, rotate_right9, sub8_flags};
#[cfg(feature = "microcode")]
use crate::gbz80core::CpuContext;
use crate::gbz80core::Flags;

/// Trait for the microcode stack.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg(feature = "microcode")]
pub struct MicrocodeStack {
    bytes: Vec<u8>,
}

#[cfg(feature = "microcode")]
impl MicrocodeStack {
    /// Check if the microcode stack is empty.
    fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    /// Push a u8 onto the microcode stack.
    fn pushu8(&mut self, val: u8) {
        self.bytes.push(val);
    }

    /// Pop a u8 from the microcode stack.
    fn popu8(&mut self) -> u8 {
        self.bytes
            .pop()
            .expect("Cannot pop u8, microcode stack empty")
    }

    /// Read the top u8 without popping.
    #[allow(unused)]
    fn peeku8(&self) -> u8 {
        *self
            .bytes
            .last()
            .expect("Cannot peek u8, microcode stack empty")
    }

    /// Push a u8 onto the microcode stack.
    fn pushu16(&mut self, val: u16) {
        let val = val.to_le_bytes();
        self.bytes.extend_from_slice(&val);
    }

    /// Pop a u16 from the microcode stack.
    fn popu16(&mut self) -> u16 {
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

    /// Peek the top u8 without popping.
    #[allow(unused)]
    fn peeku16(&self) -> u16 {
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
    fn ustack_dup(&mut self, count: usize) {
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
    fn ustack_swap(&mut self, top_count: usize, second_count: usize) {
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
    fn ustack_discard(&mut self, count: usize) {
        debug_assert!(
            self.bytes.len() >= count,
            "Not enough bytes on the microcode stack to discard {} bytes, only have {}",
            count,
            self.bytes.len()
        );
        self.bytes.drain(self.bytes.len() - count..);
    }
}

/// Result of executing a microcode instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg(feature = "microcode")]
pub enum MicrocodeFlow {
    /// Microcode instruction requires a yield. CPU should stop executing microcode until
    /// 4 ticks have elapsed.
    Yield1m,
    /// Continue executing microcode.
    Continue,
}

/// Builder for microcode.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct MicrocodeBuilder {
    microcode: Vec<Microcode>,
}

impl MicrocodeBuilder {
    /// Create a new empty builder.
    pub fn new() -> Self {
        Default::default()
    }

    /// Create a new builder with the given initial instructions.
    pub fn first(code: impl Into<MicrocodeBuilder>) -> Self {
        code.into()
    }

    /// Create a builder containing just a yield.
    pub fn r#yield() -> Self {
        Self::first(Microcode::Yield)
    }

    // Create a block of microcode which will pop a single byte boolean off the microcode
    // stack and then execute one of two branches depending on whether it is nonzero
    // (true) or zero (false).
    pub fn cond(
        code_if_true: impl Into<MicrocodeBuilder>,
        code_if_false: impl Into<MicrocodeBuilder>,
    ) -> Self {
        // We will build code that is:
        // SkipIf { steps: false_branch.len() + 1 }
        // false_branch
        // Skip { steps: true_branch.len() }
        // true_branch
        //
        // Because SkipIf runs on a true condition, we put the false branch first.
        // We only add the extra unconditional skip if the true_branch is non-empty.

        let code_if_true = code_if_true.into();
        let mut code_if_false = code_if_false.into();

        // If the true branch is non-empty, add code to the end of the false branch that
        // will skip the true branch. If the true branch is empty, this is
        // unnecessary.
        if !code_if_true.microcode.is_empty() {
            code_if_false.microcode.push(Microcode::Skip {
                steps: code_if_true.microcode.len(),
            });
        }

        MicrocodeBuilder::first(Microcode::SkipIf {
            steps: code_if_false.microcode.len(),
        })
        .then(code_if_false)
        .then(code_if_true)
    }

    /// Pop a single byte boolean off the microcode stack and run this if it is true.
    pub fn if_true(code_if_true: impl Into<MicrocodeBuilder>) -> Self {
        MicrocodeBuilder::cond(code_if_true, MicrocodeBuilder::new())
    }

    /// Pop a single byte boolean off the microcode stack and run this if it is false.
    pub fn if_false(code_if_false: impl Into<MicrocodeBuilder>) -> Self {
        MicrocodeBuilder::cond(MicrocodeBuilder::new(), code_if_false)
    }

    /// Create a single-step microcode that reads from the specified source.
    pub fn read<R: MicrocodeReadable>(from: R) -> Self {
        from.to_read()
    }

    /// Create a single-step microcode that writes to the specified source.
    pub fn write<W: MicrocodeWritable>(to: W) -> Self {
        to.to_write()
    }

    /// Add the given instruction or instructions to the end of this microcode.
    pub fn then(mut self, code: impl Into<MicrocodeBuilder>) -> Self {
        let other = code.into();
        self.microcode.extend(other.microcode);
        self
    }

    /// Add a yield to the end of the builder.
    pub fn then_yield(mut self) -> Self {
        self.microcode.push(Microcode::Yield);
        self
    }

    /// Add a read to the end of the builder.
    pub fn then_read<R: MicrocodeReadable>(self, from: R) -> Self {
        self.then(from.to_read())
    }

    /// Add a write to the end of the builder.
    pub fn then_write<W: MicrocodeWritable>(self, to: W) -> Self {
        self.then(to.to_write())
    }

    /// Build an instruction from this microcode, adding the label for the instruction.
    pub fn build(self, label: String) -> InstrDef {
        InstrDef {
            label,
            microcode: self.microcode,
        }
    }
}

impl From<Microcode> for MicrocodeBuilder {
    fn from(value: Microcode) -> Self {
        Self {
            microcode: vec![value],
        }
    }
}

impl<T: Into<MicrocodeBuilder>> From<Option<T>> for MicrocodeBuilder {
    fn from(value: Option<T>) -> Self {
        value.map(Into::into).unwrap_or_default()
    }
}

/// Helper for the Microcode Builder to allow read/write then_read/then_write to operate
/// directly on destinations.
pub trait MicrocodeReadable {
    fn to_read(self) -> MicrocodeBuilder;
}

pub trait MicrocodeWritable {
    fn to_write(self) -> MicrocodeBuilder;
}

/// Definition of an instruction, built from sub-instructions. This is usually stored in a
/// lazy static and used by reference, as an Instr.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstrDef {
    label: String,
    microcode: Vec<Microcode>,
}

impl InstrDef {
    /// Get the number of microcode instructions in this Instr.
    pub fn len(&self) -> usize {
        self.microcode.len()
    }

    /// Get the label applied to this InstrDef.
    pub fn label(&self) -> &str {
        &self.label
    }
}

/// Get the microcode at the given index.
impl<T> Index<T> for InstrDef
where
    T: SliceIndex<[Microcode]>,
{
    type Output = <T as SliceIndex<[Microcode]>>::Output;

    fn index(&self, index: T) -> &Self::Output {
        &self.microcode[index]
    }
}

/// This is the Instr type that is normally used; it is a reference to a &'static InstrDef.
///
/// Note: because this is a reference to a static program value, if creating save-states,
/// you should probably only allow save-states between instructions, that way you don't
/// have to worry about how to serialize this in the Gbz80State and how to deal with if
/// the instructions change between emulator versions.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Instr(&'static InstrDef);

impl Instr {
    /// Create an [`Instr`] referring to the given [`InstrDef`]
    #[cfg(feature = "microcode")]
    pub(super) fn new(def: &'static InstrDef) -> Self {
        Self(def)
    }

    /// Get the number of microcode instructions in this Instr.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Get the label applied to this Instr.
    #[inline]
    pub fn label(&self) -> &'static str {
        self.0.label()
    }

    /// Retrieve the microcode instruction for the CPU `[Internal Fetch]` operation.
    #[cfg(feature = "microcode")]
    pub fn internal_fetch() -> Instr {
        /// Static for the CPU's internal fetch instruction.
        static INTERNAL_FETCH: Lazy<InstrDef> = Lazy::new(|| {
            MicrocodeBuilder::first(InternalFetch).build("[Internal Fetch]".to_owned())
        });
        Self(&*INTERNAL_FETCH)
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

/// Identifies an 8 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Reg8 {
    Acc,
    B,
    C,
    D,
    E,
    H,
    L,
}

impl Reg8 {
    /// Read this register into the microcode stack.
    #[cfg(feature = "microcode")]
    fn read(self, ctx: &mut impl CpuContext) {
        let val = match self {
            Reg8::Acc => ctx.cpu().regs.acc,
            Reg8::B => ctx.cpu().regs.b,
            Reg8::C => ctx.cpu().regs.c,
            Reg8::D => ctx.cpu().regs.d,
            Reg8::E => ctx.cpu().regs.e,
            Reg8::H => ctx.cpu().regs.h,
            Reg8::L => ctx.cpu().regs.l,
        };
        ctx.cpu_mut().microcode_stack.pushu8(val);
    }

    /// Write this register from the microcode stack.
    #[cfg(feature = "microcode")]
    fn write(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu8();
        match self {
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

impl MicrocodeReadable for Reg8 {
    fn to_read(self) -> MicrocodeBuilder {
        Microcode::ReadReg8(self).into()
    }
}

impl MicrocodeWritable for Reg8 {
    fn to_write(self) -> MicrocodeBuilder {
        Microcode::WriteReg8(self).into()
    }
}

/// Helper struct for the microcode builder. When applied with `read`/`write` or
/// `then_read`/`then_write` inserts a yield followed by a memory read/write.
pub struct Mem8;

impl MicrocodeReadable for Mem8 {
    fn to_read(self) -> MicrocodeBuilder {
        MicrocodeBuilder::r#yield().then(Microcode::ReadMem8)
    }
}

impl MicrocodeWritable for Mem8 {
    fn to_write(self) -> MicrocodeBuilder {
        MicrocodeBuilder::r#yield().then(Microcode::WriteMem8)
    }
}

/// Helper struct for the microcode builder. When applied with `read` or
/// `then_read` inserts a PC fetch-increment followed by a memory read.
pub struct Immediate8;

impl MicrocodeReadable for Immediate8 {
    fn to_read(self) -> MicrocodeBuilder {
        // Fetch the PC.
        // stack: ...|pcl|pch|
        MicrocodeBuilder::read(Reg16::Pc)
            // Copy the PC so we can increment it.
            // stack: ...|pcl|pch|pcl|pch|
            .then(Microcode::Dup(2))
            // Increment the program counter.
            // stack: ...|pcl|pch|PCL|PCH|
            .then(Microcode::Inc16)
            // Write back the new PC value.
            // stack: ...|pcl|pch|
            .then_write(Reg16::Pc)
            // Use the old PC value as the address to read from.
            // stack: ...|val|
            .then_read(Mem8)
    }
}

/// Helper struct for the microcode builder. When applied with `read` or
/// `then_read` inserts two Immediate8 reads. Endianness is correct because we always push
/// the lower byte of a LE word to the microcode stack first followed by the higher byte,
/// which always keeps the word in gameboy LE byte order regardless of the running system
/// endianness.
pub struct Immediate16;

impl MicrocodeReadable for Immediate16 {
    fn to_read(self) -> MicrocodeBuilder {
        MicrocodeBuilder::read(Immediate8).then_read(Immediate8)
    }
}

/// Identifies a 16 bit register in the microcode.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    Sp,
    Pc,
}

impl Reg16 {
    #[cfg(feature = "microcode")]
    fn read(self, ctx: &mut impl CpuContext) {
        let val = match self {
            Reg16::AF => ctx.cpu().regs.af(),
            Reg16::BC => ctx.cpu().regs.bc(),
            Reg16::DE => ctx.cpu().regs.de(),
            Reg16::HL => ctx.cpu().regs.hl(),
            Reg16::Sp => ctx.cpu().regs.sp,
            Reg16::Pc => ctx.cpu().regs.pc,
        };
        ctx.cpu_mut().microcode_stack.pushu16(val);
    }

    #[cfg(feature = "microcode")]
    fn write(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu16();
        match self {
            Reg16::AF => ctx.cpu_mut().regs.set_af(val),
            Reg16::BC => ctx.cpu_mut().regs.set_bc(val),
            Reg16::DE => ctx.cpu_mut().regs.set_de(val),
            Reg16::HL => ctx.cpu_mut().regs.set_hl(val),
            Reg16::Sp => ctx.cpu_mut().regs.sp = val,
            Reg16::Pc => ctx.cpu_mut().regs.pc = val,
        }
    }
}

impl MicrocodeReadable for Reg16 {
    fn to_read(self) -> MicrocodeBuilder {
        Microcode::ReadReg16(self).into()
    }
}

impl MicrocodeWritable for Reg16 {
    fn to_write(self) -> MicrocodeBuilder {
        Microcode::WriteReg16(self).into()
    }
}

/// Helper struct for the microcode builder. When applied with `read`/`write` or
/// `then_read`/`then_write` inserts two read/writes to move a 16 bit value.
pub struct Mem16;

/// When reading a u16 from memory onto the microcode stack, we start with the 16 bit
/// address of the lower byte already on the microcode stack, and want to end with the 16
/// bit value on the microcode stack in its place. We will read in the order lower byte
/// then higher byte.
impl MicrocodeReadable for Mem16 {
    fn to_read(self) -> MicrocodeBuilder {
        // stack: ...|al|ah|
        // Duplicate the address so we don't lose it when we do the first read.
        // stack: ...|al|ah|al|ah|
        MicrocodeBuilder::first(Microcode::Dup(2))
            // This issues the memory yield and grabs the lower order byte.
            // stack: ...|al|ah|vl|
            .then_read(Mem8)
            // Swap the newly read top byte with the original address.
            // stack: ...|vl|al|ah|
            .then(Microcode::Swap { top: 1, second: 2 })
            // Increment the address to get the second byte.
            // stack: ...|vl|AL|AH|
            .then(Microcode::Inc16)
            // This issues the memory yield and grabs the higher order byte.
            // Since the stack always uses lower order byte first, this leaves the two
            // bytes in the correct order on the microcode stack.
            // stack: ...|vl|vh|
            .then_read(Mem8)
    }
}

/// When writing a u16 from the microcode stack into memory, we start with the 16 bit
/// address of the lower byte on the top of the microcode stack, with the 16 bit value
/// below it and want to pop both those off the microcode stack by the time we're done.
/// Trickily, we want to write the lower byte of the value before the higher byte, so this
/// involves swaping around the top couple values on the stack several times.
impl MicrocodeWritable for Mem16 {
    fn to_write(self) -> MicrocodeBuilder {
        // stack: ...|vl|vh|al|ah|
        // Duplicate the address so we don't lose it when we do the first write.
        // stack: ...|vl|vh|al|ah|al|ah|
        MicrocodeBuilder::first(Microcode::Dup(2))
            // Bring the first byte of the value to be written to the top of the stack:
            // stack: ...|vh|al|ah|al|ah|vl|
            .then(Microcode::Swap { top: 5, second: 1 })
            // Move the first byte of the value to be written back below the target
            // address:
            // stack: ...|vh|al|ah|vl|al|ah|
            .then(Microcode::Swap { top: 1, second: 2 })
            // Write the first byte of the value, yielding as neded.
            // stack: ...|vh|al|ah|
            .then_write(Mem8)
            // Increment the destination address.
            // stack: ...|vh|AL|AH|
            .then(Microcode::Inc16)
            // Write the high order byte to memory.
            .then_write(Mem8)
    }
}

/// Helper for reading the stack pointer and incrementing it.
///
/// When used as a [`MicrocodeReadable`], `IncSp` will load the value of the stack pointer
/// to the microcode stack and then increment the stack pointer in-place.
pub struct IncSp;

impl MicrocodeReadable for IncSp {
    fn to_read(self) -> MicrocodeBuilder {
        // Get the current stack pointer.
        // stack: ...|spl|sph|
        MicrocodeBuilder::read(Reg16::Sp)
            // Copy the SP so we keep the old value as the result.
            // stack: ...|spl|sph|spl|sph|
            .then(Microcode::Dup(2))
            // Increment the stack pointer.
            // stack: ...|spl|sph|SPL|SPH|
            .then(Microcode::Inc16)
            // Write the incremented value back to the register.
            // stack: ...|spl|sph|
            .then_write(Reg16::Sp)
    }
}

/// Helper for decrementing the stack pointer and reading it.
///
/// When used as a [`MicrocodeReadable`], `DecSp` will decrement the value of the stack
/// pointer in place and read the value into the microcode stack.
pub struct DecSp;

impl MicrocodeReadable for DecSp {
    fn to_read(self) -> MicrocodeBuilder {
        // Get the current stack pointer.
        // stack: ...|SPL|SPH|
        MicrocodeBuilder::read(Reg16::Sp)
            // Decrement the stack pointer.
            // stack: ...|spl|sph|
            .then(Microcode::Dec16)
            // Copy the SP so we still have a copy on the microcode stack after putting it
            // back in the register.
            // stack: ...|spl|sph|spl|sph|
            .then(Microcode::Dup(2))
            // Write the decremented value back to the register.
            // stack: ...|spl|sph|
            .then_write(Reg16::Sp)
    }
}

/// Helper struct for manipulating the GameBoy stack from microcode.
pub struct GbStack16;

/// When used as a [`MicrocodeReadable`], `GbStack16` acts as a `pop` operation. A 16 bit
/// value will be read out from the location of the stack pointer, and the stack pointer
/// will be incremented by 2 (the stack starts at the end of memory and moves down).
impl MicrocodeReadable for GbStack16 {
    fn to_read(self) -> MicrocodeBuilder {
        // Get the SP incremented address.
        // stack: ...|spl|sph|
        MicrocodeBuilder::read(IncSp)
            // Yield a cycle and then read from the sp address on the stack to get the
            // low-order byte of the value being popped.
            // stack: ...|vl |
            .then_read(Mem8)
            // Fetch and increment the stack pointer again.
            // stack: ...|vl |spl|sph|
            .then_read(IncSp)
            // Yield a cycle and then read from the sp address on the stack to get the
            // high-order byte of the value being popped.
            // stack: ...|vl |vh |
            .then_read(Mem8)
    }
}

/// When used as a [`MicrocodeWritable`], `GbStack16` acts as a `push` operation. A 16 bit
/// value will be popped from the microcode stack, the stack pointer will be decremented
/// by 2, and the value will be written to the new location of the stack pointer.
impl MicrocodeWritable for GbStack16 {
    fn to_write(self) -> MicrocodeBuilder {
        // stack: ...|vl |vh |
        // Get the SP decremented address.
        // stack: ...|vl |vh |spl|sph|
        MicrocodeBuilder::read(DecSp)
            // Yield a cycle and then write to the sp address on the stack to write the
            // high-order byte of the value being pushed.
            // stack: ...|vl |
            .then_write(Mem8)
            // Fetch and decrement the stack pointer again.
            // stack: ...|vl |spl|sph|
            .then_read(DecSp)
            // Yield a cycle and then write to the sp address on the stack to write the
            // low-order byte of the value being pushed.
            // stack: ...|
            .then_write(Mem8)
    }
}

/// Microcode instruction used to implement the internals of the CPU.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Microcode {
    /// Instructs the CPU to yield.
    Yield,

    /*
     * 8 bit manipulations
     */
    // Value reads and writes.
    /// Read an 8 bit register onto the stack.
    ReadReg8(Reg8),
    /// Write an 8 bit register from the stack.
    WriteReg8(Reg8),
    /// Pop a 16 bit value from the stack and use it to read an 8 bit value onto the
    /// stack.
    ReadMem8,
    /// Pop a 16 bit value from the stack and use it as the address, then pop an 8 bit
    /// value from the stack and wite it to that address.
    WriteMem8,
    /// Appends a value directly onto the microcode stack.
    Append(u8),

    // Ops for manipulating the flags register
    /// Reads the value of the flags register onto the microcode stack, masking to only
    /// the specified flags.
    GetFlagsMasked {
        mask: Flags,
    },
    /// Pops a flag set off the top of the stack and applies the given mask before
    /// assigning it to the flags register. Flags not in the mask are ignored (not
    /// overwritten).
    SetFlagsMasked {
        mask: Flags,
    },

    // 8 bit operators
    /// Execute a microcode binary operation.
    BinaryOp(BinaryOp),
    /// Execute a microcode unary operation.
    UnaryOp(UnaryOp),
    /// Execute a microcode bit op.
    BitOp(BitOp),

    /*
     * 16 bit manipulations
     */
    // Value reads and writes
    /// Read a 16 bit register onto the stack.
    ReadReg16(Reg16),
    /// Write as 16 bit register from the stack.
    WriteReg16(Reg16),

    // 16 bit operators
    /// Increment a 16 bit value on the microcode stack. No flags.
    Inc16,
    /// Decrement a 16 bit value on the microcode stack. No flags.
    Dec16,
    /// Performs a 16 bit add with flags. Pops two 16 bit args off the stack (lhs on top,
    /// rhs below it), adds them, and pushes the result followed by the flags on top. The
    /// returned flags will have 00HC set based on the upper byte of the operation (as if
    /// it was performed by running the pseudo-instructions `add l,<arg-low>; adc
    /// h,<arg-high>`.
    Add16,
    /// Pops a 16 bit address off the stack followed by an 8 bit offset below it. Applies
    /// address offsetting and pushes the new address followed by the flags on top.
    OffsetAddr,

    /*
     * CPU Internals.
     */
    // Specialty CPU Instructions (parts of real GB opcodes).
    /// Performs the CPU Stop instruction.
    Stop,
    /// Performs the CPU Halt instruction.
    Halt,
    /// Turns on interrupts in the CPU.
    EnableInterrupts {
        /// If true, sets interrupts on immediately (e.g. `reti`), otherwise sets
        /// interrupts after the next instruction (e.g. `ei`).
        immediate: bool,
    },
    DisableInterrupts,

    // Helpers for processing interrupts:
    /// Checks if the CPU is halted and loads the halted state onto the microcode stack as
    /// 1 byte.
    CheckHalt,
    /// Sets "halted" to false.
    ClearHalt,
    /// Get the set of interrupt flags which are enabled and in the interrupt vector.
    GetActiveInterrupts,
    /// Pushes the value of the Halt Bug flag onto the microcode stack, clearing the value
    /// to false.
    PopHaltBug,
    /// Gets the address of the interrupt handler for the next active and enabled
    /// interrupt from the interupt vector and clears that interrupt from the interrupt
    /// vector. Does not disable interrupts.
    PopInterrupt,
    /// Tells the CPU that interrupt_master_enable.tick should be run when the current
    /// instruction finishes. This will happen on FetchNextInstruction, whether that is
    /// triggered explicitly or by reaching the end of the current instruction.
    TickImeOnEnd,

    // Internal microcode flow control.
    /// Unconditionally skip the given number of microcode steps.
    ///
    /// Skips the microcode pc forward by this number of steps. Note that the microcode pc
    /// is already incremented for the skip instruction, so that is not counted when
    /// figuring out how many steps to skip.
    ///
    /// Note: must not skip farther than 1 instruction past the end of the Instruction or
    /// the CPU will panic.
    Skip {
        steps: usize,
    },
    /// Conditionally skip the given number of microcode steps.
    ///
    /// Pops an 8 bit value off the microcode stack, and if it is non-zero, skips the
    /// microcode pc forward by this number of steps. Note that the microcode pc is
    /// already incremented for the skip instruction, so that is not counted when figuring
    /// out how many steps to skip.
    ///
    /// Note: must not skip farther than 1 instruction past the end of the Instruction or
    /// the CPU will panic.
    SkipIf {
        steps: usize,
    },

    // Helpers for moving between instructions.
    /// Replace the currently executing instruction with the microcode for the CPU's
    /// internal halt check, interrupt handler, and instruction fetch.
    ///
    /// Executing this instruction also checks if `previous_ime` is set, and if so, ticks
    /// the IME state forward.
    ///
    /// It is not necessary to include this in every instruction, as the CPU will perform
    /// this automatically if it runs out of steps in the currently executing instruction.
    /// This can be useful as a 'break' or 'return' from within an instruction.
    FetchNextInstruction,
    // Pop an 8 bit value off the microcode stack and look it up the the opcode table.
    // Replace the currently executing instruction with that instruction.
    ParseOpcode,
    // Pop an 8 bit value off the microcode stack and look it up the the CB opcode table.
    // Replace the currently executing instruction with that instruction.
    ParseCBOpcode,

    /*
     * Special microcode stack manipulation instructions.
     */
    /// Duplicate this number of bytes on the top of the microcode stack.
    Dup(usize),
    /// Swap two values on the microcode stack. Top and second are the number of bytes
    /// from the top of the stack and the number being swapped with.
    Swap {
        /// This is the size of value which is initially on the top of the stack. It will
        /// end up below the second_val.
        top: usize,
        /// This is the size of the value which is initially second in the stack. It will
        /// end up on top.
        second: usize,
    },
    /// Discard this number of bytes on the top of the microcode stack.
    Discard(usize),
}

impl Microcode {
    /// Convert a single microcode instruction into a single-instruction builder.
    pub fn to_builder(self) -> MicrocodeBuilder {
        MicrocodeBuilder::first(self)
    }

    #[cfg(feature = "microcode")]
    pub(super) fn eval(self, ctx: &mut impl CpuContext) -> MicrocodeFlow {
        match self {
            Self::Yield => return MicrocodeFlow::Yield1m,
            Self::ReadReg8(reg) => reg.read(ctx),
            Self::WriteReg8(reg) => reg.write(ctx),
            Self::ReadMem8 => ops::read_mem8(ctx),
            Self::WriteMem8 => ops::write_mem8(ctx),
            Self::Append(val) => ctx.cpu_mut().microcode_stack.pushu8(val),
            Self::GetFlagsMasked { mask } => ops::get_flags_masked(ctx, mask),
            Self::SetFlagsMasked { mask } => ops::set_flags_masked(ctx, mask),
            Self::BinaryOp(op) => op.eval(ctx),
            Self::UnaryOp(op) => op.eval(ctx),
            Self::BitOp(op) => op.eval(ctx),
            Self::ReadReg16(reg) => reg.read(ctx),
            Self::WriteReg16(reg) => reg.write(ctx),
            Self::Inc16 => ops::inc16(ctx),
            Self::Dec16 => ops::dec16(ctx),
            Self::Add16 => ops::add16(ctx),
            Self::OffsetAddr => ops::offset_addr(ctx),
            Self::Stop => panic!("STOP is bizarre and complicated and not implemented."),
            Self::Halt => halt(ctx),
            Self::EnableInterrupts { immediate } => ops::enable_interrupts(ctx, immediate),
            Self::DisableInterrupts => ctx.cpu_mut().interrupt_master_enable.clear(),
            Self::CheckHalt => ops::check_halt(ctx),
            Self::ClearHalt => ctx.cpu_mut().halted = false,
            Self::GetActiveInterrupts => ops::get_active_interrupts(ctx),
            Self::PopHaltBug => ops::pop_halt_bug(ctx),
            Self::PopInterrupt => ops::pop_interrupt(ctx),
            Self::TickImeOnEnd => ops::tick_ime_on_end(ctx),
            Self::Skip { steps } => ops::skip(ctx, steps),
            Self::SkipIf { steps } => ops::skip_if(ctx, steps),
            Self::FetchNextInstruction => ops::fetch_next_instruction(ctx),
            Self::ParseOpcode => ops::parse_opcode(ctx),
            Self::ParseCBOpcode => ops::parse_cb_opcode(ctx),
            Self::Dup(count) => ctx.cpu_mut().microcode_stack.ustack_dup(count),
            Self::Swap { top, second } => ctx.cpu_mut().microcode_stack.ustack_swap(top, second),
            Self::Discard(count) => ctx.cpu_mut().microcode_stack.ustack_discard(count),
        }
        MicrocodeFlow::Continue
    }
}

/// Contains implementations of microcode operations. Only needed when in microcode mode.
#[cfg(feature = "microcode")]
mod ops {
    use std::mem;

    use super::Instr;
    use crate::gbz80core::{CBOpcode, CpuContext, Flags, Opcode};
    use crate::interrupts::Interrupts;
    use crate::memdev::{Addr, MemDevice};

    /// Pop a 16 bit address and use it to read an 8 bit value from memory onto the stack.
    pub(super) fn read_mem8(ctx: &mut impl CpuContext) {
        let addr = Addr::from(ctx.cpu_mut().microcode_stack.popu16());
        let val = ctx.mem().read(addr);
        ctx.cpu_mut().microcode_stack.pushu8(val);
    }

    /// Pop a 16 bit address and an 8 bit value and write the value to the address.
    pub(super) fn write_mem8(ctx: &mut impl CpuContext) {
        let addr = Addr::from(ctx.cpu_mut().microcode_stack.popu16());
        let val = ctx.cpu_mut().microcode_stack.popu8();
        ctx.mem_mut().write(addr, val);
    }

    /// Internal implementation of microcode to fetch masked flags to the output.
    pub(super) fn get_flags_masked(ctx: &mut impl CpuContext, mask: Flags) {
        let flags = ctx.cpu().regs.flags.intersection(mask);
        ctx.cpu_mut().microcode_stack.pushu8(flags.bits());
    }

    /// Internal implementation of microcode to apply masked flags to the output.
    pub(super) fn set_flags_masked(ctx: &mut impl CpuContext, mask: Flags) {
        let flags = Flags::from_bits_truncate(ctx.cpu_mut().microcode_stack.popu8());
        ctx.cpu_mut().regs.flags.merge(flags, mask);
    }

    /// Internal implementation of microcode u16 increment.
    pub(super) fn inc16(ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu16();
        ctx.cpu_mut().microcode_stack.pushu16(val.wrapping_add(1));
    }

    /// Internal implementation of microcode u16 decrement.
    pub(super) fn dec16(ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu16();
        ctx.cpu_mut().microcode_stack.pushu16(val.wrapping_sub(1));
    }

    /// Internal implementation of microcode u16 add.
    pub(super) fn add16(ctx: &mut impl CpuContext) {
        let lhs = ctx.cpu_mut().microcode_stack.popu16();
        let rhs = ctx.cpu_mut().microcode_stack.popu16();

        let mut flags = Flags::empty();
        if (lhs & 0x7ff) + (rhs & 0x7ff) > 0x7ff {
            flags |= Flags::HALFCARRY;
        }
        let (res, carry) = lhs.overflowing_add(rhs);
        flags |= Flags::check_carry(carry);

        ctx.cpu_mut().microcode_stack.pushu16(res);
        ctx.cpu_mut().microcode_stack.pushu8(flags.bits());
    }

    pub(super) fn offset_addr(ctx: &mut impl CpuContext) {
        use crate::gbz80core::oputils::offset_addr as offset_internal;
        let cpu = ctx.cpu_mut();

        let addr = cpu.microcode_stack.popu16();
        let offset = cpu.microcode_stack.popu8() as i8;

        let (res, flags) = offset_internal(addr, offset);
        cpu.microcode_stack.pushu16(res);
        cpu.microcode_stack.pushu8(flags.bits());
    }

    /// Internal implementation of the microcode enable interrupts instruction. Runs either
    /// set or set_next_instruction, depending on whether the interrupt is to be set
    /// immeidately or not.
    pub(super) fn enable_interrupts(ctx: &mut impl CpuContext, immediate: bool) {
        if immediate {
            ctx.cpu_mut().interrupt_master_enable.set();
        } else {
            ctx.cpu_mut().interrupt_master_enable.set_next_instruction();
        }
    }

    /// Pushes the value of whether the CPU is halted onto the microcode stack as a u8.
    pub(super) fn check_halt(ctx: &mut impl CpuContext) {
        let halted = ctx.cpu().halted as u8;
        ctx.cpu_mut().microcode_stack.pushu8(halted);
    }

    /// Pushes the set of interrupts which are both active and enabled onto the microcode
    /// stack.
    pub(super) fn get_active_interrupts(ctx: &mut impl CpuContext) {
        let active = ctx.interrupts().active().bits();
        ctx.cpu_mut().microcode_stack.pushu8(active);
    }

    /// Pushes the value of the halt_bug flag onto the microcode stack, clearing the value.
    pub(super) fn pop_halt_bug(ctx: &mut impl CpuContext) {
        let halt_bug = mem::replace(&mut ctx.cpu_mut().halt_bug, false) as u8;
        ctx.cpu_mut().microcode_stack.pushu8(halt_bug);
    }

    /// Pushes the destination address of the next active and enabled interrupt onto the
    /// microcode stack and clears that interrupt. Panics if no interrupts are active!.
    pub(super) fn pop_interrupt(ctx: &mut impl CpuContext) {
        match ctx.interrupts().active().iter().next() {
            Some(interrupt) => {
                ctx.interrupts_mut().clear(interrupt);
                ctx.cpu_mut().microcode_stack.pushu16(interrupt.handler_addr());
            }
            None => panic!("Must not use the PopInterrupt microcode instruction if there are no active interrupts."),
        }
    }

    /// Enables IME ticking on the next `FetchNextInstruction`.
    pub(super) fn tick_ime_on_end(ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        debug_assert!(
            cpu.prev_ime.is_none(),
            "prev_ime is already set to {:?}, trying to set {:?}",
            cpu.prev_ime.unwrap(),
            cpu.interrupt_master_enable
        );
        cpu.prev_ime = Some(cpu.interrupt_master_enable);
    }

    /// Skip the CPU forward by this number of microcode instruction steps.
    pub(super) fn skip(ctx: &mut impl CpuContext, steps: usize) {
        // Only checked for overflow in debug.
        ctx.cpu_mut().microcode_pc += steps;
        debug_assert!(ctx.cpu().microcode_pc <= ctx.cpu().instruction.len());
    }

    /// Skip the CPU forward by this number of microcode instruction steps if the u8 value on
    /// top of the microcode stack is non-zero.
    pub(super) fn skip_if(ctx: &mut impl CpuContext, steps: usize) {
        let cond = ctx.cpu_mut().microcode_stack.popu8();
        if cond != 0 {
            // Only checked for overflow in debug.
            ctx.cpu_mut().microcode_pc += steps;
            debug_assert!(ctx.cpu().microcode_pc <= ctx.cpu().instruction.len());
        }
    }

    /// Resets to the CPU Internal instruction.
    pub(super) fn fetch_next_instruction(ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        debug_assert!(
            cpu.microcode_stack.is_empty(),
            "Previous Instruction {} failed to empty its stack",
            cpu.instruction.label()
        );
        if let Some(prev_ime) = cpu.prev_ime.take() {
            cpu.interrupt_master_enable.tick(prev_ime);
        }
        cpu.microcode_pc = 0;
        cpu.instruction = Instr::internal_fetch();
    }

    /// Parses a regular opcode from the microcode stack and replaces the current
    /// instruction with it.
    pub(super) fn parse_opcode(ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        let opcode = cpu.microcode_stack.popu8();
        debug_assert!(
            cpu.microcode_stack.is_empty(),
            "Previous Instruction {} failed to empty its stack",
            cpu.instruction.label()
        );
        cpu.microcode_pc = 0;
        cpu.instruction = Opcode::get_instruction(opcode);
    }

    /// Parses a cb opcode from the microcode stack and replaces the current
    /// instruction with it.
    pub(super) fn parse_cb_opcode(ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        let opcode = cpu.microcode_stack.popu8();
        debug_assert!(
            cpu.microcode_stack.is_empty(),
            "Previous Instruction {} failed to empty its stack",
            cpu.instruction.label()
        );
        cpu.microcode_pc = 0;
        cpu.instruction = CBOpcode::get_instruction(opcode);
    }
}

/// These are the binary operations available in the microcode. They largely correspond to
/// operations available in the ALU on the gameboy. All of these operations pop two values
/// off of the microcode stack and push their result followed by their resulting flags
/// onto the microcode stack.
///
/// Some of these operations automatically read from the flags register, however they do
/// not modify the flags register. To do that, you have to use an ApplyFlags operation
/// afterwards, which allows you to mask the flags as needed.
///
/// For order-dependent operations, the first operand (left hand side) is the top of the
/// microcode stack and the second operand (right hand side) is the second value on the
/// microcide stack. For example, if using
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    AddCarry,
    Sub,
    SubCarry,
    And,
    Xor,
    Or,
}

impl BinaryOp {
    #[cfg(feature = "microcode")]
    fn eval(self, ctx: &mut impl CpuContext) {
        let cpu = ctx.cpu_mut();
        let lhs = cpu.microcode_stack.popu8();
        let rhs = cpu.microcode_stack.popu8();
        let (res, flags) = match self {
            Self::Add => add8_flags(lhs, rhs),
            Self::AddCarry => {
                let (mut res, mut flags) = add8_flags(lhs, rhs);
                if cpu.regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = add8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second add had a result of zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                (res, flags)
            }
            Self::Sub => sub8_flags(lhs, rhs),
            Self::SubCarry => {
                let (mut res, mut flags) = sub8_flags(lhs, rhs);
                if cpu.regs.flags.contains(Flags::CARRY) {
                    let (res2, flags2) = sub8_flags(res, 1);
                    res = res2;
                    // Zero flag should only be set if the second subtract had a result of
                    // zero.
                    flags = flags2 | (flags - Flags::ZERO);
                }
                (res, flags)
            }
            Self::And => {
                let res = lhs & rhs;
                let flags = Flags::HALFCARRY | Flags::check_zero(res);
                (res, flags)
            }
            Self::Or => {
                let res = lhs | rhs;
                let flags = Flags::check_zero(res);
                (res, flags)
            }
            Self::Xor => {
                let res = lhs ^ rhs;
                let flags = Flags::check_zero(res);
                (res, flags)
            }
        };
        cpu.microcode_stack.pushu8(res);
        cpu.microcode_stack.pushu8(flags.bits);
    }
}

impl From<BinaryOp> for MicrocodeBuilder {
    fn from(op: BinaryOp) -> Self {
        Microcode::BinaryOp(op).into()
    }
}

/// These are the unary operations available in the microcode. They largely correspond to
/// operations available in the ALU on the gameboy, including CB prefix opcodes. All of
/// these operations pop one value off of the microcode stack and push their result
/// followed by their resulting flags onto the microcode stack.
///
/// Some of these operations automatically read from the flags register, however they do
/// not modify the flags register. To do that, you have to use an ApplyFlags operation
/// afterwards, which allows you to mask the flags as needed.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    /// 8-bit left rotate. Bit 7 goes to both the carry and bit 0.
    /// Note: Matches CB opcode by setting the zero flag. For non-CB opcodes, you need to
    /// clear the zero flag.
    RotateLeft8,
    /// 9-bit left rotate. Bit 7 goes to carry and carry goes to bit 0.
    /// Note: Matches CB opcode by setting the zero flag. For non-CB opcodes, you need to
    /// clear the zero flag.
    RotateLeft9,
    /// 8-bit right rotate. Bit 0 goes to both the carry and bit 7.
    /// Note: Matches CB opcode by setting the zero flag. For non-CB opcodes, you need to
    /// clear the zero flag.
    RotateRight8,
    /// 9-bit left rotate. Bit 0 goes to carry and carry goes to bit 7.
    /// Note: Matches CB opcode by setting the zero flag. For non-CB opcodes, you need to
    /// clear the zero flag.
    RotateRight9,
    /// Helper for doing binary-coded-decimal. Adjusts the hex didgits to keep both nybbles in range
    /// 0..=9 by adding 0x06 and/or 0x60 to push the digit to the next nybble.
    DecimalAdjust,
    /// Inverts the Value.
    Compliment,
    /// Shift left. Bit 7 gotes to carry, and 0 fills in Bit 0.
    ShiftLeft,
    /// Shift right. Bit 0 goes to carry, and 0 fills in Bit 7.
    ShiftRight,
    /// Shift right with sign-extension. Bit 0 goes to carry, and Bit 7 is copied with its current
    /// value.
    ShiftRightSignExt,
    /// Swap the nybbles of the byte.
    Swap,
}

impl UnaryOp {
    #[cfg(feature = "microcode")]
    fn eval(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu8();
        let (res, flags) = match self {
            Self::RotateLeft8 => {
                let res = val.rotate_left(1);
                let flags = Flags::check_zero(res) | Flags::check_carry(res & 1 != 0);
                (res, flags)
            }
            Self::RotateLeft9 => rotate_left9(val, ctx.cpu().regs.flags),
            Self::RotateRight8 => {
                let res = val.rotate_right(1);
                let flags = Flags::check_zero(res) | Flags::check_carry(res & 0x80 != 0);
                (res, flags)
            }
            Self::RotateRight9 => rotate_right9(val, ctx.cpu().regs.flags),
            Self::DecimalAdjust => {
                let inflags = ctx.cpu().regs.flags;
                // Always clears HALFCARRY.
                let mut flags = Flags::empty();
                let mut res = val;
                if inflags.contains(Flags::SUB) {
                    if inflags.contains(Flags::CARRY) {
                        flags |= Flags::CARRY;
                        res = res.wrapping_sub(0x60);
                    }
                    if inflags.contains(Flags::HALFCARRY) {
                        res = res.wrapping_sub(0x06);
                    }
                } else {
                    if inflags.contains(Flags::CARRY) || val > 0x99 {
                        flags |= Flags::CARRY;
                        res = res.wrapping_add(0x60);
                    }
                    if inflags.contains(Flags::HALFCARRY) || res & 0xf > 9 {
                        res = res.wrapping_add(0x06);
                    }
                }
                flags |= Flags::check_zero(res);
                (res, flags)
            }
            Self::Compliment => {
                const FLAGS: Flags = Flags::SUB.union(Flags::HALFCARRY);
                let res = !val;
                (res, FLAGS)
            }
            Self::ShiftLeft => {
                let res = val << 1;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 0x80 != 0);
                (res, flags)
            }
            Self::ShiftRight => {
                let res = val >> 1;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
                (res, flags)
            }
            Self::ShiftRightSignExt => {
                let res = ((val as i8) >> 1) as u8;
                let flags = Flags::check_zero(res) | Flags::check_carry(val & 1 != 0);
                (res, flags)
            }
            Self::Swap => {
                let res = ((val & 0x0f) << 4) | ((val & 0xf0) >> 4);
                let flags = Flags::check_zero(res);
                (res, flags)
            }
        };
        ctx.cpu_mut().microcode_stack.pushu8(res);
        ctx.cpu_mut().microcode_stack.pushu8(flags.bits());
    }
}

impl From<UnaryOp> for MicrocodeBuilder {
    fn from(op: UnaryOp) -> Self {
        Microcode::UnaryOp(op).into()
    }
}

/// Bit operations for the microcode. These are separate from the UnaryOp because they
/// have a different I/O pattern. UnaryOp operations always take one input + CPU flags and
/// produce two outputs (res, flags) with the new flags on top of the stack, while BitOps
/// always produce only one output. For SetBit or ResetBit, the output is the modified
/// value, for TestBit, the output is the resulting flags.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BitOp {
    /// Check if the given bit (given as an index in range 0..=7) is set in the operand.
    TestBit(u8),
    /// Sets the given bit (given as an index in range 0..=7) in the operand.
    SetBit(u8),
    /// Clears the given bit (given as an index in range 0..=7) in the operand.
    ResetBit(u8),
}

impl BitOp {
    #[cfg(feature = "microcode")]
    fn eval(self, ctx: &mut impl CpuContext) {
        let val = ctx.cpu_mut().microcode_stack.popu8();
        let res = match self {
            Self::TestBit(bit) => {
                let flags = Flags::check_zero(val & (1 << bit)) | Flags::HALFCARRY;
                flags.bits()
            }
            Self::SetBit(bit) => val | (1 << bit),
            Self::ResetBit(bit) => val & !(1 << bit),
        };
        ctx.cpu_mut().microcode_stack.pushu8(res);
    }
}

impl From<BitOp> for MicrocodeBuilder {
    fn from(op: BitOp) -> Self {
        Microcode::BitOp(op).into()
    }
}

/// Helper to build the internal fetch routine.
///
/// When built into an instruction, handles checking if the CPU is halted, then services
/// interrupts, then loads and executes an instruction.
pub struct InternalFetch;

impl From<InternalFetch> for MicrocodeBuilder {
    fn from(_: InternalFetch) -> Self {
        // Check if halted (which may just yield and return).
        MicrocodeBuilder::first(HandleHalt)
            // Service interrupts (which may jump to an interrupt and return).
            .then(ServiceInterrupt)
            // Tell the CPU we are about to process a real instruction now, so we should
            // tick the IME after this.
            .then(Microcode::TickImeOnEnd)
            // If we didn't service an interrupt and aren't halted, load and execute an
            // opcode.
            // stack: ...|
            // Load the instruction onto the stack.
            // stack: ...|opcode|
            .then_read(Immediate8)
            // Pop the opcode off the stack and parse it, replacing this instruction with
            // that opcode.
            // stack: ...|
            .then(Microcode::ParseOpcode)
    }
}

/// Helper to build microcode to handle if the CPU is halted.
pub struct HandleHalt;

impl From<HandleHalt> for MicrocodeBuilder {
    fn from(_: HandleHalt) -> Self {
        // stack: ...|
        // Load the halt flag onto the microcode stack.
        // stack: ...|halt|
        MicrocodeBuilder::first(Microcode::CheckHalt)
            // Branch based on if the value is true:
            // stack: ...|
            .then(MicrocodeBuilder::if_true(
                // Read the set of active+enabled interrupts.
                // stack: ...|int |
                MicrocodeBuilder::first(Microcode::GetActiveInterrupts)
                    // Branch based on if there are any active interrupts.
                    // stack: ...|
                    .then(MicrocodeBuilder::cond(
                        // If there are active interrupts, clear the halt.
                        Microcode::ClearHalt,
                        // Otherwise, yield and restart the instruction fetch routine.
                        // Yield is needed here because we haven't fetched an instruction
                        // yet, so we need to wait for another 1m tick without fetching.
                        MicrocodeBuilder::r#yield()
                            // This acts like a return, ending the current Instruction.
                            .then(Microcode::FetchNextInstruction),
                    )),
            ))
    }
}

/// Helper to build microcode to service an interrupt.
///
/// Checks if an interrupt should be serviced, and if so performs the hidden isr
/// instruction to jump to the interrupt handler. Runs FetchNextInstruction when done to
/// reset and load the instruction at the microcode handler address
pub struct ServiceInterrupt;

impl From<ServiceInterrupt> for MicrocodeBuilder {
    fn from(_: ServiceInterrupt) -> Self {
        // stack: ...|
        // Read the set of active interrupts:
        // stack: ...|ai  |
        MicrocodeBuilder::first(Microcode::GetActiveInterrupts)
            // Check if there are any active interrupts.
            // stack: ...|
            .then(MicrocodeBuilder::if_true(
                // If there are:
                // Pop the first interrupt from the interrupt vector and push the
                // interrupt's target address onto the microcode stack.
                // stack: ...|intl|inth|
                MicrocodeBuilder::first(Microcode::PopInterrupt)
                    // Turn off IME to pervent further interrupts.
                    // stack: ...|intl|inth|
                    .then(Microcode::DisableInterrupts)
                    // Wait two cycles.
                    .then_yield()
                    .then_yield()
                    // Get the current program counter.
                    // stack: ...|intl|inth|pcl |pch |
                    .then_read(Reg16::Pc)
                    // Get whether the halt-bug is active.
                    // stack: ...|intl|inth|pcl |pch |hb  |
                    .then(Microcode::PopHaltBug)
                    // Branch based on whether the halt-bug is active.
                    // stack: ...|intl|inth|pcl |pch |
                    // If true, decrement the PC, otherwise leave PC as-is.
                    // stack: ...|intl|inth|pcl |pch |
                    .then(MicrocodeBuilder::if_true(Microcode::Dec16))
                    // Push the (possibly decremented) PC value onto the gameboy stack.
                    // stack: ...|intl|inth|
                    .then_write(GbStack16)
                    // With the stack write and two yields, we are now at 4 cycles, we
                    // need to take 5 total cycles, so delay again here.
                    .then_yield()
                    // Now write the interrut destination to the program counter.
                    // stack: ...|
                    .then_write(Reg16::Pc)
                    // Now restart from the beginning at the interrupt address.
                    // This acts like a return/break and prevents anything else in the
                    // current Instruction from running (if ServiceInterrupt is used as
                    // part of a larger instruction).
                    .then(Microcode::FetchNextInstruction),
            ))
    }
}
