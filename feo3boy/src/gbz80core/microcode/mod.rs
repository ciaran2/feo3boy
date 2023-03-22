use std::ops::Index;
use std::slice::SliceIndex;

use once_cell::sync::Lazy;

use crate::gbz80core::microcode::combocodes::ComboCode;
use crate::gbz80core::Flags;

#[cfg(feature = "microcode")]
mod r#impl;

#[cfg(feature = "microcode")]
pub use r#impl::{MicrocodeFlow, MicrocodeStack};

pub mod combocodes;

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
    #[allow(unused)]
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

    /// Executes a single combo-code as one step. Only available when full combo-code
    /// implementations are enabled with the `combo-code` feature, otherwise combo codes
    /// must be expanded to several normal `Microcode` instructions.
    #[cfg(feature = "combo-code")]
    ComboCode(ComboCode),

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
    /// Gets whether IME is currently enabled.
    CheckIme,
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
        MicrocodeBuilder::first(ComboCode::HandleHalt)
            // Service interrupts (which may jump to an interrupt and return).
            .then(ServiceInterrupt)
            // Load and execute the next instruction.
            .then(LoadAndExecute)
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
        // Read the IME:
        // stack: ...|ime |
        // And branch on it
        // stack: ...|
        MicrocodeBuilder::first(Microcode::CheckIme).then(MicrocodeBuilder::if_true(
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
                )),
        ))
    }
}

/// Helper to provide microcode for LoadAndExecute of an instruction, including halt-bug
/// handling and enabling IME state ticking.
pub struct LoadAndExecute;

impl From<LoadAndExecute> for MicrocodeBuilder {
    fn from(_: LoadAndExecute) -> Self {
        // Tell the CPU we are about to process a real instruction now, so we should tick
        // the IME after this.
        MicrocodeBuilder::first(Microcode::TickImeOnEnd)
            // If we didn't service an interrupt and aren't halted, load and execute an
            // opcode.
            // stack: ...|
            // Load the instruction onto the stack.
            // stack: ...|opcode|
            .then_read(Immediate8)
            // Before executing, check if the haltbug is triggered, and if so, decrement
            // the PC.
            // stack: ...|opcode|hb|
            .then(Microcode::PopHaltBug)
            // Pop the haltbug flag and process.
            // stack: ...|opcode|
            .then(MicrocodeBuilder::if_true(
                // If true, push the PC, decrement it, and pop it back into the PC.
                MicrocodeBuilder::read(Reg16::Pc)
                    .then(Microcode::Dec16)
                    .then_write(Reg16::Pc),
            ))
            // Pop the opcode off the stack and parse it, replacing this instruction with
            // that opcode.
            // stack: ...|
            .then(Microcode::ParseOpcode)
    }
}
