//! This is the microcode implementation of [`crate::gbz80core::opcode`]. Provides
//! converstions from [`Opcode`] to [`Microcode`].

use std::mem::{self, MaybeUninit};

use once_cell::sync::Lazy;

use crate::gbz80core::microcode::{
    BinaryOp, BitOp, GbStack16, Immediate16, Immediate8, Instr, InstrDef, Microcode,
    MicrocodeBuilder, Reg16, UnaryOp,
};
use crate::gbz80core::opcode::args::{ConditionCode, Operand16, Operand8};
use crate::gbz80core::opcode::{CBOpcode, CBOperation, Opcode};
use crate::gbz80core::Flags;

impl Opcode {
    /// Get the [`Instr`] for a particular Opcode.
    pub fn get_instruction(opcode: u8) -> Instr {
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

        Instr::new(&OPCODE_TABLE[opcode as usize])
    }
}

impl From<Opcode> for InstrDef {
    fn from(value: Opcode) -> Self {
        let builder = match value {
            Opcode::Nop => MicrocodeBuilder::new(),
            Opcode::Stop => Microcode::Stop.into(),
            Opcode::JumpRelative(cond) => jump_relative(cond),
            Opcode::Inc8(operand) => inc8(operand),
            Opcode::Dec8(operand) => dec8(operand),
            Opcode::Load8 { dest, source } => MicrocodeBuilder::read(source).then_write(dest),
            Opcode::Inc16(operand) => inc16(operand),
            Opcode::Dec16(operand) => dec16(operand),
            Opcode::Load16 { dest, source } => load16(dest, source),
            Opcode::Add16(operand) => add16(operand),
            Opcode::Halt => Microcode::Halt.into(),
            Opcode::AluOp { operand, op } => MicrocodeBuilder::read(operand).then(op),
            Opcode::AluUnary(op) => op.into(),
            Opcode::Call(cond) => call(cond),
            Opcode::Jump(cond) => jump(cond),
            Opcode::Ret(cond) => ret(cond),
            Opcode::Push(operand) => MicrocodeBuilder::read(operand)
                // Push has an extra delay before writing.
                .then_yield()
                .then_write(GbStack16),
            Opcode::Pop(operand) => MicrocodeBuilder::read(GbStack16).then_write(operand),
            Opcode::PrefixCB => MicrocodeBuilder::read(Immediate8).then(Microcode::ParseCBOpcode),
            Opcode::DisableInterrupts => Microcode::DisableInterrupts.into(),
            Opcode::EnableInterrupts => Microcode::EnableInterrupts { immediate: false }.into(),
            Opcode::RetInterrupt => interrupt_return(),
            Opcode::OffsetSp => offset_sp(),
            Opcode::AddressOfOffsetSp => address_of_offset_sp(),
            Opcode::JumpHL => MicrocodeBuilder::read(Reg16::HL).then_write(Reg16::Pc),
            Opcode::Reset(dest) => reset(dest),
            // A brief note on this doc page:
            // https://gbdev.io/pandocs/CPU_Comparison_with_Z80.html
            // says that the unused opcodes will lock up the CPU, rather than behave as a
            // no-op.
            // However, we just build them as an empty instruction, which is a no-op.
            Opcode::MissingInstruction(_) => MicrocodeBuilder::new(),
        };
        builder.build(value.to_string())
    }
}

/// Build microcode for the relative jump instruction.
pub(super) fn jump_relative(cond: ConditionCode) -> MicrocodeBuilder {
    // stack: ...|
    // Read the jump offset from the next immediate.
    // Loading the offset also moves the program counter over the next instruction, which
    // is good because the jump is relative to the following instruction.
    // stack: ...|i8  |
    MicrocodeBuilder::read(Immediate8)
        // Read the PC address of the instruction after this one onto the stack.
        // stack: ...|i8  |pcl |pch |
        .then_read(Reg16::Pc)
        // Apply the PC offset.
        // Read the PC address of the instruction after this one onto the stack.
        // stack: ...|dstl|dsth|flag|
        .then(Microcode::OffsetAddr)
        // Discard the flags since JR doesn't use them.
        // stack: ...|dstl|dsth|
        .then(Microcode::Discard(1))
        // Apply the jump only if the condition matches.
        // First yield, because there's an extra dely in JR instructions when branching,
        // then pop the destination off the stack and into the PC register.
        .then(cond.cond(
            // If the condition matches, apply it to the PC.
            MicrocodeBuilder::r#yield().then_write(Reg16::Pc),
            // If it doesn't match, discard the computed new PC value.
            Microcode::Discard(2),
        ))
}

/// Provides microcode for an 8 bit increment instruction.
pub(super) fn inc8(operand: Operand8) -> MicrocodeBuilder {
    // Inc doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
    // binary ops operate with the top of the stack being the LHS.
    // stack: ...|1|
    MicrocodeBuilder::first(Microcode::Append(1))
        // Then fetch the operand.
        // stack: ...|1|v|
        .then_read(operand)
        // Apply the operation
        // stack: ...|r|f|
        .then(BinaryOp::Add)
        // Write out the flags which are modified
        // stack: ...|r|
        .then_write(MASK)
        // Write the result back to the same operand.
        // stack: ...|
        .then_write(operand)
}

/// Provides microcode for an 8 bit decrement instruction.
pub(super) fn dec8(operand: Operand8) -> MicrocodeBuilder {
    // Dec doesn't set the carry flag.
    const MASK: Flags = Flags::all().difference(Flags::CARRY);

    // Start by putting a 1 (the right-hand-side for our ALU sub) onto the stack, since
    // binary ops operate with the top of the stack being the LHS.
    // stack: ...|1|
    MicrocodeBuilder::first(Microcode::Append(1))
        // Then fetch the operand.
        // stack: ...|1|v|
        .then_read(operand)
        // Apply the operation
        // stack: ...|r|f|
        .then(BinaryOp::Sub)
        // Write out the flags which are modified
        // stack: ...|r|
        .then_write(MASK)
        // Write the result back to the same operand.
        // stack: ...|
        .then_write(operand)
}

/// Provides microcode for a 16 bit increment instruction.
pub(super) fn inc16(operand: Operand16) -> MicrocodeBuilder {
    MicrocodeBuilder::read(operand)
        .then(Microcode::Inc16)
        // 16 bit inc doesn't set any flags, and all actual operands are always registers,
        // but it does delay by 1 additional M cycle, probably because it has to operate
        // on two bytes.
        .then(Microcode::Yield)
        .then_write(operand)
}

/// Provides microcode for a 16 bit decrement instruction.
pub(super) fn dec16(operand: Operand16) -> MicrocodeBuilder {
    MicrocodeBuilder::read(operand)
        .then(Microcode::Dec16)
        // 16 bit dec doesn't set any flags, and all actual operands are always registers,
        // but it does delay by 1 additional M cycle, probably because it has to operate
        // on two bytes.
        .then(Microcode::Yield)
        .then_write(operand)
}

/// Provides microcode for 16 bit load operations.
pub(super) fn load16(dest: Operand16, source: Operand16) -> MicrocodeBuilder {
    MicrocodeBuilder::read(source)
        .then(if (dest, source) == (Operand16::Sp, Operand16::HL) {
            // Most of the 16 bit loads are <Pair>,<Immediate> and take time based on
            // number of memory accesses. There are two exceptions. LD (u16),SP, which is
            // also just timed based on the number of memory accesses, and LD SP,HL, which
            // is all registers but still takes an extra 1m cycle, which isn't
            // automatically provided by Operand16 register interactions, so we insert it
            // here.
            Some(Microcode::Yield)
        } else {
            None
        })
        .then_write(dest)
}

/// Generates microcode for a 16 bit register add into HL. Never sets the zero flag and
/// clears the subtract flag, but does set carry and half-carry based on the upper byte of
/// the operation (as if it was performed by running the pseudo-instructions `add
/// l,<arg-low>; adc h,<arg-high>`.
pub(super) fn add16(arg: Operand16) -> MicrocodeBuilder {
    // 16 bit add never modifies the zero flag.
    const MASK: Flags = Flags::all().difference(Flags::ZERO);

    // RHS goes on the stack first.
    MicrocodeBuilder::read(arg)
        // Then LHS goes on the stack.
        .then_read(Reg16::HL)
        // This produces the unmasked flags on top of the stack with the result
        // underneath.
        .then(Microcode::Add16)
        // Add an extra delay cycle, since this op is an 8t not 4t, probably because it is
        // operating on two bytes, and that extra delay doesn't come from memory accesses.
        .then_yield()
        .then_write(MASK)
        .then_write(Reg16::HL)
}

/// Build the microcode for a conditional or unconditional call.
pub(super) fn call(cond: ConditionCode) -> MicrocodeBuilder {
    // Conveniently, unconditional call behaves exactly the same as a conditional call
    // with a true value, down to the timing.
    // First, load the destination address onto the microcode stack.
    // stack: ...|dstl|dsth|
    MicrocodeBuilder::read(Immediate16)
        // Evaluate conditionally:
        // stack: ...|dstl|dsth|
        .then(
            cond.cond(
                // If true:
                // Read the PC onto the microcode stack so we can push it to the gameboy
                // stack.
                // stack: ...|dstl|dsth|pcl |pch |
                MicrocodeBuilder::read(Reg16::Pc)
                    // Conditional jump has an extra internal delay if the condition is
                    // true before pushing the return address to the GB stack.
                    .then_yield()
                    // Write the return address to the Gameboy Stack.
                    // stack: ...|dstl|dsth|
                    .then_write(GbStack16)
                    // Write the destination of the jump to the PC.
                    // stack: ...|
                    .then_write(Reg16::Pc),
                // If false:
                // Discard the destination address from the microcode stack.
                Microcode::Discard(2),
            ),
        )
}

/// Builds microcode for a conditional absolute jump.
pub(super) fn jump(cond: ConditionCode) -> MicrocodeBuilder {
    // Conveniently, unconditional jump behaves exactly the same as a conditional jump
    // with a true value, down to the timing.

    // Fetch the destination of the jump from the immediate.
    // stack: ...|dstl|dsth|
    MicrocodeBuilder::read(Immediate16)
        // Evaluate conditionally:
        .then(
            cond.cond(
                // If true:
                // Delay by one extra cycle because branching adds an extra cycle despite not
                // accessing memory.
                MicrocodeBuilder::r#yield()
                    // Write the desintation address to the PC.
                    // stack: ...|
                    .then_write(Reg16::Pc),
                // If false:
                // Discard the destination address.
                Microcode::Discard(2),
            ),
        )
}

/// Performs a conditional return.
pub(super) fn ret(cond: ConditionCode) -> MicrocodeBuilder {
    // First do a yield if this is a conditional return. The conditional returns have an
    // extra delay at the beginning which isn't part of the unconditional return.
    MicrocodeBuilder::first(match cond {
        ConditionCode::Unconditional => None,
        _ => Some(Microcode::Yield),
    })
    // Apply the actual return only if the condition is true.
    .then(
        cond.if_true(
            // Pop the return address off the Gameboy stack and onto the microcode stack.
            // This takes two m cycles.
            // stack: ...|retl|reth|
            MicrocodeBuilder::read(GbStack16)
                // Apply the additional yield that happens after the GB stack pop and
                // before the return is applied to the program counter.
                .then_yield()
                // Pop the return address off the microcode stack and into the pc.
                .then_write(Reg16::Pc),
        ),
    )
}

/// Build microcode to enable interrupts and return.
pub(super) fn interrupt_return() -> MicrocodeBuilder {
    // Pop the return address off the GB stack and onto the microcode stack.
    MicrocodeBuilder::read(GbStack16)
        // Delay by one additional cycle since there's one extra delay in RETI.
        .then_yield()
        // Write the return address to the PC.
        .then_write(Reg16::Pc)
        // Enable interrupts immediately.
        .then(Microcode::EnableInterrupts { immediate: true })
}

/// Get microcode to offset the stack pointer by an immediate value.
pub(super) fn offset_sp() -> MicrocodeBuilder {
    // stack: ...|off|
    MicrocodeBuilder::read(Immediate8)
        // stack: ...|off|spl|sph|
        .then_read(Reg16::Sp)
        // stack: ...|SPL|SPH|flg|
        .then(Microcode::OffsetAddr)
        // This instruction takes two more cycles after loading the offset.
        .then_yield()
        .then_yield()
        // stack: ...|SPL|SPH|
        .then_write(Flags::all())
        .then_write(Reg16::Sp)
}

/// Create microcode to load the result of offsetting the stack pointer by an immediate
/// value into HL.
pub(super) fn address_of_offset_sp() -> MicrocodeBuilder {
    // stack: ...|off|
    MicrocodeBuilder::read(Immediate8)
        // stack: ...|off|spl|sph|
        .then_read(Reg16::Sp)
        // stack: ...|SPL|SPH|flg|
        .then(Microcode::OffsetAddr)
        // This instruction takes one more cycle after loading the offset.
        // Interestingly, this instruction is actually faster than `ADD SP,i8`.
        .then_yield()
        // stack: ...|SPL|SPH|
        .then_write(Flags::all())
        .then_write(Reg16::HL)
}

/// Builds microcode for the reset instruction. Similar to call with a fixed destination.
pub(super) fn reset(dest: u8) -> MicrocodeBuilder {
    // There's an extra delay at the start of an RST instruction.
    MicrocodeBuilder::r#yield()
        // Push the PC onto the gameboy stack.
        .then_read(Reg16::Pc)
        .then_write(GbStack16)
        // Set the dest address into the PC.
        // initial stack: ...|
        // Push the low order bytes of the dest address.
        // stack: ...|destl|
        .then(Microcode::Append(dest))
        // Push the high order bytes of the dest address.
        // stack: ...|destl|desth|
        .then(Microcode::Append(dest))
        // Set the PC to the specified address.
        .then_write(Reg16::Pc)
}

impl CBOpcode {
    /// Get the [`Instr`] for a particular CB Opcode.
    pub fn get_instruction(opcode: u8) -> Instr {
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

        Instr::new(&CB_OPCODE_TABLE[opcode as usize])
    }
}

impl From<CBOpcode> for InstrDef {
    fn from(value: CBOpcode) -> Self {
        // Builds microcode for the CB Opcodes that takes a value and affects flags.
        fn cb_unary_op(operand: Operand8, operator: UnaryOp) -> MicrocodeBuilder {
            // First, read the operand onto the microcode stack.
            // stack: ...|val|
            MicrocodeBuilder::read(operand)
                // Apply the operator, generating the result and flags.
                // stack: ...|res|flags|
                .then(operator)
                // Apply the flags, overwriting all flags in the register.
                .then_write(Flags::all())
                .then_write(operand)
        }
        let builder = match value.op {
            CBOperation::RotateLeft8 => cb_unary_op(value.operand, UnaryOp::RotateLeft8),
            CBOperation::RotateLeft9 => cb_unary_op(value.operand, UnaryOp::RotateLeft9),
            CBOperation::RotateRight8 => cb_unary_op(value.operand, UnaryOp::RotateRight8),
            CBOperation::RotateRight9 => cb_unary_op(value.operand, UnaryOp::RotateRight9),
            CBOperation::ShiftLeft => cb_unary_op(value.operand, UnaryOp::ShiftLeft),
            CBOperation::ShiftRight => cb_unary_op(value.operand, UnaryOp::ShiftRight),
            CBOperation::ShiftRightSignExt => {
                cb_unary_op(value.operand, UnaryOp::ShiftRightSignExt)
            }
            CBOperation::Swap => cb_unary_op(value.operand, UnaryOp::Swap),
            CBOperation::TestBit(bit) => {
                // Doesn't affect the carry flag.
                const MASK: Flags = Flags::all().difference(Flags::CARRY);
                MicrocodeBuilder::read(value.operand)
                    .then(BitOp::TestBit(bit))
                    .then_write(MASK)
            }
            CBOperation::SetBit(bit) => MicrocodeBuilder::read(value.operand)
                .then(BitOp::SetBit(bit))
                .then_write(value.operand),
            CBOperation::ResetBit(bit) => MicrocodeBuilder::read(value.operand)
                .then(BitOp::ResetBit(bit))
                .then_write(value.operand),
        };
        builder.build(value.to_string())
    }
}
