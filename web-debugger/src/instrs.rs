use feo3boy::memdev::MemDevice;
use feo3boy_opcodes::opcode::args::{ConditionCode, Operand16, Operand8};
use feo3boy_opcodes::opcode::{CBOpcode, Opcode};
use yew::prelude::*;

/// Info needed to show instruction disassembly.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct InstrInfo {
    /// Opcode at the instruction.
    opcode: Opcode,
    /// Immediate bytes. Present regardless of whether the opcode uses an immediate. If CB
    /// prefix, first byte is the cb opcode.
    imm: [u8; 2],
}

impl InstrInfo {
    pub fn fetch_at(mem: &impl MemDevice, addr: u16) -> Self {
        Self {
            opcode: Opcode::decode(mem.read(addr.into())),
            imm: [
                mem.read(addr.wrapping_add(1).into()),
                mem.read(addr.wrapping_add(2).into()),
            ],
        }
    }

    fn imm8(self) -> u8 {
        self.imm[0]
    }

    fn imm16(self) -> u16 {
        u16::from_le_bytes(self.imm)
    }

    fn reladdr(self) -> u16 {
        0xff00 + self.imm8() as u16
    }

    fn cbopcode(self) -> CBOpcode {
        CBOpcode::decode(self.imm8())
    }
}

#[derive(Properties, PartialEq)]
pub struct Props {
    pub instr: InstrInfo,
}

pub enum Msg {}

pub struct Instr {}

impl Component for Instr {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Instr {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <div class="Instr">
                {self.instr_spans(ctx)}
            </div>
        }
    }
}

impl Instr {
    fn instr_spans(&self, ctx: &Context<Self>) -> Html {
        let instr = ctx.props().instr;
        // TODO: Extract detail from more opcodes, add popups and similar for more detail.
        match instr.opcode {
            Opcode::JumpRelative(ConditionCode::Unconditional) => html! {<>
                <span>{"JR"}</span>{" "}
                <span>{instr.imm8() as i8}</span>
            </>},
            Opcode::JumpRelative(cond) => html! {<>
                <span>{"JR"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{instr.imm8() as i8}</span>
            </>},
            Opcode::Load8 {
                dest: Operand8::AddrRelC,
                source,
            } => {
                html! {<>
                    <span>{"LDH"}</span>{" "}
                    <span>{"(C)"}</span>{","}
                    <span>{source}</span>
                </>}
            }
            Opcode::Load8 {
                dest,
                source: Operand8::AddrRelC,
            } => {
                html! {<>
                    <span>{"LDH"}</span>{" "}
                    <span>{dest}</span>{","}
                    <span>{"(C)"}</span>
                </>}
            }
            Opcode::Load8 {
                dest: Operand8::AddrRelImmediate,
                source,
            } => {
                html! {<>
                    <span>{"LDH"}</span>{" "}
                    <span>{format!("({:04x}h)", instr.reladdr())}</span>{","}
                    <span>{source}</span>
                </>}
            }
            Opcode::Load8 {
                dest,
                source: Operand8::AddrRelImmediate,
            } => {
                html! {<>
                    <span>{"LDH"}</span>{" "}
                    <span>{dest}</span>{","}
                    <span>{format!("({:04x}h)", instr.reladdr())}</span>
                </>}
            }
            Opcode::Load8 {
                dest: Operand8::AddrImmediate,
                source,
            } => {
                html! {<>
                    <span>{"LD"}</span>{" "}
                    <span>{format!("({:04x}h)", instr.imm16())}</span>{","}
                    <span>{source}</span>
                </>}
            }
            Opcode::Load8 {
                dest,
                source: Operand8::AddrImmediate,
            } => {
                html! {<>
                    <span>{"LD"}</span>{" "}
                    <span>{dest}</span>{","}
                    <span>{format!("({:04x}h)", instr.imm16())}</span>
                </>}
            }

            Opcode::Load8 {
                dest,
                source: Operand8::Immediate,
            } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{dest}</span>{","}
                <span>{format!("{:02x}h", instr.imm8())}</span>
            </>},
            Opcode::Load8 { dest, source } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{dest}</span>{","}
                <span>{source}</span>
            </>},
            Opcode::Load16 {
                dest,
                source: Operand16::Immediate,
            } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{dest}</span>{","}
                <span>{format!("{:04x}h", instr.imm16())}</span>
            </>},
            Opcode::Load16 {
                dest: Operand16::AddrImmediate,
                source,
            } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{format!("({:04x}h)", instr.imm16())}</span>{","}
                <span>{source}</span>
            </>},
            Opcode::AluOp {
                op,
                operand: Operand8::Immediate,
            } => html! {<>
                <span>{op}</span>{" "}
                <span>{"A"}</span>{","}
                <span>{format!("{:02x}h", instr.imm8())}</span>
            </>},
            Opcode::AluOp { op, operand } => html! {<>
                <span>{op}</span>{" "}
                <span>{"A"}</span>{","}
                <span>{operand}</span>
            </>},
            Opcode::Call(ConditionCode::Unconditional) => html! {<>
                <span>{"CALL"}</span>{" "}
                <span>{format!("{:04x}h", instr.imm16())}</span>
            </>},
            Opcode::Call(cond) => html! {<>
                <span>{"CALL"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{format!("{:04x}h", instr.imm16())}</span>
            </>},
            Opcode::Jump(ConditionCode::Unconditional) => html! {<>
                <span>{"JP"}</span>{" "}
                <span>{format!("{:04x}h", instr.imm16())}</span>
            </>},
            Opcode::Jump(cond) => html! {<>
                <span>{"JP"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{format!("{:04x}h", instr.imm16())}</span>
            </>},
            Opcode::PrefixCB => {
                html! {<span>{instr.cbopcode()}</span>}
            }
            Opcode::OffsetSp => html! {<>
                <span>{"ADD"}</span>{" "}
                <span>{"SP"}</span>{","}
                <span>{instr.imm8() as i8}</span>
            </>},
            Opcode::AddressOfOffsetSp => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{"HL"}</span>{","}
                <span>{"SP"}</span>
                <span>{format!("{:+}", instr.imm8() as i8)}</span>
            </>},
            Opcode::JumpHL => html! {<>
                <span>{"JP"}</span>{" "}
                <span>{"HL"}</span>
            </>},
            Opcode::Reset(target) => html! {<>
                <span>{"RST"}</span>{" "}
                <span>{format!("{:02X}h", target)}</span>
            </>},
            _ => {
                html! {<span>{instr.opcode}</span>}
            }
        }
    }
}
