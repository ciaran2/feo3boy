use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::gbz80core::{CBOpcode, ConditionCode, Opcode, Operand16, Operand8};
use feo3boy::memdev::MemDevice;
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub gb: Rc<Gb>,
    /// Address of the address to interpret as an instruction.
    pub addr: u16,
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
        let addr = ctx.props().addr;
        let mem = &ctx.props().gb.mmu;
        let regs = &ctx.props().gb.cpustate.regs;

        // CB opcodes never use immediates, so immediates are always the next byte.
        let imm8 = || mem.read(addr.wrapping_add(1).into());
        let imm16 = || {
            u16::from_le_bytes([
                mem.read(addr.wrapping_add(1).into()),
                mem.read(addr.wrapping_add(2).into()),
            ])
        };
        let reladdr = || 0xff00 + imm8() as u16;

        let opcode = Opcode::decode(mem.read(addr.into()));
        // TODO: Extract detail from more opcodes, add popups and similar for more detail.
        match opcode {
            Opcode::JumpRelative(ConditionCode::Unconditional) => html! {<>
                <span>{"JR"}</span>{" "}
                <span>{imm8() as i8}</span>
            </>},
            Opcode::JumpRelative(cond) => html! {<>
                <span>{"JR"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{imm8() as i8}</span>
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
                    <span>{format!("({:04x}h)", reladdr())}</span>{","}
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
                    <span>{format!("({:04x}h)", reladdr())}</span>
                </>}
            }
            Opcode::Load8 {
                dest: Operand8::AddrImmediate,
                source,
            } => {
                html! {<>
                    <span>{"LD"}</span>{" "}
                    <span>{format!("({:04x}h)", imm16())}</span>{","}
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
                    <span>{format!("({:04x}h)", imm16())}</span>
                </>}
            }

            Opcode::Load8 {
                dest,
                source: Operand8::Immediate,
            } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{dest}</span>{","}
                <span>{format!("{:02x}h", imm8())}</span>
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
                <span>{format!("{:04x}h", imm16())}</span>
            </>},
            Opcode::Load16 {
                dest: Operand16::AddrImmediate,
                source,
            } => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{format!("({:04x}h)", imm16())}</span>{","}
                <span>{source}</span>
            </>},
            Opcode::AluOp {
                op,
                operand: Operand8::Immediate,
            } => html! {<>
                <span>{op}</span>{" "}
                <span>{"A"}</span>{","}
                <span>{format!("{:02x}h", imm8())}</span>
            </>},
            Opcode::AluOp { op, operand } => html! {<>
                <span>{op}</span>{" "}
                <span>{"A"}</span>{","}
                <span>{operand}</span>
            </>},
            Opcode::Call(ConditionCode::Unconditional) => html! {<>
                <span>{"CALL"}</span>{" "}
                <span>{format!("{:04x}h", imm16())}</span>
            </>},
            Opcode::Call(cond) => html! {<>
                <span>{"CALL"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{format!("{:04x}h", imm16())}</span>
            </>},
            Opcode::Jump(ConditionCode::Unconditional) => html! {<>
                <span>{"JP"}</span>{" "}
                <span>{format!("{:04x}h", imm16())}</span>
            </>},
            Opcode::Jump(cond) => html! {<>
                <span>{"JP"}</span>{" "}
                <span>{cond}</span>{","}
                <span>{format!("{:04x}h", imm16())}</span>
            </>},
            Opcode::PrefixCB => {
                let cbopcode = CBOpcode::decode(imm8());
                html! {<span>{cbopcode}</span>}
            }
            Opcode::OffsetSp => html! {<>
                <span>{"ADD"}</span>{" "}
                <span>{"SP"}</span>{","}
                <span>{imm8() as i8}</span>
            </>},
            Opcode::AddressOfOffsetSp => html! {<>
                <span>{"LD"}</span>{" "}
                <span>{"HL"}</span>{","}
                <span>{"SP"}</span>
                <span>{format!("{:+}", imm8() as i8)}</span>
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
                html! {<span>{opcode}</span>}
            }
        }
    }
}
