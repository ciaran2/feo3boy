use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::gbz80core::{CBOpcode, Opcode};
use feo3boy::memdev::MemDevice;
use yew::prelude::*;

#[derive(Properties)]
pub struct Props {
    pub gb: Rc<Gb>,
}

impl PartialEq for Props {
    fn eq(&self, other: &Props) -> bool {
        Rc::ptr_eq(&self.gb, &other.gb)
    }
}

pub enum Msg {}

pub struct Derefs {}

impl Component for Derefs {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Derefs {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let regs = &ctx.props().gb.cpustate.regs;
        let mem = &ctx.props().gb.mmu;

        let bcu8 = mem.read(regs.bc().into());
        let deu8 = mem.read(regs.de().into());
        let hlu8 = mem.read(regs.hl().into());
        let ffc = mem.read((0xff00 + regs.c as u16).into());

        let opcode = Opcode::decode(mem.read(regs.pc.into()));
        let instr = match opcode {
            Opcode::PrefixCB => {
                CBOpcode::decode(mem.read(regs.pc.wrapping_add(1).into())).to_string()
            }
            instr => instr.to_string(),
        };
        let immediate_addr = match opcode {
            Opcode::PrefixCB => regs.pc.wrapping_add(2),
            _ => regs.pc.wrapping_add(1),
        };
        let immediate8 = mem.read(immediate_addr.into());
        let immediate16 =
            u16::from_le_bytes([immediate8, mem.read(immediate_addr.wrapping_add(1).into())]);

        let ffval = mem.read((0xff00 + immediate8 as u16).into());
        html! {
            <div class="Derefs row">
                <div class="column nogap">
                    <h4>{"As *uint8"}</h4>
                    <table class="single">
                        <thead>
                            <tr>
                                <th>{"ptr"}</th>
                                <th>{"hex"}</th>
                                <th>{"dec"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <td>{"(BC)"}</td>
                                <td class="num">{format!("{:02x}", bcu8)}</td>
                                <td class="num dec">{bcu8}</td>
                            </tr>
                            <tr>
                                <td>{"(DE)"}</td>
                                <td class="num">{format!("{:02x}", deu8)}</td>
                                <td class="num dec">{deu8}</td>
                            </tr>
                            <tr>
                                <td>{"(HL)"}</td>
                                <td class="num">{format!("{:02x}", hlu8)}</td>
                                <td class="num dec">{hlu8}</td>
                            </tr>
                            <tr>
                                <td>{"(+C)"}</td>
                                <td class="num">{format!("{:02x}", ffc)}</td>
                                <td class="num dec">{ffc}</td>
                            </tr>
                        </tbody>
                    </table>
                </div>
                <div class="column nogap">
                    <h4>{"(PC)"}</h4>
                    <span class="instr">{instr}</span>
                    <table class="double">
                        <thead>
                            <tr>
                                <th>{"imm"}</th>
                                <th>{"hex"}</th>
                                <th>{"dec"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <td>{"u8"}</td>
                                <td class="num">{format!("{:02x}", immediate8)}</td>
                                <td class="num dec">{immediate8}</td>
                            </tr>
                            <tr>
                                <td>{"u16"}</td>
                                <td class="num">{format!("{:04x}", immediate16)}</td>
                                <td class="num dec">{immediate16}</td>
                            </tr>
                            <tr>
                                <td>{"(+u8)"}</td>
                                <td class="num">{format!("{:02x}", ffval)}</td>
                                <td class="num dec">{ffval}</td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            </div>
        }
    }
}
