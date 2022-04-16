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
        let (i8sign, i8mag) = match immediate8 as i8 {
            -128..=-1 => ("-", (immediate8 as i8).abs_diff(0)),
            0..=127 => ("+", immediate8),
        };
        let immediate16 =
            u16::from_le_bytes([immediate8, mem.read(immediate_addr.wrapping_add(1).into())]);

        let jrdest = regs
            .pc
            .wrapping_add(2)
            .wrapping_add(immediate8 as i16 as u16);
        let ffval = mem.read((0xff00 + immediate8 as u16).into());

        let spu8 = mem.read(regs.sp.into());
        let spu16 = u16::from_le_bytes([spu8, mem.read(regs.sp.wrapping_add(1).into())]);

        html! {
            <div class="Derefs column nogap">
                <div class="row">
                    <h3>{"Pointers & Instructions"}</h3>
                </div>
                <div class="row">
                    <div class="column">
                        <table class="double">
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
                                <tr>
                                    <td>{"(SP 8)"}</td>
                                    <td class="num">{format!("{:02x}", spu8)}</td>
                                    <td class="num dec">{spu8}</td>
                                </tr>
                                <tr>
                                    <td>{"(SP 16)"}</td>
                                    <td class="num">{format!("{:04x}", spu16)}</td>
                                    <td class="num dec">{spu16}</td>
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
                                    <td>{"i8"}</td>
                                    <td class="num">{format!("{}{:02x}", i8sign, i8mag)}</td>
                                    <td class="num dec">{immediate8 as i8}</td>
                                </tr>
                                <tr>
                                    <td>{"JR"}</td>
                                    <td class="num">{format!("{:04x}", jrdest)}</td>
                                    <td class="num dec">{jrdest}</td>
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
            </div>
        }
    }
}
