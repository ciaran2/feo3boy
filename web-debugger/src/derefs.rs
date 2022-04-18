use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::memdev::MemDevice;
use yew::prelude::*;

use crate::instrs::Instr;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub gb: Rc<Gb>,
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
        let gb = &ctx.props().gb;
        let regs = &gb.cpustate.regs;
        let mem = &gb.mmu;

        let bcu8 = mem.read(regs.bc().into());
        let deu8 = mem.read(regs.de().into());
        let hlu8 = mem.read(regs.hl().into());
        let ffc = mem.read((0xff00 + regs.c as u16).into());

        let spu16 = u16::from_le_bytes([
            mem.read(regs.sp.into()),
            mem.read(regs.sp.wrapping_add(1).into()),
        ]);

        html! {
            <div class="Derefs column nogap">
                <h3>{"Pointers & Instructions"}</h3>
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
                            <td>{"(SP)"}</td>
                            <td class="num">{format!("{:04x}", spu16)}</td>
                            <td class="num dec">{spu16}</td>
                        </tr>
                        <tr>
                            <td>{"(PC)"}</td>
                            <td colspan={2} class="instr">
                                <Instr gb={gb.clone()} addr={regs.pc} />
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        }
    }
}
