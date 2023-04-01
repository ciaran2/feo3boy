use feo3boy::gb::Gb;
use feo3boy::memdev::RootMemDevice;
use yew::prelude::*;

use crate::instrs::{Instr, InstrInfo};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ComputedDerefs {
    /// Value pointed at by BC
    pbc: u8,
    /// Value pointed at by DE
    pde: u8,
    /// Value pointed at by HL
    phl: u8,
    /// Value pointed at by 0xFF00+C
    pffc: u8,
    /// Value pointed at by stack pointer.
    psp: u16,
    /// Instruction under the program counter.
    ppc: InstrInfo,
}

impl From<&Gb> for ComputedDerefs {
    fn from(gb: &Gb) -> Self {
        Self {
            pbc: gb.mmu.read(gb.cpustate.regs.bc()),
            pde: gb.mmu.read(gb.cpustate.regs.de()),
            phl: gb.mmu.read(gb.cpustate.regs.hl()),
            pffc: gb.mmu.read(0xff00 + gb.cpustate.regs.c as u16),
            psp: gb.mmu.read(gb.cpustate.regs.sp),
            ppc: InstrInfo::fetch_at(&gb.mmu, gb.cpustate.regs.pc),
        }
    }
}

#[derive(Properties, PartialEq)]
pub struct Props {
    pub derefs: ComputedDerefs,
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
        let derefs = &ctx.props().derefs;
        html! {
            <div class="Derefs column nogap">
                <h3>{"Pointers & Instructions"}</h3>
                <table>
                    <thead>
                        <tr>
                            <th>{"ptr"}</th>
                            <th>{"hex"}</th>
                            <th>{"dec"}</th>
                            <th></th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>{"(BC)"}</td>
                            <td class="num">{format!("{:02x}", derefs.pbc)}</td>
                            <td class="num dec">{derefs.pbc}</td>
                        </tr>
                        <tr>
                            <td>{"(DE)"}</td>
                            <td class="num">{format!("{:02x}", derefs.pde)}</td>
                            <td class="num dec">{derefs.pde}</td>
                        </tr>
                        <tr>
                            <td>{"(HL)"}</td>
                            <td class="num">{format!("{:02x}", derefs.phl)}</td>
                            <td class="num dec">{derefs.phl}</td>
                        </tr>
                        <tr>
                            <td>{"(+C)"}</td>
                            <td class="num">{format!("{:02x}", derefs.pffc)}</td>
                            <td class="num dec">{derefs.pffc}</td>
                        </tr>
                        <tr>
                            <td>{"(SP)"}</td>
                            <td class="num">{format!("{:04x}", derefs.psp)}</td>
                            <td class="num dec">{derefs.psp}</td>
                        </tr>
                        <tr>
                            <td>{"(PC)"}</td>
                            <td colspan={3} class="instr">
                                <Instr instr={derefs.ppc} />
                            </td>
                        </tr>
                    </tbody>
                </table>
            </div>
        }
    }
}
