use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::gbz80core::Flags;
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

pub struct Regs {}

impl Component for Regs {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Regs {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let regs = &ctx.props().gb.cpustate.regs;
        html! {
            <div class="Regs row">
                <div class="column">
                    <table class="single">
                        <thead>
                            <tr>
                                <th>{"reg"}</th>
                                <th>{"hex"}</th>
                                <th>{"dec"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <td>{"A"}</td>
                                <td class="num">{format!("{:02x}", regs.acc)}</td>
                                <td class="num dec">{regs.acc}</td>
                            </tr>
                            <tr>
                                <td>{"B"}</td>
                                <td class="num">{format!("{:02x}", regs.b)}</td>
                                <td class="num dec">{regs.b}</td>
                            </tr>
                            <tr>
                                <td>{"C"}</td>
                                <td class="num">{format!("{:02x}", regs.c)}</td>
                                <td class="num dec">{regs.c}</td>
                            </tr>
                            <tr>
                                <td>{"D"}</td>
                                <td class="num">{format!("{:02x}", regs.d)}</td>
                                <td class="num dec">{regs.d}</td>
                            </tr>
                            <tr>
                                <td>{"E"}</td>
                                <td class="num">{format!("{:02x}", regs.e)}</td>
                                <td class="num dec">{regs.e}</td>
                            </tr>

                            <tr>
                                <td>{"H"}</td>
                                <td class="num">{format!("{:02x}", regs.h)}</td>
                                <td class="num dec">{regs.h}</td>
                            </tr>
                            <tr>
                                <td>{"L"}</td>
                                <td class="num">{format!("{:02x}", regs.l)}</td>
                                <td class="num dec">{regs.l}</td>
                            </tr>
                        </tbody>
                    </table>
                </div>
                <div class="column">
                    <table class="double">
                        <thead>
                            <tr>
                                <th>{"reg"}</th>
                                <th>{"hex"}</th>
                                <th>{"dec"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <td>{"BC"}</td>
                                <td class="num">{format!("{:04x}", regs.bc())}</td>
                                <td class="num dec">{regs.bc()}</td>
                            </tr>
                            <tr>
                                <td>{"DE"}</td>
                                <td class="num">{format!("{:04x}", regs.de())}</td>
                                <td class="num dec">{regs.de()}</td>
                            </tr>
                            <tr>
                                <td>{"HL"}</td>
                                <td class="num">{format!("{:04x}", regs.hl())}</td>
                                <td class="num dec">{regs.hl()}</td>
                            </tr>
                            <tr>
                                <td>{"SP"}</td>
                                <td class="num">{format!("{:04x}", regs.sp)}</td>
                                <td class="num dec">{regs.sp}</td>
                            </tr>
                            <tr>
                                <td>{"PC"}</td>
                                <td class="num">{format!("{:04x}", regs.pc)}</td>
                                <td class="num dec">{regs.pc}</td>
                            </tr>
                        </tbody>
                    </table>
                    <table class="flags">
                        <thead>
                            <tr>
                                <th>{"Z"}</th>
                                <th>{"N"}</th>
                                <th>{"H"}</th>
                                <th>{"C"}</th>
                            </tr>
                        </thead>
                        <tbody>
                            <tr>
                                <td>{regs.flags.contains(Flags::ZERO) as u8}</td>
                                <td>{regs.flags.contains(Flags::SUB) as u8}</td>
                                <td>{regs.flags.contains(Flags::HALFCARRY) as u8}</td>
                                <td>{regs.flags.contains(Flags::CARRY) as u8}</td>
                            </tr>
                        </tbody>
                    </table>
                </div>
            </div>
        }
    }
}
