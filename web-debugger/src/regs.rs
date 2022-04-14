use std::rc::Rc;

use feo3boy::gb::Gb;
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
            <div class="Regs">
                <table>
                    <thead>
                        <tr>
                            <th>{"Reg"}</th>
                            <th>{"Val (hex)"}</th>
                            <th>{"Val (dec)"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td>{"A"}</td>
                            <td>{format!("{:#4x}", regs.acc)}</td>
                            <td>{regs.acc}</td>
                        </tr>
                        <tr>
                            <td>{"B"}</td>
                            <td>{format!("{:#4x}", regs.b)}</td>
                            <td>{regs.b}</td>
                        </tr>
                        <tr>
                            <td>{"C"}</td>
                            <td>{format!("{:#4x}", regs.c)}</td>
                            <td>{regs.c}</td>
                        </tr>
                        <tr>
                            <td>{"BC"}</td>
                            <td>{format!("{:#6x}", regs.bc())}</td>
                            <td>{regs.bc()}</td>
                        </tr>
                        <tr>
                            <td>{"D"}</td>
                            <td>{format!("{:#4x}", regs.d)}</td>
                            <td>{regs.d}</td>
                        </tr>
                        <tr>
                            <td>{"E"}</td>
                            <td>{format!("{:#4x}", regs.e)}</td>
                            <td>{regs.e}</td>
                        </tr>
                        <tr>
                            <td>{"DE"}</td>
                            <td>{format!("{:#6x}", regs.de())}</td>
                            <td>{regs.de()}</td>
                        </tr>
                        <tr>
                            <td>{"H"}</td>
                            <td>{format!("{:#4x}", regs.h)}</td>
                            <td>{regs.h}</td>
                        </tr>
                        <tr>
                            <td>{"L"}</td>
                            <td>{format!("{:#4x}", regs.l)}</td>
                            <td>{regs.l}</td>
                        </tr>
                        <tr>
                            <td>{"HL"}</td>
                            <td>{format!("{:#6x}", regs.hl())}</td>
                            <td>{regs.hl()}</td>
                        </tr>
                        <tr>
                            <td>{"SP"}</td>
                            <td>{format!("{:#6x}", regs.sp)}</td>
                            <td>{regs.sp}</td>
                        </tr>
                        <tr>
                            <td>{"PC"}</td>
                            <td>{format!("{:#6x}", regs.pc)}</td>
                            <td>{regs.pc}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        }
    }
}
