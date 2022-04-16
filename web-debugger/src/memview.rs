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

pub struct Memview {}

impl Component for Memview {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Memview {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <div class="Memview">
                <h1>{"TODO: Memory View"}</h1>
            </div>
        }
    }
}
