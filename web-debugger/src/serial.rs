use std::rc::Rc;

use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub serial_output: Rc<String>,
}

pub enum Msg {}

pub struct Serial {}

impl Component for Serial {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Serial {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="Serial column">
            <h3>{"Serial Output"}</h3>
            <pre>{ctx.props().serial_output.clone()}</pre>
        </div>}
    }
}
