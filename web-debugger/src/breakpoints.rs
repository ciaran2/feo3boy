use std::collections::BTreeMap;
use std::rc::Rc;

use feo3boy::gb::Gb;
use log::warn;
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use yew::prelude::*;

use crate::get_value_from_input_event;

/// A breakpoint for the emulator.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Breakpoint {
    pub name: String,
    pub enabled: bool,
}

#[derive(Properties, PartialEq)]
pub struct Props {
    pub gb: Rc<Gb>,
    pub breakpoints: Rc<BTreeMap<u16, Breakpoint>>,
    pub add_breakpoint: Callback<(u16, String, bool)>,
    pub delete_breakpoint: Callback<u16>,
    pub toggle_breakpoint: Callback<(u16, bool)>,
}

pub enum Msg {
    Add,
    ChangeEnable { input: bool },
    ChangeName { input: String },
    ChangeAddr { input: String },
}

pub struct Breakpoints {
    /// Enable state for new breakpoint.
    enable_new_breakpoint: bool,
    /// Name for the new breakpoint.
    new_breakpoint_name: String,
    /// Address of the new breakpoint.
    new_breakpoint_addr: String,
    parsed_addr: Option<u16>,
}

impl Component for Breakpoints {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Breakpoints {
            enable_new_breakpoint: true,
            new_breakpoint_name: Default::default(),
            new_breakpoint_addr: "0".to_string(),
            parsed_addr: Some(0),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Add => {
                if let Some(addr) = self.parsed_addr {
                    ctx.props().add_breakpoint.emit((
                        addr,
                        self.new_breakpoint_name.clone(),
                        self.enable_new_breakpoint,
                    ));
                } else {
                    warn!("Address is not valid");
                }
            }
            Msg::ChangeEnable { input } => {
                self.enable_new_breakpoint = input;
            }
            Msg::ChangeName { input } => {
                self.new_breakpoint_name = input;
            }
            Msg::ChangeAddr { input } => {
                self.new_breakpoint_addr = input;
                static DEC: Lazy<Regex> = Lazy::new(|| Regex::new(r"^([0-9]+)$").unwrap());
                static HEX: Lazy<Regex> = Lazy::new(|| {
                    Regex::new(r"(?:^([0-9a-fA-F]+)h$)|(?:^0x([0-9a-fA-F]+)$)").unwrap()
                });
                self.parsed_addr = if let Some(cap) = DEC.captures(&*self.new_breakpoint_addr) {
                    cap.get(1).and_then(|cap| cap.as_str().parse::<u16>().ok())
                } else if let Some(cap) = HEX.captures(&*self.new_breakpoint_addr) {
                    cap.get(1)
                        .or(cap.get(2))
                        .and_then(|cap| u16::from_str_radix(cap.as_str(), 16).ok())
                } else {
                    None
                };
            }
        }
        true
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let enable_new_breakpoint = self.enable_new_breakpoint;

        let link = ctx.link();
        let change_enable = link.callback(move |_| Msg::ChangeEnable {
            input: !enable_new_breakpoint,
        });
        let change_name = link.callback(|e| Msg::ChangeName {
            input: get_value_from_input_event(e),
        });
        let change_addr = link.callback(|e| Msg::ChangeAddr {
            input: get_value_from_input_event(e),
        });
        let onsubmit = link.callback(|e: FocusEvent| {
            e.prevent_default();
            Msg::Add
        });
        html! {<div class="Breakpoints column nogap">
            <h3>{"Breakpoints"}</h3>
            <form {onsubmit}>
                <table>
                    <thead>
                        <tr>
                            <th class="enable"></th>
                            <th class="adddel"></th>
                            <th class="name">{"name"}</th>
                            <th class="addr">{"addr"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        { for ctx.props().breakpoints.iter().map(|(&addr, &Breakpoint { enabled, ref name })| {
                            let toggle = ctx.props().toggle_breakpoint.reform(move |_| (addr, !enabled));
                            let del = ctx.props().delete_breakpoint.reform(move |_| addr);
                            html! {<tr key={addr as i128}>
                                <td class="enable">
                                    <input type="checkbox" checked={enabled}
                                        onchange={toggle} />
                                </td>
                                <td class="adddel"><button type="button" onclick={del}>{"del"}</button></td>
                                <td class="name">{name}</td>
                                <td class="addr">{format!("{:04x}", addr)}</td>
                            </tr>}
                        }) }
                        <tr key="edit">
                            <td class="enable">
                                <input type="checkbox" checked={enable_new_breakpoint}
                                    onchange={change_enable} />
                            </td>
                            <td class="adddel"><button>{"add"}</button></td>
                            <td class="name">
                                <input type="text" value={self.new_breakpoint_name.clone()} oninput={change_name} />
                            </td>
                            <td class="addr">
                                <input type="text" value={self.new_breakpoint_addr.clone()} oninput={change_addr} />
                            </td>
                        </tr>
                    </tbody>
                </table>
            </form>
        </div>}
    }
}
