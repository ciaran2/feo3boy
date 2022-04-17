use std::collections::BTreeMap;
use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::memdev::{BiosRom, Cartridge};
use gloo::timers::callback::Timeout;
use log::info;
use yew::prelude::*;

use breakpoints::{Breakpoint, Breakpoints};
use derefs::Derefs;
use memview::Memview;
use regs::Regs;
use romload::RomLoader;

mod breakpoints;
mod bytesup;
mod derefs;
mod instrs;
mod memview;
mod regs;
mod romload;

enum Msg {
    /// Set the bios rom to use. Resets the GB.
    SetBios {
        bios: Option<BiosRom>,
    },
    /// Set the cartridge rom to use. Resets the GB.
    SetCart {
        cart: Option<Cartridge>,
    },
    /// Start or stop automatic ticking.
    SetRunning {
        should_run: bool,
    },
    /// Run one tick. If singlestep is true, the `running` property will be ignored (and
    /// cleared if set).
    Tick {
        singlestep: bool,
    },
    Reset,
    /// Add a breakpoint at the specified address. If one is set, replace it.
    AddBreakpoint {
        addr: u16,
        name: String,
        enabled: bool,
    },
    /// Delete a breakpoint at the specified address.
    DeleteBreakpoint {
        addr: u16,
    },
    /// Set the enabled state of a breakpoint at the specified address.
    ToggleBreakpoint {
        addr: u16,
        new_enabled: bool,
    },
}

struct App {
    /// Bios to load into the GB.
    bios: BiosRom,
    /// Cartridge to load into the GB.
    cart: Cartridge,
    /// GB instance being run.
    gb: Rc<Gb>,
    /// Next pending tick.
    pending_tick: Option<Timeout>,
    /// Breakpoints for the program.
    breakpoints: Rc<BTreeMap<u16, Breakpoint>>,
}

impl App {
    /// Rebuild the GB for new roms.
    fn reset_roms(&mut self) {
        self.gb = Rc::new(Gb::new(self.bios.clone(), self.cart.clone()));
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        let bios = BiosRom::default();
        let cart = Cartridge::None;
        let gb = Rc::new(Gb::new(bios.clone(), cart.clone()));
        Self {
            bios,
            cart,
            gb,
            pending_tick: None,
            breakpoints: Default::default(),
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::SetBios { bios } => {
                self.bios = bios.unwrap_or_default();
                self.reset_roms();
                true
            }
            Msg::SetCart { cart } => {
                self.cart = cart.unwrap_or(Cartridge::None);
                self.reset_roms();
                true
            }
            Msg::SetRunning { should_run } => {
                if should_run && self.pending_tick.is_none() {
                    let step = ctx.link().callback(|_| Msg::Tick { singlestep: false });
                    self.pending_tick = Some(Timeout::new(0, move || step.emit(())));
                    true
                } else if !should_run && self.pending_tick.is_some() {
                    self.pending_tick.take().unwrap().cancel();
                    true
                } else {
                    false
                }
            }
            Msg::Tick { singlestep } => {
                if singlestep {
                    self.pending_tick.take().map(|tick| tick.cancel());
                }
                Rc::make_mut(&mut self.gb).tick();
                if !singlestep {
                    match self.breakpoints.get(&self.gb.cpustate.regs.pc) {
                        Some(breakpoint) if breakpoint.enabled => self.pending_tick = None,
                        _ => {
                            let step = ctx.link().callback(|_| Msg::Tick { singlestep: false });
                            self.pending_tick = Some(Timeout::new(0, move || step.emit(())));
                        }
                    }
                }
                true
            }
            Msg::Reset => {
                self.pending_tick.take().map(|tick| tick.cancel());
                self.reset_roms();
                true
            }
            Msg::AddBreakpoint {
                addr,
                name,
                enabled,
            } => {
                let breakpoints = Rc::make_mut(&mut self.breakpoints);
                breakpoints.insert(addr, Breakpoint { name, enabled });
                true
            }
            Msg::DeleteBreakpoint { addr } => {
                Rc::make_mut(&mut self.breakpoints).remove(&addr).is_some()
            }
            Msg::ToggleBreakpoint { addr, new_enabled } => {
                if let Some(bp) = Rc::make_mut(&mut self.breakpoints).get_mut(&addr) {
                    let old = bp.enabled;
                    bp.enabled = new_enabled;
                    old != new_enabled
                } else {
                    false
                }
            }
        }
    }

    fn view(&self, ctx: &yew::Context<Self>) -> yew::Html {
        let link = ctx.link();
        let setbios = link.callback(|bios| Msg::SetBios { bios });
        let setcart = link.callback(|cart| Msg::SetCart { cart });
        let run = link.callback(|_| Msg::SetRunning { should_run: true });
        let pause = link.callback(|_| Msg::SetRunning { should_run: false });
        let step = link.callback(|_| Msg::Tick { singlestep: true });
        let reset = link.callback(|_| Msg::Reset);

        let add_breakpoint = link.callback(|(addr, name, enabled)| Msg::AddBreakpoint {
            addr,
            name,
            enabled,
        });
        let delete_breakpoint = link.callback(|addr| Msg::DeleteBreakpoint { addr });
        let toggle_breakpoint =
            link.callback(|(addr, new_enabled)| Msg::ToggleBreakpoint { addr, new_enabled });
        html! {
            <div class="App">
                <div class="header">
                    <h1>{"feo3boy debugger"}</h1>
                </div>
                <div class="body row">
                    <div class="column">
                        <div class="romselect">
                            <RomLoader<BiosRom> onchange={setbios} input_id="bios-load" label="BIOS" />
                            <RomLoader<Cartridge> onchange={setcart} input_id="cart-load" label="Cartridge" />
                        </div>
                        <div class="row">
                            <div class="column">
                                <div class="runcontrols">
                                    <button onclick={reset}>{"Reset"}</button>
                                    if self.pending_tick.is_none() {
                                        <button onclick={run}>{"Run"}</button>
                                        <button onclick={step}>{"Step"}</button>
                                    } else {
                                        <button onclick={pause}>{"Pause"}</button>
                                        <button disabled=true>{"Step"}</button>
                                    }
                                </div>
                                <Regs gb={self.gb.clone()} />
                            </div>
                            <div class="column">
                                <Derefs gb={self.gb.clone()} />
                            </div>
                            <div class="column">
                                <Breakpoints gb={self.gb.clone()} breakpoints={self.breakpoints.clone()}
                                    {add_breakpoint} {delete_breakpoint} {toggle_breakpoint} />
                            </div>
                        </div>
                    </div>
                    <div class="column">
                        <Memview gb={self.gb.clone()} />
                    </div>
                </div>
            </div>
        }
    }
}

fn main() {
    console_log::init_with_level(log::Level::Debug).expect("Unable to init logger");
    yew::start_app::<App>();
}
