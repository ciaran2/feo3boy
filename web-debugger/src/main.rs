use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::memdev::{BiosRom, Cartridge};
use gloo::timers::callback::Timeout;
use yew::prelude::*;

use regs::Regs;
use romload::RomLoader;

mod bytesup;
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
                    let step = ctx.link().callback(|_| Msg::Tick { singlestep: false });
                    self.pending_tick = Some(Timeout::new(0, move || step.emit(())));
                }
                true
            }
            Msg::Reset => {
                self.pending_tick.take().map(|tick| tick.cancel());
                self.reset_roms();
                true
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
        html! {
            <div class="App">
                <div>
                    <h1>{"feo3boy debugger"}</h1>
                </div>
                <div>
                    <RomLoader<BiosRom> onchange={setbios} input_id="bios-load" label="BIOS" />
                    <RomLoader<Cartridge> onchange={setcart} input_id="cart-load" label="Cartridge" />
                </div>
                <div>
                    <button onclick={reset}>{"Reset"}</button>
                    if self.pending_tick.is_none() {
                        <button onclick={run}>{"Run"}</button>
                        <button onclick={step}>{"Step"}</button>
                    } else {
                        <button onclick={pause}>{"Pause"}</button>
                    }
                </div>
                <Regs gb={self.gb.clone()} />
            </div>
        }
    }
}

fn main() {
    console_log::init_with_level(log::Level::Debug).expect("Unable to init logger");
    yew::start_app::<App>();
}
