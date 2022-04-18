use std::collections::BTreeMap;
use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::memdev::{BiosRom, Cartridge};
use gloo::storage::errors::StorageError;
use gloo::storage::{LocalStorage, Storage};
use gloo::timers::callback::Timeout;
use log::warn;
use serde::{Deserialize, Serialize};
use yew::prelude::*;

use breakpoints::{Breakpoint, Breakpoints};
use derefs::Derefs;
use memview::Memview;
use regs::Regs;
use romload::{RomFile, RomLoader, SavedRom};
use serial::Serial;

mod breakpoints;
mod bytesup;
mod derefs;
mod instrs;
mod memview;
mod regs;
mod romload;
mod serial;

const SAVED_STATE_KEY: &str = "feo3boy.webdebugger.savestate-v1";

/// Saved debugger state.
#[derive(Default, Serialize, Deserialize)]
struct SaveState {
    bios: Option<SavedRom>,
    cart: Option<SavedRom>,
    breakpoints: BTreeMap<u16, Breakpoint>,
}

enum Msg {
    /// Set the bios rom to use. Resets the GB.
    SetBios {
        bios: Option<RomFile<BiosRom>>,
    },
    /// Set the cartridge rom to use. Resets the GB.
    SetCart {
        cart: Option<RomFile<Cartridge>>,
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
    bios: Option<Rc<RomFile<BiosRom>>>,
    /// Cartridge to load into the GB.
    cart: Option<Rc<RomFile<Cartridge>>>,
    /// GB instance being run.
    gb: Rc<Gb>,
    /// Next pending tick.
    pending_tick: Option<Timeout>,
    /// Breakpoints for the program.
    breakpoints: Rc<BTreeMap<u16, Breakpoint>>,
    /// Serial data output by the program.
    serial: Rc<String>,
}

impl App {
    /// Rebuild the GB for new roms.
    fn reset_roms(&mut self) {
        self.gb = Rc::new(Gb::new(self.bios(), self.cart()));
        self.serial = Rc::new(String::new());
    }

    /// Clone the selected bios rom or get the default bios.
    fn bios(&self) -> BiosRom {
        self.bios
            .as_ref()
            .map(|bios| bios.rom.clone())
            .unwrap_or_default()
    }

    /// Clone the selected cartridge rom or get the default cartridge.
    fn cart(&self) -> Cartridge {
        self.cart
            .as_ref()
            .map_or_else(|| Cartridge::None, |cart| cart.rom.clone())
    }

    fn save_state(&self) {
        let saved_state = SaveState {
            bios: self.bios.as_ref().map(|bios| bios.clone_for_save()),
            cart: self.cart.as_ref().map(|cart| cart.clone_for_save()),
            breakpoints: self.breakpoints.as_ref().clone(),
        };
        if let Err(e) = LocalStorage::set(SAVED_STATE_KEY, &saved_state) {
            warn!("Unable to save database: {}", e);
        }
    }
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        let saved_state: SaveState = LocalStorage::get(SAVED_STATE_KEY).unwrap_or_else(|e| {
            if !matches!(e, StorageError::KeyNotFound(_)) {
                warn!("Failed to load database: {}", e);
            }
            Default::default()
        });

        let mut app = Self {
            bios: saved_state
                .bios
                .and_then(|bios| match RomFile::<BiosRom>::try_from(bios) {
                    Ok(rom) => Some(Rc::new(rom)),
                    Err(e) => {
                        warn!("Failed to parse saved rom: {}", e);
                        None
                    }
                }),
            cart: saved_state
                .cart
                .and_then(|bios| match RomFile::<Cartridge>::try_from(bios) {
                    Ok(rom) => Some(Rc::new(rom)),
                    Err(e) => {
                        warn!("Failed to parse saved rom: {}", e);
                        None
                    }
                }),
            gb: Rc::new(Gb::new(BiosRom::default(), Cartridge::None)),
            pending_tick: None,
            breakpoints: Rc::new(saved_state.breakpoints),
            serial: Rc::new(String::new()),
        };
        app.reset_roms();
        app
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::SetBios { bios } => {
                self.bios = bios.map(Rc::new);
                self.save_state();
                self.reset_roms();
                self.pending_tick.take().map(|tick| tick.cancel());
                true
            }
            Msg::SetCart { cart } => {
                self.cart = cart.map(Rc::new);
                self.save_state();
                self.reset_roms();
                self.pending_tick.take().map(|tick| tick.cancel());
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
            Msg::Tick { singlestep: true } => {
                self.pending_tick.take().map(|tick| tick.cancel());
                let gb = Rc::make_mut(&mut self.gb);
                gb.tick();
                let serial_out = gb.serial.stream.receive_bytes();
                if serial_out.len() != 0 {
                    let serial = Rc::make_mut(&mut self.serial);
                    serial.extend(serial_out.map(|b| b as char));
                }
                true
            }
            Msg::Tick { singlestep: false } => {
                // For performance (to avoid the overhead of continuously cloning the gb
                // state and rerendering the whole page), run multiple ticks per web timer
                // step.
                const NON_SINGLESTEP_TICKS: usize = 1024;
                let gb = Rc::make_mut(&mut self.gb);
                for _ in 0..NON_SINGLESTEP_TICKS {
                    gb.tick();
                    match self.breakpoints.get(&gb.cpustate.regs.pc) {
                        Some(breakpoint) if breakpoint.enabled => break,
                        _ => {}
                    }
                }
                let serial_out = gb.serial.stream.receive_bytes();
                if serial_out.len() != 0 {
                    let serial = Rc::make_mut(&mut self.serial);
                    serial.extend(serial_out.map(|b| b as char));
                }
                match self.breakpoints.get(&gb.cpustate.regs.pc) {
                    Some(breakpoint) if breakpoint.enabled => self.pending_tick = None,
                    _ => {
                        let step = ctx.link().callback(|_| Msg::Tick { singlestep: false });
                        self.pending_tick = Some(Timeout::new(0, move || step.emit(())));
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
                self.save_state();
                true
            }
            Msg::DeleteBreakpoint { addr } => {
                if Rc::make_mut(&mut self.breakpoints).remove(&addr).is_some() {
                    self.save_state();
                    true
                } else {
                    false
                }
            }
            Msg::ToggleBreakpoint { addr, new_enabled } => {
                if let Some(bp) = Rc::make_mut(&mut self.breakpoints).get_mut(&addr) {
                    if bp.enabled != new_enabled {
                        bp.enabled = new_enabled;
                        self.save_state();
                        true
                    } else {
                        false
                    }
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
                        <h3>{"ROM Selection"}</h3>
                        <div class="romselect">
                            <RomLoader<BiosRom> onchange={setbios} input_id="bios-load" label="BIOS"
                                current={self.bios.clone()} />
                            <RomLoader<Cartridge> onchange={setcart} input_id="cart-load" label="Cartridge"
                                current={self.cart.clone()} />
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
                        <Serial serial_output={self.serial.clone()} />
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
    console_log::init_with_level(log::Level::Info).expect("Unable to init logger");
    yew::start_app::<App>();
}
