use feo3boy::gb::Gb;
use feo3boy::memdev::{Cartridge, Mbc1Rom, RomOnly};
use owning_ref::RcRef;
use yew::prelude::*;

use super::{addr_range, hexbyte, named, ViewByteSlice};
use crate::CompareAssign;

type CartRef = RcRef<Gb, Cartridge>;
type RomOnlyRef = RcRef<Gb, RomOnly>;
type Mbc1Ref = RcRef<Gb, Mbc1Rom>;

#[derive(Properties, PartialEq)]
pub struct CartridgeProps {
    pub cart: CartRef,
}

#[function_component(CartridgeRomSection)]
pub fn cartridge_rom(props: &CartridgeProps) -> Html {
    let body = match &*props.cart {
        Cartridge::None => html! { <NoCartRomSection /> },
        Cartridge::RomOnly(_) => {
            let rom = props.cart.clone().map(|cart| match cart {
                Cartridge::RomOnly(ref rom) => rom,
                _ => unreachable!(),
            });
            html! { <RomOnlyRomSection {rom} /> }
        }
        Cartridge::Mbc1(_) => {
            let rom = props.cart.clone().map(|cart| match cart {
                Cartridge::Mbc1(rom) => rom,
                _ => unreachable!(),
            });
            html! { <Mbc1RomRomSection {rom} /> }
        }
    };
    html! {<div class="mem-section cart-rom">
        <h4>{"Cartridge ROM"}</h4>
        {body}
    </div>}
}

#[function_component(NoCartRomSection)]
fn no_cart_rom_section() -> Html {
    html! {<div class="line bad">
        {addr_range(0x0000..=0x7fff)}
        <span>{"No Cartridge Inserted"}</span>
    </div>}
}

#[derive(Properties, PartialEq)]
struct RomOnlyProps {
    pub rom: RomOnlyRef,
}

#[function_component(RomOnlyRomSection)]
fn rom_only_rom_section(props: &RomOnlyProps) -> Html {
    let bank0 = props.rom.clone().map(|rom| rom.rom_banks[0].as_ref());
    let bank1 = props.rom.clone().map(|rom| rom.rom_banks[1].as_ref());
    html! {<>
        <h4 class="subsection">{"ROM Bank 0"}</h4>
        <ViewByteSlice start_addr={0x0000} slice={bank0} class={classes!("read-only")} />
        <h4 class="subsection">{"ROM Bank 1"}</h4>
        <ViewByteSlice start_addr={0x0000} slice={bank1} class={classes!("read-only")} />
    </>}
}

#[derive(Properties, PartialEq)]
struct Mbc1RomProps {
    pub rom: Mbc1Ref,
}

enum Mbc1RomRomSectionMsg {
    SetLowerBank { viewed_lower: Option<usize> },
    SetUpperBank { viewed_upper: Option<usize> },
}

struct Mbc1RomRomSection {
    viewed_lower: Option<usize>,
    viewed_upper: Option<usize>,
}

impl Component for Mbc1RomRomSection {
    type Properties = Mbc1RomProps;
    type Message = Mbc1RomRomSectionMsg;

    fn create(_ctx: &Context<Self>) -> Self {
        Mbc1RomRomSection {
            viewed_lower: None,
            viewed_upper: None,
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Mbc1RomRomSectionMsg::SetLowerBank { viewed_lower } => {
                self.viewed_lower.ne_assign(viewed_lower)
            }
            Mbc1RomRomSectionMsg::SetUpperBank { viewed_upper } => {
                self.viewed_upper.ne_assign(viewed_upper)
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let rom = &ctx.props().rom;
        let link = ctx.link();
        let lower_bank = match self.viewed_lower {
            Some(viewed_lower) => rom.clone().map(|rom| &rom.rom_banks()[viewed_lower][..]),
            None => rom.clone().map(|rom| &rom.lower_bank()[..]),
        };
        let clearlower =
            link.callback(|_| Mbc1RomRomSectionMsg::SetLowerBank { viewed_lower: None });
        let upper_bank = match self.viewed_upper {
            Some(viewed_upper) => rom.clone().map(|rom| &rom.rom_banks()[viewed_upper][..]),
            None => rom.clone().map(|rom| &rom.upper_bank()[..]),
        };
        let clearupper =
            link.callback(|_| Mbc1RomRomSectionMsg::SetUpperBank { viewed_upper: None });
        html! {<>
            <h4 class="subsection">{"Internal Registers"}</h4>
            <div class="line write-only">
                {addr_range(0x0000..=0x1fff)}
                {named("RAM Enable")}
                <span>{rom.ram_enable()}</span>
            </div>
            <div class="line write-only">
                {addr_range(0x2000..=0x3fff)}
                {named("ROM Bank")}
                <span class="byte">{hexbyte(rom.rom_bank().get())}</span>
                <span class="byte dec">{rom.rom_bank()}</span>
            </div>
            <div class="line write-only">
                {addr_range(0x4000..=0x5fff)}
                <div class="named">
                    <h5>{"RAM Bank"}</h5>
                    <h5>{"Bank Set"}</h5>
                </div>
                <span class="byte">{hexbyte(rom.bank_set())}</span>
                <span class="byte dec">{rom.bank_set()}</span>
            </div>
            <div class="line write-only">
                {addr_range(0x6000..=0x7fff)}
                <div class="named">
                    <h5>{"Banking Mode"}</h5>
                </div>
                <span class="byte">
                    if rom.advanced_banking_mode() {
                        {"RAM Banking Mode / Advanced ROM Banking Mode"}
                    } else {
                        {"Simple ROM Banking Mode"}
                    }
                </span>
            </div>
            <h4 class="subsection">{"Lower ROM Bank"}</h4>
            <div class="bankselect">
                <button class={classes!("choosebank", "auto", self.viewed_lower.is_none().then(|| "viewed"))}
                    onclick={clearlower}>
                    <span class="banknum">{rom.selected_lower_bank()}</span>
                    <span class="material-icons">{"hdr_auto"}</span>
                </button>
                { for (0..rom.rom_banks().len()).step_by(32).map(|i| {
                    let is_viewed = self.viewed_lower == Some(i);
                    let is_selected = rom.selected_lower_bank() == i;
                    let class = classes!("choosebank", is_viewed.then(|| "viewed"));
                    let onclick = link.callback(move |_| Mbc1RomRomSectionMsg::SetLowerBank { viewed_lower: Some(i) });
                    html! {<button {class}  {onclick}>
                        <span class="banknum">{i}</span>
                        if is_selected {
                            <span class="material-icons">{"radio_button_checked"}</span>
                        } else {
                            <span class="material-icons">{"radio_button_unchecked"}</span>
                        }
                    </button>}
                }) }
            </div>
            <ViewByteSlice start_addr={0x0000} slice={lower_bank} class={classes!("read-only", match self.viewed_lower {
                None => None,
                Some(viewed_lower) if viewed_lower == rom.selected_lower_bank() => None,
                _ => Some("disabled"),
            })} />
            <h4 class="subsection">{"Upper ROM Bank"}</h4>
            <div class="bankselect">
                <button class={classes!("choosebank", "auto", self.viewed_upper.is_none().then(|| "viewed"))}
                    onclick={clearupper}>
                    <span class="banknum">{rom.selected_upper_bank()}</span>
                    <span class="material-icons">{"hdr_auto"}</span>
                </button>
                { for (1..rom.rom_banks().len()).filter(|&i| i % 32 != 0).map(|i| {
                    let is_viewed = self.viewed_upper == Some(i);
                    let is_selected = rom.selected_upper_bank() == i;
                    let class = classes!("choosebank", is_viewed.then(|| "viewed"));
                    let onclick = link.callback(move |_| Mbc1RomRomSectionMsg::SetUpperBank { viewed_upper: Some(i) });
                    html! {<button {class} {onclick}>
                        <span class="banknum">{i}</span>
                        if is_selected {
                            <span class="material-icons">{"radio_button_checked"}</span>
                        } else {
                            <span class="material-icons">{"radio_button_unchecked"}</span>
                        }
                    </button>}
                }) }
            </div>
            <ViewByteSlice start_addr={0x4000} slice={upper_bank} class={classes!("read-only", match self.viewed_upper {
                None => None,
                Some(viewed_upper) if viewed_upper == rom.selected_upper_bank() => None,
                _ => Some("disabled"),
            })} />
        </>}
    }

    fn changed(&mut self, ctx: &Context<Self>) -> bool {
        let rom = &ctx.props().rom;
        if let Some(view) = self.viewed_lower {
            if view >= rom.rom_banks().len() {
                self.viewed_lower = None;
            }
        }
        if let Some(view) = self.viewed_upper {
            if view >= rom.rom_banks().len() {
                self.viewed_upper = None;
            }
        }
        true
    }
}

#[function_component(CartridgeRamSection)]
pub fn cartridge_ram(props: &CartridgeProps) -> Html {
    let body = match &*props.cart {
        Cartridge::None => html! { <NoCartRamSection /> },
        Cartridge::RomOnly(_) => {
            let rom = props.cart.clone().map(|cart| match cart {
                Cartridge::RomOnly(ref rom) => rom,
                _ => unreachable!(),
            });
            html! { <RomOnlyRamSection {rom} /> }
        }
        Cartridge::Mbc1(_) => {
            let rom = props.cart.clone().map(|cart| match cart {
                Cartridge::Mbc1(rom) => rom,
                _ => unreachable!(),
            });
            html! { <Mbc1RomRamSection {rom} /> }
        }
    };
    html! {<div class="mem-section cart-ram">
        <h4>{"Cartridge RAM"}</h4>
        {body}
    </div>}
}

#[function_component(NoCartRamSection)]
fn no_cart_ram_section() -> Html {
    html! {<div class="line bad">
        {addr_range(0xa000..=0xbfff)}
        <span>{"No Cartridge Inserted"}</span>
    </div>}
}

#[function_component(RomOnlyRamSection)]
fn rom_only_ram_section(props: &RomOnlyProps) -> Html {
    match props.rom.ram_bank {
        None => html! {<div class="line bad">
            {addr_range(0xa000..=0xbfff)}
            <span>{"No cartridge RAM"}</span>
        </div>},
        Some(_) => {
            let ram = props
                .rom
                .clone()
                .map(|rom| &rom.ram_bank.as_ref().unwrap()[..]);
            html! { <ViewByteSlice start_addr={0xa000} slice={ram} /> }
        }
    }
}

enum Mbc1RomRamSectionMsg {
    SetBank { viewed: Option<usize> },
}

struct Mbc1RomRamSection {
    viewed: Option<usize>,
}

impl Component for Mbc1RomRamSection {
    type Properties = Mbc1RomProps;
    type Message = Mbc1RomRamSectionMsg;

    fn create(_ctx: &Context<Self>) -> Self {
        Mbc1RomRamSection { viewed: None }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Mbc1RomRamSectionMsg::SetBank { viewed } => self.viewed.ne_assign(viewed),
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let rom = &ctx.props().rom;
        if rom.ram_banks().is_empty() {
            html! {<div class="line bad">
                {addr_range(0xa000..=0xbfff)}
                <span>{"No cartridge RAM"}</span>
            </div>}
        } else {
            let link = ctx.link();
            let clearviewed = link.callback(|_| Mbc1RomRamSectionMsg::SetBank { viewed: None });
            let bank = rom
                .clone()
                .map(|rom| &rom.ram_banks()[self.viewed.unwrap_or_default()][..]);
            html! {<>
                <div class="bankselect">
                    <button class={classes!("choosebank", "auto", self.viewed.is_none().then(|| "viewed"))}
                        onclick={clearviewed}>
                        <span class="banknum">{rom.selected_ram_bank()}</span>
                        <span class="material-icons">{"hdr_auto"}</span>
                    </button>
                    { for (0..rom.ram_banks().len()).map(|i| {
                        let is_viewed = self.viewed == Some(i);
                        let is_selected = rom.selected_ram_bank() == i;
                        let class = classes!("choosebank", is_viewed.then(|| "viewed"));
                        let onclick = link.callback(move |_| Mbc1RomRamSectionMsg::SetBank { viewed: Some(i) });
                        html! {<button {class} {onclick}>
                            <span class="banknum">{i}</span>
                            if is_selected {
                                <span class="material-icons">{"radio_button_checked"}</span>
                            } else {
                                <span class="material-icons">{"radio_button_unchecked"}</span>
                            }
                        </button>}
                    }) }
                </div>
                <ViewByteSlice start_addr={0xa000} slice={bank} class={classes!(match self.viewed {
                    None if rom.ram_enable() => None,
                    Some(lower) if rom.ram_enable() && lower == rom.selected_ram_bank() => None,
                    _ => Some("disabled"),
                })} />
            </>}
        }
    }
}
