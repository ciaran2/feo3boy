use feo3boy::gb::Gb;
use feo3boy::memdev::{Cartridge, Mbc1Rom, RomOnly};
use owning_ref::RcRef;
use yew::prelude::*;

use super::{addr_range, hexbyte, named, ViewByteSlice};

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

enum Mbc1RomRomSectionMsg {}

struct Mbc1RomRomSection {}

impl Component for Mbc1RomRomSection {
    type Properties = Mbc1RomProps;
    type Message = Mbc1RomRomSectionMsg;

    fn create(_ctx: &Context<Self>) -> Self {
        Mbc1RomRomSection {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let rom = &ctx.props().rom;
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

        </>}
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
            <span>{"No cartridge ram"}</span>
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

enum Mbc1RomRamSectionMsg {}

struct Mbc1RomRamSection {}

impl Component for Mbc1RomRamSection {
    type Properties = Mbc1RomProps;
    type Message = Mbc1RomRamSectionMsg;

    fn create(_ctx: &Context<Self>) -> Self {
        Mbc1RomRamSection {}
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {}
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {}
    }
}
