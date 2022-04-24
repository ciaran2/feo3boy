use feo3boy::gb::Gb;
use feo3boy::memdev::{Cartridge, Mbc1Rom, RomOnly};
use owning_ref::RcRef;
use yew::prelude::*;

use super::{addr_range, ViewByteSlice};

type CartRef = RcRef<Gb, Cartridge>;
type Mbc1Ref = RcRef<Gb, Mbc1Rom>;

#[derive(Properties, PartialEq)]
pub struct CartridgeRomProps {
    pub cart: CartRef,
}

#[function_component(CartridgeRom)]
pub fn cartridge_rom(props: &CartridgeRomProps) -> Html {
    let body = match &*props.cart {
        Cartridge::None => html! {<div class="line">
            {addr_range(0x0000..=0x7fff)}
            <span>{"No Cartridge Inserted"}</span>
        </div>},
        Cartridge::RomOnly(_) => {
            let bank0 = props.cart.clone().map(|cart| match cart {
                Cartridge::RomOnly(rom) => rom.rom_banks[0].as_ref(),
                _ => unreachable!(),
            });
            let bank1 = props.cart.clone().map(|cart| match cart {
                Cartridge::RomOnly(rom) => rom.rom_banks[1].as_ref(),
                _ => unreachable!(),
            });
            html! {<>
                <h4 class="subsection">{"ROM Bank 0"}</h4>
                <ViewByteSlice start_addr={0x0000} slice={bank0} />
                <h4 class="subsection">{"ROM Bank 1"}</h4>
                <ViewByteSlice start_addr={0x0000} slice={bank1} />
            </>}
        }
        Cartridge::Mbc1(_) => {
            let romref = props.cart.clone().map(|cart| match cart {
                Cartridge::Mbc1(rom) => rom,
                _ => unreachable!(),
            });
            html! {}
        }
    };
    html! {<div class="mem-section cart-rom">
        <h4>{"Cartridge ROM"}</h4>
        {body}
    </div>}
}
