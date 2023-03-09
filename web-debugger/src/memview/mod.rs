use feo3boy::gb::Gb;
use feo3boy::interrupts::InterruptFlags;
use feo3boy::memdev::GbMmu;
use owning_ref::RcRef;
use yew::prelude::*;

use byteslice::ViewByteSlice;

mod byteslice;

type GbMem = RcRef<Gb, GbMmu>;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub mem: GbMem,
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
            <div class="Memview column">
                <h3>{"Memory"}</h3>
                {self.view_bios(ctx)}
                {self.view_cart_rom(ctx)}
                {self.view_vram(ctx)}
                {self.view_cart_ram(ctx)}
                {self.view_wram(ctx)}
                {self.view_oam(ctx)}
                {self.view_io(ctx)}
                {self.view_zram(ctx)}
                {self.view_ie(ctx)}
            </div>
        }
    }
}

impl Memview {
    fn view_bios(&self, ctx: &Context<Self>) -> Html {
        let enabled = ctx.props().mem.io.bios_enabled;
        let enabled_class = match enabled {
            false => "disabled",
            true => "enabled",
        };
        html! {<div class={classes!("bios", enabled_class)} >
            <h4>
                {"BIOS"}
                if !enabled {
                    {" (disabled)"}
                }
            </h4>
            <ViewByteSlice start_addr={0x0000}
                slice={ctx.props().mem.clone().map(|mem| mem.bios.bytes())} />
        </div>}
    }

    fn view_cart_rom(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="cart-rom">
            <h4>{"Cartridge ROM"}</h4>
        </div>}
    }

    fn view_vram(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="vram">
            <h4>{"Video RAM"}</h4>
            <ViewByteSlice start_addr={0x8000}
                slice={ctx.props().mem.clone().map(|mem| mem.vram.bytes())} />
        </div>}
    }

    fn view_cart_ram(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="cart-ram">
            <h4>{"Cartridge RAM"}</h4>
        </div>}
    }

    fn view_wram(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="wram">
            <h4>{"Working RAM"}</h4>
            <ViewByteSlice start_addr={0xc000}
                slice={ctx.props().mem.clone().map(|mem| mem.wram.as_ref())} />
        </div>}
    }

    fn view_oam(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="oam">
            <h4>{"Sprite Info"}</h4>
            <ViewByteSlice start_addr={0xfe00}
                slice={ctx.props().mem.clone().map(|mem| mem.oam.bytes())} />
        </div>}
    }

    fn view_io(&self, ctx: &Context<Self>) -> Html {
        let io = &ctx.props().mem.io;
        html! {<div class="io">
            <h4>{"Memory-Mapped IO"}</h4>
            <div class="line">
                {addr(0xff01)}
                <table class="namedbytes">
                    <thead>
                        <tr>
                            <th colspan={3}>{"Serial Data"}</th>
                            <th>{"Serial Control"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="byte">{hexbyte(io.serial_data)}</td>
                            <td class="byte charinterp">{interpret_as_char(io.serial_data)}</td>
                            <td class="byte">{format!("{:08b}", io.serial_data)}</td>
                            <td class="byte">{hexbyte(io.serial_control)}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
            <div class="line">
                {addr(0xff40)}
                <table class="namedbytes">
                    <thead>
                        <tr>
                            <th>{"LCD Control"}</th>
                            <th>{"LCD Status"}</th>
                            <th>{"SCX"}</th>
                            <th>{"SCY"}</th>
                            <th>{"LY"}</th>
                            <th>{"LYC"}</th>
                            <th>{"WY"}</th>
                            <th>{"WX"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="byte">{hexbyte(io.lcd_control.bits())}</td>
                            <td class="byte">{hexbyte(io.lcd_status.bits())}</td>
                            <td class="byte">{hexbyte(io.scroll_x)}</td>
                            <td class="byte">{hexbyte(io.scroll_y)}</td>
                            <td class="byte">{hexbyte(io.lcdc_y)}</td>
                            <td class="byte">{hexbyte(io.lcdc_y_compare)}</td>
                            <td class="byte">{hexbyte(io.window_y)}</td>
                            <td class="byte">{hexbyte(io.window_x)}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
            <div class="line">
                {addr(0xff0f)}
                <div class="desc">{"Interrupt Vector"}</div>
                {view_interrupt_flags(io.interrupt_flags)}
            </div>
            <div class="line write-only">
                {addr(0xff50)}
                <table class="namedbytes">
                    <thead>
                        <tr>
                            <th>{"BIOS Enabled"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="byte">{io.bios_enabled}</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </div>}
    }

    fn view_zram(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="zram">
            <h4>{"\"Page Zero\" RAM"}</h4>
            <ViewByteSlice start_addr={0xff80}
                slice={ctx.props().mem.clone().map(|mem| mem.zram.as_ref())} />
        </div>}
    }

    fn view_ie(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="ie">
            <h4>{"Interrupt Enable"}</h4>
            <div class="line">
                {addr(0xffff)}
                {view_interrupt_flags(ctx.props().mem.interrupt_enable.0)}
            </div>
        </div>}
    }
}

fn view_interrupt_flags(flags: InterruptFlags) -> Html {
    html! {<table class="interrupt-flags">
        <thead>
            <tr>
                <th></th>
                <th></th>
                <th></th>
                <th>{"JOYPAD"}</th>
                <th>{"SERIAL"}</th>
                <th>{"TIMER"}</th>
                <th>{"STAT"}</th>
                <th>{"VBLANK"}</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>{(flags.bits() & 0b1000_0000) >> 7}</td>
                <td>{(flags.bits() & 0b0100_0000) >> 6}</td>
                <td>{(flags.bits() & 0b0010_0000) >> 5}</td>
                <td>{flags.contains(InterruptFlags::JOYPAD) as u8}</td>
                <td>{flags.contains(InterruptFlags::SERIAL) as u8}</td>
                <td>{flags.contains(InterruptFlags::TIMER) as u8}</td>
                <td>{flags.contains(InterruptFlags::STAT) as u8}</td>
                <td>{flags.contains(InterruptFlags::VBLANK) as u8}</td>
            </tr>
        </tbody>
    </table>}
}

/// Get a string representing a single byte in hex.
fn hexbyte(byte: u8) -> String {
    format!("{:02x}", byte)
}

/// Get HTML for the given address.
fn addr(addr: u16) -> Html {
    html! {<div class="addr"><h5>{format!("{:#06x}", addr)}</h5></div> }
}

fn interpret_as_char(byte: u8) -> char {
    match byte {
        0..=0x20 => char::from_u32(0x2400 + byte as u32).unwrap(),
        0x21..=0x7e => byte as char,
        0x7f => '\u{2421}',
        _ => '\u{00a0}',
    }
}
