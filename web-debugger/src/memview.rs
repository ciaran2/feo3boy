use std::rc::Rc;

use feo3boy::gb::Gb;
use feo3boy::interrupts::InterruptFlags;
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    pub gb: Rc<Gb>,
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
        let enabled = ctx.props().gb.mmu.io.bios_enabled;
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
            {view_byte_slice(0x0000, &ctx.props().gb.mmu.bios.bytes())}
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
            {view_byte_slice(0x8000, &ctx.props().gb.mmu.vram)}
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
            {view_byte_slice(0xc000, &ctx.props().gb.mmu.wram)}
        </div>}
    }

    fn view_oam(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="oam">
            <h4>{"Sprite Info"}</h4>
            {view_byte_slice(0xfe00, &ctx.props().gb.mmu.oam)}
        </div>}
    }

    fn view_io(&self, ctx: &Context<Self>) -> Html {
        let io = &ctx.props().gb.mmu.io;
        html! {<div class="io">
            <h4>{"Memory-Mapped IO"}</h4>
            <div class="line">
                {addr(0xff01)}
                <table class="namedbytes">
                    <thead>
                        <tr>
                            <th>{"Serial Data"}</th>
                            <th>{"Serial Control"}</th>
                        </tr>
                    </thead>
                    <tbody>
                        <tr>
                            <td class="byte">{hexbyte(io.serial_data)}</td>
                            <td class="byte">{hexbyte(io.serial_control)}</td>
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
            {view_byte_slice(0xff80, &ctx.props().gb.mmu.zram)}
        </div>}
    }

    fn view_ie(&self, ctx: &Context<Self>) -> Html {
        html! {<div class="ie">
            <h4>{"Interrupt Enable"}</h4>
            <div class="line">
                {addr(0xffff)}
                {view_interrupt_flags(ctx.props().gb.mmu.interrupt_enable.0)}
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

fn view_byte_slice(start_addr: u16, bytes: &[u8]) -> Html {
    fn idx_classes(i: usize) -> Classes {
        let mut classes = classes!();
        if i == 0 {
            classes.push("first-addr");
        }
        if i % 4 == 0 {
            classes.push("dword-start");
        }
        if i == 0xf {
            classes.push("last-addr");
        }
        classes
    }

    const BYTES_PER_LINE: usize = 16;
    let body = bytes
        .chunks(BYTES_PER_LINE)
        .zip((start_addr..).step_by(BYTES_PER_LINE))
        .map(|(data, line_addr)| {
            html! {<tr>
                <th class="addr">{format!("{:#06x}", line_addr)}</th>
                { for (0..BYTES_PER_LINE).map(|i| html! {
                    <td class={classes!(idx_classes(i), "byte")}>
                        if let Some(&byte) = data.get(i) {
                            {hexbyte(byte)}
                        }
                    </td>
                }) }
                { for data.iter().enumerate().map(|(i, &byte)| html! {
                    <td class={classes!(idx_classes(i), "ascii")}>{
                        match byte {
                            0..=0x20 => char::from_u32(0x2400 + byte as u32).unwrap(),
                            0x21..=0x7e => byte as char,
                            0x7f => '\u{2421}',
                            _ => '\u{00a0}',
                        }
                    }</td>
                }) }
            </tr>}
        })
        .collect::<Html>();
    html! {<table class="hexeditor">
        <tr>
            <th class="addr" />
            { for (0..=0xf).map(|b| html! {
                <th class={classes!(idx_classes(b as usize), "byte")}>{hexbyte(b)}</th>
            }) }
            { for (0..=0xf).map(|b| html! {
                <th class={classes!(idx_classes(b as usize), "ascii")}>{hexbyte(b)}</th>
            }) }
        </tr>
        {body}
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
