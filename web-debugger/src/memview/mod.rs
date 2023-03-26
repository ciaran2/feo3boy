use std::ops::Deref;
use std::ops::RangeInclusive;

use feo3boy::gb::Gb;
use feo3boy::interrupts::InterruptFlags;
use feo3boy::memdev::{GbMmu, MemMappedIo};
use owning_ref::RcRef;
use yew::prelude::*;

use byteslice::ViewByteSlice;
use cartridge::{CartridgeRamSection, CartridgeRomSection};

mod byteslice;
mod cartridge;

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
        let mem = &ctx.props().mem;
        html! {<div class="Memview column">
            <h3>{"Memory"}</h3>
            <div class="key">
                <span class="read-write">{"Read-Write"}</span>
                <span class="read-only">{"Read Only"}</span>
                <span class="write-only">{"Write Only"}</span>
                <span class="unimplemented">{"Unimplemented"}</span>
                <span class="bad">{"Unusable"}</span>
            </div>
            {self.view_bios(ctx)}
            <CartridgeRomSection cart={mem.clone().map(|mem| &mem.cart)} />
            <div class="mem-section vram">
                <h4>{"Video RAM"}</h4>
                <ViewByteSlice start_addr={0x8000}
                    slice={mem.clone().map(|mem| mem.vram.deref().deref().as_ref())} />
            </div>
            <CartridgeRamSection cart={mem.clone().map(|mem| &mem.cart)} />
            <div class="mem-section wram">
                <h4>{"Working RAM"}</h4>
                <ViewByteSlice start_addr={0xc000}
                    slice={mem.clone().map(|mem| mem.wram.as_ref())} />
            </div>
            <div class="mem-section oam">
                <h4>{"Sprite Info"}</h4>
                <ViewByteSlice start_addr={0xfe00}
                    slice={mem.clone().map(|mem| mem.oam.deref().deref().as_ref())} />
            </div>
            <div class="mem-section unused">
                <h4>{"Unmapped Region"}</h4>
                <div class="line bad">
                    {addr_range(0xfea0..=0xffef)}
                    <span>{"Not Usable"}</span>
                </div>
            </div>
            <ViewIo io={mem.io.clone()} />
            <div class="mem-section zram">
                <h4>{"\"Page Zero\" RAM"}</h4>
                <ViewByteSlice start_addr={0xff80}
                    slice={mem.clone().map(|mem| mem.zram.as_ref())} />
            </div>
            <div class="mem-section ie">
                <h4>{"Interrupt Enable"}</h4>
                <div class="line">
                    {addr(0xffff)}
                    <ViewInterruptFlags flags={mem.interrupt_enable.0} />
                </div>
            </div>
        </div>}
    }
}

impl Memview {
    fn view_bios(&self, ctx: &Context<Self>) -> Html {
        let enabled = ctx.props().mem.io.bios_enabled;
        let enabled_class = match enabled {
            false => "disabled",
            true => "enabled",
        };
        html! {<div class={classes!("mem-section", "bios", enabled_class)} >
            <h4>
                {"BIOS"}
                if !enabled {
                    {" (disabled)"}
                }
            </h4>
            <ViewByteSlice start_addr={0x0000} class={classes!("read-only")}
                slice={ctx.props().mem.clone().map(|mem| mem.bios.as_ref())} />
        </div>}
    }
}

#[derive(Properties, PartialEq)]
pub struct ViewInterruptFlagsProps {
    pub flags: InterruptFlags,
}

#[function_component(ViewInterruptFlags)]
fn view_interrupt_flags(props: &ViewInterruptFlagsProps) -> Html {
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
                <td>{(props.flags.bits() & 0b1000_0000) >> 7}</td>
                <td>{(props.flags.bits() & 0b0100_0000) >> 6}</td>
                <td>{(props.flags.bits() & 0b0010_0000) >> 5}</td>
                <td>{props.flags.contains(InterruptFlags::JOYPAD) as u8}</td>
                <td>{props.flags.contains(InterruptFlags::SERIAL) as u8}</td>
                <td>{props.flags.contains(InterruptFlags::TIMER) as u8}</td>
                <td>{props.flags.contains(InterruptFlags::STAT) as u8}</td>
                <td>{props.flags.contains(InterruptFlags::VBLANK) as u8}</td>
            </tr>
        </tbody>
    </table>}
}

/// Get a string representing a single byte in hex.
fn hexbyte(byte: u8) -> String {
    format!("{:02x}", byte)
}

/// Get a string representing a single byte in hex.
fn hex16(byte: u16) -> String {
    format!("{:04x}", byte)
}
/// Get HTML for the given address.
fn addr(addr: u16) -> Html {
    html! {<div class="addr"><h5>{format!("{:#06x}", addr)}</h5></div> }
}

fn addr_range(range: RangeInclusive<u16>) -> Html {
    html! {<div class="addr range">
        <h5>{format!("{:#06x}", range.start())}</h5>
        if range.len() > 1 {
            if range.len() > 3 {
                <h5>{"\u{22ee}"}</h5>
            } else if range.len() == 3 {
                <h5>{format!("{:#06x}", range.start() + 1)}</h5>
            }
            <h5>{format!("{:#06x}", range.end())}</h5>
        }
    </div>}
}

fn named(name: &'static str) -> Html {
    html! {<div class="named"><h5>{name}</h5></div> }
}

fn interpret_as_char(byte: u8) -> char {
    match byte {
        0..=0x20 => char::from_u32(0x2400 + byte as u32).unwrap(),
        0x21..=0x7e => byte as char,
        0x7f => '\u{2421}',
        _ => '\u{00a0}',
    }
}

#[derive(Properties, PartialEq)]
pub struct ViewIoProps {
    pub io: MemMappedIo,
}

#[function_component(ViewIo)]
pub fn view_io(props: &ViewIoProps) -> Html {
    html! {<div class="mem-section io">
        <h4>{"Memory-Mapped IO"}</h4>
        <div class="line">
            {addr(0xff00)}
            {named("Joypad Input")}
            <span class="byte">{hexbyte(props.io.buttons.bits())}</span>
        </div>
        <div class="line">
            {addr(0xff01)}
            {named("Serial Data")}
            <span class="byte">{hexbyte(props.io.serial_data)}</span>
            <span class="byte charinterp">{interpret_as_char(props.io.serial_data)}</span>
            <span class="byte">{format!("{:08b}", props.io.serial_data)}</span>
        </div>
        <div class="line">
            {addr(0xff02)}
            {named("Serial Control")}
            <span class="byte">{hexbyte(props.io.serial_control)}</span>
        </div>
        <div class="line bad">
            {addr(0xff03)}
            {named("Unused")}
        </div>
        <div class="line">
            {addr(0xff04)}
            {named("Divider")}
            <span class="byte">{hex16(props.io.divider)}</span>
        </div>
        <div class="line">
            {addr(0xff05)}
            {named("Timer Accumulator")}
            <span class="byte">{hexbyte(props.io.timer)}</span>
        </div>
        <div class="line">
            {addr(0xff06)}
            {named("Timer Modulo")}
            <span class="byte">{hexbyte(props.io.timer_mod)}</span>
        </div>
        <div class="line">
            {addr(0xff07)}
            {named("Timer Control")}
            <span class="byte">{hexbyte(props.io.timer_control.bits())}</span>
        </div>
        <div class="line bad">
            {addr_range(0xff08..=0xff0e)}
            {named("Unused")}
        </div>
        <div class="line">
            {addr(0xff0f)}
            {named("Interrupt Vector")}
            <ViewInterruptFlags flags={props.io.interrupt_flags} />
        </div>
        <div class="line unimplemented">
            {addr_range(0xff10..=0xff26)}
            {named("Sound")}
            <span>{"(Unimplemented)"}</span>
        </div>
        <div class="line bad">
            {addr_range(0xff27..=0xff2f)}
            {named("Unused")}
        </div>
        <div class="line unimplemented">
            {addr_range(0xff30..=0xff3f)}
            {named("Wave Pattern")}
            <span>{"(Unimplemented)"}</span>
        </div>
        <div class="line">
            {addr(0xff40)}
            {named("LCD Control")}
            <span class="byte">{hexbyte(props.io.lcd_control.bits())}</span>
        </div>
        <div class="line">
            {addr(0xff41)}
            {named("LCD Status")}
            <span class="byte">{hexbyte(props.io.lcd_status.bits())}</span>
        </div>
        <div class="line">
            {addr(0xff42)}
            {named("SCX")}
            <span class="byte">{hexbyte(props.io.scroll_x)}</span>
        </div>
        <div class="line">
            {addr(0xff43)}
            {named("SCY")}
            <span class="byte">{hexbyte(props.io.scroll_y)}</span>
        </div>
        <div class="line">
            {addr(0xff44)}
            {named("LY")}
            <span class="byte">{hexbyte(props.io.lcdc_y)}</span>
        </div>
        <div class="line">
            {addr(0xff45)}
            {named("LYC")}
            <span class="byte">{hexbyte(props.io.lcdc_y_compare)}</span>
        </div>
        <div class="line unimplemented">
            {addr(0xff46)}
            {named("DMA")}
            <span class="byte">{hexbyte(props.io.dma_addr)}</span>
        </div>
        <div class="line">
            {addr(0xff47)}
            {named("BGP")}
            <span class="byte">{hexbyte(props.io.bg_palette)}</span>
        </div>
        <div class="line">
            {addr(0xff48)}
            {named("OBP0")}
            <span class="byte">{hexbyte(props.io.obj_palette[0])}</span>
        </div>
        <div class="line">
            {addr(0xff49)}
            {named("OBP1")}
            <span class="byte">{hexbyte(props.io.obj_palette[1])}</span>
        </div>
        <div class="line">
            {addr(0xff4a)}
            {named("WY")}
            <span class="byte">{hexbyte(props.io.window_y)}</span>
        </div>
        <div class="line">
            {addr(0xff4b)}
            {named("WX")}
            <span class="byte">{hexbyte(props.io.window_x)}</span>
        </div>
        <div class="line bad">
            {addr_range(0xff4c..=0xff4f)}
            {named("Unused")}
        </div>
        <div class="line write-only">
            {addr(0xff50)}
            {named("BIOS Enabled")}
            <span>{props.io.bios_enabled}</span>
        </div>
        <div class="line bad">
            {addr_range(0xff51..=0xff7f)}
            {named("Unused")}
        </div>
    </div>}
}
