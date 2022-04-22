use feo3boy::gb::Gb;
use owning_ref::RcRef;
use yew::prelude::*;

use super::{hexbyte, interpret_as_char};

type MemSlice = RcRef<Gb, [u8]>;

const BYTES_PER_LINE: usize = 16;

#[derive(Properties, PartialEq)]
pub struct ViewByteSliceProps {
    pub start_addr: u16,
    pub slice: MemSlice,
}
#[function_component(ViewByteSlice)]
pub fn view_byte_slice(props: &ViewByteSliceProps) -> Html {
    let body = (0..props.slice.len())
        .step_by(BYTES_PER_LINE)
        .zip((props.start_addr..).step_by(BYTES_PER_LINE))
        .map(|(start, start_addr)| {
            let slice = props.slice.clone().map(|slice| {
                let end = (start + BYTES_PER_LINE).min(slice.len());
                &slice[start..end]
            });
            html! { <ViewByteSliceLine {start_addr} {slice} /> }
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

#[function_component(ViewByteSliceLine)]
fn view_byte_slice_line(props: &ViewByteSliceProps) -> Html {
    html! {<tr>
        <th class="addr">{format!("{:#06x}", props.start_addr)}</th>
        { for (0..BYTES_PER_LINE).map(|i| html! {
            <td class={classes!(idx_classes(i), "byte")}>
                if let Some(&byte) = props.slice.get(i) {
                    {hexbyte(byte)}
                }
            </td>
        }) }
        { for props.slice.iter().enumerate().map(|(i, &byte)| html! {
            <td class={classes!(idx_classes(i), "ascii")}>{interpret_as_char(byte)}</td>
        }) }
    </tr>}
}
