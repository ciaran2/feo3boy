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
    pub class: Option<Classes>,
}

#[function_component(ViewByteSlice)]
pub fn view_byte_slice(props: &ViewByteSliceProps) -> Html {
    let mut lines = vec![];
    let mut zero_lines = vec![];

    fn collect_zero_lines(lines: &mut Vec<Html>, zero_lines: &mut Vec<(usize, u16, MemSlice)>) {
        if !zero_lines.is_empty() {
            if zero_lines.len() <= 3 {
                for (idx, start_addr, slice) in zero_lines.drain(..) {
                    lines.push(html! {
                        <ViewByteSliceLine key={start_addr as u128}
                            {start_addr} {slice} />
                    });
                }
            } else {
                let (sidx, start_addr, _) = zero_lines.first().unwrap();
                let (last_idx, last_start_addr, lastslice) = zero_lines.last().unwrap();
                // Len is always at least 1 because of how we iterate.
                let end_addr = last_start_addr + (lastslice.len() as u16 - 1);
                let size = (last_idx + lastslice.len()) - sidx;
                lines.push(html! {<tr>
                    <th class="addr"><div class="range">
                        <h5>{format!("{:#06x}", start_addr)}</h5>
                        <h5>{"\u{22ee}"}</h5>
                        <h5>{format!("{:#06x}", end_addr)}</h5>
                    </div></th>
                    <td class="group" colspan={32}>{size}{" zero bytes (0x00)"}</td>
                </tr>});
                zero_lines.clear();
            }
        }
    }

    for (idx, start_addr) in (0..props.slice.len())
        .step_by(BYTES_PER_LINE)
        .zip((props.start_addr..).step_by(BYTES_PER_LINE))
    {
        let slice = props.slice.clone().map(|slice| {
            let end = (idx + BYTES_PER_LINE).min(slice.len());
            &slice[idx..end]
        });
        if slice.iter().all(|&b| b == 0) {
            zero_lines.push((idx, start_addr, slice));
        } else {
            collect_zero_lines(&mut lines, &mut zero_lines);
            lines.push(html! {
                <ViewByteSliceLine key={start_addr as u128}
                    {start_addr} {slice} />
            });
        }
    }
    collect_zero_lines(&mut lines, &mut zero_lines);

    html! {<table class={classes!("hexeditor", &props.class)}>
        <tr>
            <th class="addr" />
            { for (0..=0xf).map(|b| html! {
                <th class={classes!(idx_classes(b as usize), "byte")}>{hexbyte(b)}</th>
            }) }
            { for (0..=0xf).map(|b| html! {
                <th class={classes!(idx_classes(b as usize), "ascii")}>{hexbyte(b)}</th>
            }) }
        </tr>
        { for lines.into_iter() }
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
