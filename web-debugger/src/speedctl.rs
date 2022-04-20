use std::cmp::Ordering;
use std::num::NonZeroU32;

use log::warn;
use once_cell::sync::Lazy;
use yew::prelude::*;

use crate::{get_value_from_input_event, AutoTickSpeed};

#[derive(Properties, PartialEq)]
pub struct Props {
    pub speed: AutoTickSpeed,
    pub changespeed: Callback<AutoTickSpeed>,
}

pub enum Msg {
    SetSpeed { speed: String },
}

pub struct SpeedCtl {}

impl Component for SpeedCtl {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        SpeedCtl {}
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::SetSpeed { speed } => match speed.parse::<i32>() {
                Ok(speed) => ctx.props().changespeed.emit(to_speed(speed)),
                Err(_) => warn!("Failed to parse value from slider"),
            },
        }
        false
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let speed = ctx.props().speed;
        let oninput = ctx.link().callback(|e| Msg::SetSpeed {
            speed: get_value_from_input_event(e),
        });
        html! {<div class="SpeedCtl">
            <div class="display-values">
                <span>{speed.delay_ms()}{"ms"}</span>
                <span>{"×"}{speed.iterations()}</span>
            </div>
            <input type="range" min="-100" max="100" step="1"
                value={linearize(speed).to_string()}
                {oninput} />
        </div>}
    }
}

const DELAY_MAX: u32 = 5_000;
static DELAY_EXP: Lazy<f32> = Lazy::new(|| (DELAY_MAX as f32).log(100.0));
static DELAY_EXP_INV: Lazy<f32> = Lazy::new(|| 1.0 / *DELAY_EXP);

const REPEAT_MAX: u32 = 10_000;
static REPEAT_EXP: Lazy<f32> = Lazy::new(|| (REPEAT_MAX as f32).log(100.0));
static REPEAT_EXP_INV: Lazy<f32> = Lazy::new(|| 1.0 / *REPEAT_EXP);

fn linearize(speed: AutoTickSpeed) -> i32 {
    match speed {
        AutoTickSpeed::Delay { ms } => {
            let ms = ms.get().min(DELAY_MAX);
            -((ms as f32).powf(*DELAY_EXP_INV) as i32)
        }
        AutoTickSpeed::Single => 0,
        AutoTickSpeed::Repeat { iterations } => {
            let iters = iterations.get().min(REPEAT_MAX);
            (iters as f32).powf(*REPEAT_EXP_INV) as i32
        }
    }
}

fn to_speed(value: i32) -> AutoTickSpeed {
    let value = value.clamp(-100, 100);
    match value.cmp(&0) {
        Ordering::Less => {
            let absf = (value as f32).abs();
            let pow = absf.powf(*DELAY_EXP).round() as u32;
            let ms = pow.clamp(1, DELAY_MAX);
            AutoTickSpeed::Delay {
                ms: NonZeroU32::new(ms).unwrap(),
            }
        }
        Ordering::Equal => AutoTickSpeed::Single,
        Ordering::Greater => {
            let pow = (value as f32).powf(*REPEAT_EXP).round() as u32;
            let iters = pow.clamp(1, REPEAT_MAX);
            AutoTickSpeed::Repeat {
                iterations: NonZeroU32::new(iters).unwrap(),
            }
        }
    }
}
