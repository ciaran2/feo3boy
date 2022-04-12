use std::fmt;
use std::marker::PhantomData;

use yew::prelude::*;

use crate::bytesup::BytesUpload;

#[derive(Properties)]
pub struct Props<T> {
    /// Id of this upload.
    pub input_id: &'static str,
    /// Label to use on the upload field.
    pub label: &'static str,
    /// Handler to recieve the set value.
    pub onchange: Callback<Option<T>>,
}

impl<T> PartialEq for Props<T> {
    fn eq(&self, other: &Props<T>) -> bool {
        self.onchange == other.onchange
    }
}

pub enum Msg {
    Loaded { data: Vec<u8> },
}

pub struct RomLoader<T> {
    error: String,
    _phantom: PhantomData<T>,
}

impl<T> Component for RomLoader<T>
where
    T: 'static + TryFrom<Vec<u8>>,
    T::Error: fmt::Display,
{
    type Message = Msg;
    type Properties = Props<T>;

    fn create(_ctx: &Context<Self>) -> Self {
        RomLoader {
            error: String::new(),
            _phantom: PhantomData,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Loaded { data } => {
                if data.is_empty() {
                    self.error.clear();
                    ctx.props().onchange.emit(None);
                    true
                } else {
                    match T::try_from(data) {
                        Ok(rom) => {
                            self.error.clear();
                            ctx.props().onchange.emit(Some(rom));
                        }
                        Err(e) => {
                            self.error = format!("{}", e);
                        }
                    }
                    true
                }
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let onload = ctx.link().callback(|data| Msg::Loaded { data });
        html! {
            <div class="BiosLoader">
                <BytesUpload input_id={ctx.props().input_id} label={ctx.props().label} {onload} />
                if !self.error.is_empty() {
                    <span class="error">{&self.error}</span>
                }
            </div>
        }
    }
}
