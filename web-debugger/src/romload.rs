use std::fmt;
use std::marker::PhantomData;

use serde::{Deserialize, Serialize};
use yew::prelude::*;

use crate::bytesup::BytesUpload;

#[derive(Eq, PartialEq, Debug)]
pub struct RomFile<T> {
    pub rom: T,
    pub name: String,
    pub raw: Vec<u8>,
}

impl<T> RomFile<T> {
    /// Get a copy of this rom as a SavedRom.
    pub fn clone_for_save(&self) -> SavedRom {
        SavedRom {
            name: self.name.clone(),
            raw: self.raw.clone(),
        }
    }

    pub fn info(&self) -> RomFileInfo {
        RomFileInfo {
            name: self.name.clone(),
        }
    }
}

impl<T> TryFrom<SavedRom> for RomFile<T>
where
    T: 'static + ConvertHelper,
{
    type Error = T::Error;

    fn try_from(SavedRom { name, raw }: SavedRom) -> Result<Self, Self::Error> {
        T::convert(&*raw).map(|rom| Self { rom, name, raw })
    }
}

#[derive(Eq, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct SavedRom {
    pub name: String,
    pub raw: Vec<u8>,
}

/// Just the info of the `RomFile<T>`. Improves performance by not doing eq comparisons on
/// the whole RomFile.
#[derive(Eq, PartialEq, Debug)]
pub struct RomFileInfo {
    pub name: String,
}

#[derive(Properties, PartialEq)]
pub struct Props<T: PartialEq> {
    /// Id of this upload.
    pub input_id: &'static str,
    /// Label to use on the upload field.
    pub label: &'static str,
    /// True if there is currently a rom set. Needed when loading from save-state when a
    /// file name will be lost.
    pub current: Option<RomFileInfo>,
    /// Handler to recieve the set value.
    pub onchange: Callback<Option<RomFile<T>>>,
}

pub enum Msg {
    Loaded { name: String, data: Vec<u8> },
    Cleared,
}

pub struct RomLoader<T> {
    status: String,
    _phantom: PhantomData<T>,
}

/// Helper to get around some lifetime difficulties with borrowed TryFrom.
pub trait ConvertHelper {
    type Error: 'static + fmt::Display;

    fn convert(data: &[u8]) -> Result<Self, Self::Error>
    where
        Self: Sized;
}

impl ConvertHelper for feo3boy::memdev::BiosRom {
    type Error = feo3boy::memdev::BiosSizeError;

    fn convert(data: &[u8]) -> Result<Self, Self::Error> {
        Self::try_from(data)
    }
}

impl ConvertHelper for feo3boy::memdev::Cartridge {
    type Error = feo3boy::memdev::ParseCartridgeError;

    fn convert(data: &[u8]) -> Result<Self, Self::Error> {
        Self::try_from(data)
    }
}

impl<T: 'static + ConvertHelper + PartialEq> Component for RomLoader<T> {
    type Message = Msg;
    type Properties = Props<T>;

    fn create(ctx: &Context<Self>) -> Self {
        RomLoader {
            status: match &ctx.props().current {
                Some(current) => format!(
                    "{} Loaded Successfully from {}",
                    ctx.props().label,
                    current.name
                ),
                None => format!("No {}", ctx.props().label),
            },
            _phantom: PhantomData,
        }
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Loaded { name, data } => {
                match T::convert(&*data) {
                    Ok(rom) => {
                        self.status =
                            format!("{} Loaded Successfully from {}", ctx.props().label, name);
                        ctx.props().onchange.emit(Some(RomFile {
                            rom,
                            name,
                            raw: data,
                        }));
                    }
                    Err(e) => {
                        self.status =
                            format!("Error loading {} from {}: {}", ctx.props().label, name, e);
                        ctx.props().onchange.emit(None);
                    }
                }
                true
            }
            Msg::Cleared => {
                self.status = format!("No {}", ctx.props().label);
                ctx.props().onchange.emit(None);
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let onload = ctx.link().callback(|loaded| match loaded {
            Some((name, data)) => Msg::Loaded { name, data },
            None => Msg::Cleared,
        });
        html! {
            <div class="RomLoader">
                <BytesUpload input_id={ctx.props().input_id} label={ctx.props().label} {onload} />
                <span class="error">{&self.status}</span>
            </div>
        }
    }
}
