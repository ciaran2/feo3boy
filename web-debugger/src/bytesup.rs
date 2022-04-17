use log::{info, warn};
use wasm_bindgen_futures::spawn_local;
use web_sys::{File, HtmlInputElement};
use yew::prelude::*;

#[derive(Properties, PartialEq)]
pub struct Props {
    /// Id of this upload.
    pub input_id: &'static str,
    /// Label to use on the upload field.
    pub label: &'static str,
    /// Callback to take the loaded data.
    pub onload: Callback<Option<(String, Vec<u8>)>>,
}

pub enum Msg {
    Changed { pending: File },
    Loaded { data: Vec<u8>, pending: File },
    Clear,
}

#[derive(Default)]
pub struct BytesUpload {
    /// Promise waiting for data to read.
    pending: Option<File>,
    input: NodeRef,
}

impl Component for BytesUpload {
    type Message = Msg;
    type Properties = Props;

    fn create(_ctx: &Context<Self>) -> Self {
        Default::default()
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::Changed { pending } => {
                self.pending = Some(pending);
                true
            }
            Msg::Loaded { data, pending } => match &self.pending {
                Some(p) if p.loose_eq(&pending) => {
                    self.pending = None;
                    ctx.props().onload.emit(Some((pending.name(), data)));
                    true
                }
                Some(_) => {
                    warn!("New change read started after this one");
                    false
                }
                None => {
                    warn!("No pending data read");
                    false
                }
            },
            Msg::Clear => {
                self.pending = None;
                ctx.props().onload.emit(None);
                match self.input.cast::<HtmlInputElement>() {
                    Some(input) => input.set_value(""),
                    None => warn!("input not bound"),
                }
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let props = ctx.props();
        let finish = ctx
            .link()
            .callback(|(data, pending)| Msg::Loaded { data, pending });
        let onchange = ctx.link().batch_callback(move |e: Event| {
            let input = e.target_dyn_into::<HtmlInputElement>()?;
            match input.files().and_then(|files| files.item(0)) {
                None => {
                    info!("No files");
                    Some(Msg::Clear)
                }
                Some(file) => {
                    {
                        let file = file.clone();
                        let blob = gloo::file::Blob::from(file.clone());
                        let finish = finish.clone();
                        spawn_local(async move {
                            match gloo::file::futures::read_as_bytes(&blob).await {
                                Ok(data) => finish.emit((data, file)),
                                Err(err) => {
                                    warn!("Failed to read file: {}", err);
                                }
                            }
                        });
                    }
                    Some(Msg::Changed { pending: file })
                }
            }
        });
        let onclick = ctx.link().callback(|_| Msg::Clear);
        html! {
            <div class="BytesUpload">
                <label for={props.input_id}>{props.label}</label>
                <input ref={self.input.clone()} type="file" id={props.input_id} {onchange} />
                <button {onclick}>{"Clear"}</button>
            </div>
        }
    }
}
