use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::{DeriveInput, Result};

use crate::device_type::DeviceType;

mod bitflags;
mod bytewrapper;
mod device_type;
mod passthrough;

/// Generates an implementation of `MemDevice` for a type.
#[proc_macro_derive(MemDevice, attributes(memdev))]
pub fn derive_mem_device(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(item as DeriveInput);

    match build_memdev_derives(&input) {
        Ok(ts) => ts.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

/// Get a representation of either `crate` or `::<name>` to use for the specified crate.
fn get_crate(name: &str) -> TokenStream {
    match proc_macro_crate::crate_name(name) {
        Ok(FoundCrate::Itself) => quote! { crate },
        Ok(FoundCrate::Name(name)) => {
            let name = Ident::new(&name, Span::call_site());
            quote! { ::#name }
        }
        Err(e) => panic!(
            "Unable to find the {} crate: {}, required by this macro.",
            name, e
        ),
    }
}

/// Generate memdev derives for this type.
fn build_memdev_derives(item: &DeriveInput) -> Result<TokenStream> {
    let device_type = DeviceType::extract_from(item)?;

    match device_type {
        DeviceType::ByteWrapper(ref wrapper) => bytewrapper::build_byte_wrapper(item, wrapper),
        DeviceType::BitFlags(ref flags) => bitflags::build_flags(item, flags),
        DeviceType::Passthrough(_) => passthrough::build_passthrough(item),
    }
}
