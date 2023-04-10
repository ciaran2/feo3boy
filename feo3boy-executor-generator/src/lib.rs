//! Provides code generation for microcode executors.

use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::{parse_macro_input, ItemMod};

use crate::direct_executor::DirectExecutor;

mod direct_executor;

/// Generates an implementation of the DirectExecutor.
#[proc_macro_attribute]
pub fn define_direct_executor(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Name to use for the executor.
    let name = parse_macro_input!(attr as Ident);

    // Contents of the module which defines externs.
    let module = parse_macro_input!(item as ItemMod);

    let def = match DirectExecutor::extract(name, module) {
        Ok(def) => def,
        Err(e) => return e.into_compile_error().into(),
    };

    def.generate().into()
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
