//! Provides code generation for microcode executors.

use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::parse_macro_input;

use crate::direct_executor::DirectExecutor;
use crate::parsing::ExecutorDef;

mod direct_executor;
mod parsing;

/// Generates an implementation of the DirectExecutor.
#[proc_macro]
pub fn define_direct_executor(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Name and mapping of externs for the executor.
    let executor = parse_macro_input!(item as ExecutorDef);

    let def = match DirectExecutor::extract(executor) {
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
