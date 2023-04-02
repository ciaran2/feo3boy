//! This crate provides the `#[define_microcode]` attribute macro which automatically
//! generates the `Microcode` type from a module containing the microcode's operations as
//! pure functions.
use proc_macro2::{Ident, Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::{parse_macro_input, ItemMod};

use crate::extractor::extract_defs;
use crate::generator::generate_microcode_type;
use crate::validation::validate_defs;

/// Provides structures which represent the various parts of the Microcode definition that
/// are needed for code generation.
mod defs;

/// Extracts various parts of the definition of the Microcode type from the microcode
/// definition module.
mod extractor;

/// Generates the resulting Microcode type from the definition data.
mod generator;

/// Utilities for specialty parsing.
mod parsing;

/// Provides validation for the defs before generating output.
mod validation;

/// Generates the `Microcode` type from a Rust module. The module that this attribute
/// macro is placed on must be defined in-line not in a separate file. The attribute macro
/// takes a single argument, which is the name to use for the `Microcode` type.
///
/// Doc comments applies to the module will be copied to the `Microcode` type. The
/// visibility modifier of the `Microcode` type is taken from the module this attribute is
/// applied to.
#[proc_macro_attribute]
pub fn define_microcode(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    // Name to use for the `Microcode` type.
    let name = parse_macro_input!(attr as Ident);

    // Contents of the type-definition module.
    let module = parse_macro_input!(item as ItemMod);

    let defs = match extract_defs(name, module) {
        Ok(defs) => defs,
        Err(e) => return e.into_compile_error().into(),
    };
    if let Err(e) = validate_defs(&defs) {
        return e.into_compile_error().into();
    }
    generate_microcode_type(&defs).into()
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
