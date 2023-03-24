use proc_macro2::TokenStream;
use quote::quote;

use crate::defs::{AllowedType, AllowedTypes, Defs, MicrocodeOp, MicrocodeTypeDef};

/// Generates the `Microcode` type from the [`MicrocodeTypeDef`].
pub fn generate_microcode_type(def: &Defs) -> TokenStream {
    let enum_def = generate_enum_def(&def.microcode_type);
    let allowed_types_def = generate_allowed_types_def(&def.allowed_types);
    let module_def = &def.module;

    quote! {
        #enum_def
        #allowed_types_def
        #module_def
    }
}

/// Generate the enum definition for the `Microcode` type.
fn generate_enum_def(
    MicrocodeTypeDef {
        name,
        vis,
        docs,
        ops,
    }: &MicrocodeTypeDef,
) -> TokenStream {
    let member_defs = ops.iter().map(|op| generate_member_def(op));

    quote! {
        #(#docs)*
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #vis enum #name {
            #(#member_defs)*
        }
    }
}

/// Generate a single member of the microcode enum.
fn generate_member_def(
    MicrocodeOp {
        name, docs, args, ..
    }: &MicrocodeOp,
) -> TokenStream {
    let fields: Vec<_> = args
        .iter()
        .filter_map(|arg| {
            arg.field_info
                .as_ref()
                .map(|field_info| (&arg.ty, field_info))
        })
        .collect();

    let content = if fields.is_empty() {
        None
    } else {
        let field_defs = fields.iter().map(|(ty, field_info)| {
            let field_name = &field_info.name;
            let docs = &field_info.docs;
            quote! {
                #(#docs)*
                #field_name: #ty,
            }
        });
        Some(quote!({ #(#field_defs)* }))
    };

    quote! {
        #(#docs)*
        #name #content,
    }
}

/// Create the enum that defines the "allowed types".
fn generate_allowed_types_def(
    AllowedTypes {
        name,
        docs,
        vis,
        types,
    }: &AllowedTypes,
) -> TokenStream {
    let members = types.iter().map(|AllowedType { name, docs, .. }| {
        quote! {
            #(#docs)*
            #name
        }
    });

    quote! {
        #(#docs)*
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #vis enum #name {
            #(#members,)*
        }
    }
}
