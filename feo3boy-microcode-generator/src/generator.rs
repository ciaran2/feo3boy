use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::{ItemMod, Type};

use crate::defs::{
    AllowedType, AllowedTypes, ArgSource, Defs, MicrocodeOp, MicrocodeSubtype, MicrocodeTypeDef,
    StackInfo,
};

/// Generates the `Microcode` type from the [`MicrocodeTypeDef`].
pub fn generate_microcode_type(def: &Defs) -> TokenStream {
    let enum_def = generate_enum_def(&def.microcode_type);
    let allowed_types_def = generate_allowed_types_def(&def.allowed_types);
    let extern_names = generate_extern_names_def(&def.microcode_type);
    let descriptor_def = generate_descriptor_def(&def.microcode_type, &def.allowed_types);
    let descriptors_impl =
        generate_descriptors_impl(&def.microcode_type, &def.allowed_types, &def.module);
    let module_def = &def.module;

    quote! {
        #enum_def
        #allowed_types_def
        #extern_names
        #descriptor_def
        #descriptors_impl
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
        ..
    }: &MicrocodeTypeDef,
) -> TokenStream {
    let member_defs = ops.iter().map(|op| generate_member_def(op));

    quote! {
        #(#docs)*
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        #vis enum #name {
            #(#member_defs)*
        }
    }
}

/// Generate a single member of the microcode enum.
fn generate_member_def(
    MicrocodeOp {
        name,
        docs,
        args,
        returns,
        sig,
        ..
    }: &MicrocodeOp,
) -> TokenStream {
    let fields: Vec<_> = args
        .iter()
        .filter_map(|arg| match arg.source {
            ArgSource::Field(ref field_info) => Some((&arg.ty, field_info)),
            ArgSource::Stack(_) => None,
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

    let sig_doc = format!("**Signature:** `{}`", sig.to_token_stream().to_string());

    let pops_doc: Vec<_> = args
        .iter()
        .filter_map(|arg| match &arg.source {
            ArgSource::Field(_) => None,
            ArgSource::Stack(StackInfo { name }) => {
                let ty = &arg.ty;
                Some(match name {
                    None => ty.to_token_stream().to_string(),
                    Some(name) => quote! { #name: #ty }.to_string(),
                })
            }
        })
        .collect();

    let pops_doc = if pops_doc.is_empty() {
        quote! {
            #[doc = ""]
            #[doc = "**Does not pop any values off of the stack.**"]
        }
    } else {
        quote! {
            #[doc = ""]
            #[doc = "**Pops these values off of the stack:**"]
            #[doc = "(in order from top of the stack to the bottom)"]
            #[doc = "```no_run"]
            #(#[doc = #pops_doc])*
            #[doc = "```"]
        }
    };

    let pushes_doc: Vec<_> = returns
        .iter()
        .rev()
        .map(|ret| ret.ty.to_token_stream().to_string())
        .collect();

    let pushes_doc = if pushes_doc.is_empty() {
        quote! {
            #[doc = ""]
            #[doc = "**Does not push any results onto the stack.**"]
        }
    } else {
        quote! {
            #[doc = ""]
            #[doc = "**Pushes these values onto the stack:**"]
            #[doc = "(in order from top of the stack to the bottom)"]
            #[doc = "```no_run"]
            #(#[doc = #pushes_doc])*
            #[doc = "```"]
        }
    };

    quote! {
        #(#docs)*
        #[doc = ""]
        #[doc = #sig_doc]
        #pops_doc
        #pushes_doc
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
        #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
        #vis enum #name {
            #(#members,)*
        }
    }
}

/// Generates an enum to represent the extern names for the microcode.
fn generate_extern_names_def(
    MicrocodeTypeDef {
        vis,
        name,
        externs_name,
        ops,
        ..
    }: &MicrocodeTypeDef,
) -> TokenStream {
    let doc = format!("Names all the possible externs defined by the [`{name}`] type.");

    let names = ops.iter().filter_map(|op| match op.subtype {
        MicrocodeSubtype::Extern => {
            let name = &op.name;
            let doc = &op.docs;
            Some(quote! {
                #(#doc)*
                #name,
            })
        }
        MicrocodeSubtype::Function => None,
    });

    quote! {
        #[doc = #doc]
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #vis enum #externs_name {
            #(#names)*
        }
    }
}

/// Generate the descriptor type for the microcode.
fn generate_descriptor_def(
    MicrocodeTypeDef {
        vis,
        descriptor_name,
        externs_name,
        ..
    }: &MicrocodeTypeDef,
    AllowedTypes {
        name: allowed_types_name,
        ..
    }: &AllowedTypes,
) -> TokenStream {
    let doc = format!("Provides descriptive informaton about an instruction from the microcode.");

    quote! {
        #[doc = #doc]
        #[derive(Debug, Clone)]
        #vis struct #descriptor_name {
            /// Name of the opcode.
            pub name: String,
            /// Arguments to this operator. Stack arguments are in the order they should
            /// be popped off the stack. Field arguments are provided as literals.
            pub args: Vec<Arg<#allowed_types_name>>,
            /// Values returned from this operator. All of them go on the stack. Provided
            /// in the order they should be pushed onto the stack.
            pub returns: Vec<#allowed_types_name>,
            /// Which type of operation this is (an extern or a pure function).
            pub optype: OperationType<#externs_name>,
        }
    }
}

/// Generate the function that retrieves the descriptor of a microcode operation.
fn generate_descriptors_impl(
    def @ MicrocodeTypeDef {
        vis,
        name,
        descriptor_name,
        ops,
        ..
    }: &MicrocodeTypeDef,
    allowed_types: &AllowedTypes,
    module: &ItemMod,
) -> TokenStream {
    let patterns = ops.iter().map(|op| {
        let pat = generate_op_pattern(op);
        let val = generate_descriptor_value(op, def, allowed_types, module);
        quote! {
            Self::#pat => #val
        }
    });

    let doc = format!("Get the [`{}`] for this operation.", descriptor_name);

    quote! {
        impl #name {
            #[doc = #doc]
            #vis fn descriptor(self) -> #descriptor_name {
                match self {
                    #(#patterns,)*
                }
            }
        }
    }
}

/// Generate a pattern which matches this microcode op and extracts all fields.
fn generate_op_pattern(op: &MicrocodeOp) -> TokenStream {
    let fields: Vec<_> = op
        .args
        .iter()
        .filter_map(|arg| match &arg.source {
            ArgSource::Field(field) => Some(&field.name),
            ArgSource::Stack(_) => None,
        })
        .collect();

    let name = &op.name;
    if fields.is_empty() {
        quote! { #name }
    } else {
        quote! {
            #name {
                #(#fields,)*
            }
        }
    }
}

/// Generate code that produces the value of a microcode descriptor for a particular
/// operation.
fn generate_descriptor_value(
    op: &MicrocodeOp,
    def: &MicrocodeTypeDef,
    allowed_types: &AllowedTypes,
    module: &ItemMod,
) -> TokenStream {
    let descriptor_name = &def.descriptor_name;
    let externs_name = &def.externs_name;
    let allowed_types_name = &allowed_types.name;
    let name = op.name.to_string();

    let args = op.args.iter().map(|arg| match &arg.source {
        ArgSource::Field(field) => {
            let field_name = &field.name;
            quote! {
                Arg::Literal(AsLiteral::as_literal(&#field_name))
            }
        }
        ArgSource::Stack(_) => {
            let typename = rust_type_to_allowed_type(&arg.ty, allowed_types);
            quote! {
                Arg::StackValue(#allowed_types_name::#typename)
            }
        }
    });

    let returns = op.returns.iter().map(|ret| {
        let typename = rust_type_to_allowed_type(&ret.ty, allowed_types);
        quote! {
            #allowed_types_name::#typename
        }
    });

    let optype = match op.subtype {
        MicrocodeSubtype::Extern => {
            let name = &op.name;
            quote! {
                OperationType::Extern {
                    name: #externs_name::#name,
                }
            }
        }
        MicrocodeSubtype::Function => {
            let modname = &module.ident;
            let funcname = &op.func_name;
            quote! {
                OperationType::Function {
                    path: quote! { #modname::#funcname }
                }
            }
        }
    };

    quote! {
        #descriptor_name {
            name: #name.into(),
            args: vec![
                #(#args,)*
            ],
            returns: vec![
                #(#returns,)*
            ],
            optype: #optype,
        }
    }
}

/// Find the name of the AllowedTypes enum value for a particular rust type.
fn rust_type_to_allowed_type<'a, 'b>(
    rust_type: &'a Type,
    allowed_types: &'b AllowedTypes,
) -> &'b Ident {
    match rust_type {
        Type::Path(path) => {
            if path.qself.is_some() {
                panic!("Expected an unqualified path");
            }
            match path.path.get_ident() {
                Some(ident) => allowed_types
                    .types
                    .iter()
                    .find_map(|allowed| {
                        if ident == allowed.ty.to_string().as_str() {
                            Some(&allowed.name)
                        } else {
                            None
                        }
                    })
                    .expect("Matching allowed type not found"),
                None => panic!("Expected a single-identifier path"),
            }
        }
        _ => panic!("Expected a path type"),
    }
}
