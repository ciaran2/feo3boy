use proc_macro2::Ident;
use syn::{
    Attribute, Error, FnArg, Item, ItemFn, ItemMacro, ItemMod, Meta, Result, ReturnType, Type,
    Visibility,
};

use crate::defs::{
    AllowedType, AllowedTypes, Defs, FieldInfo, MicrocodeArg, MicrocodeOp, MicrocodeRes,
    MicrocodeSubtype, MicrocodeTypeDef,
};
use crate::parsing::AllowedTypesMappings;

/// Extracts everything we need for the generator to work.
pub fn extract_defs(name: Ident, mut module: ItemMod) -> Result<Defs> {
    let vis = module.vis.clone();
    let docs = extract_docs(&module.attrs);

    let (allowed_types, ops) = match module.content {
        Some(ref mut content) => {
            extract_and_filter_items(&module.ident, &module.vis, &mut content.1)?
        }
        None => {
            return Err(Error::new(
                module.ident.span(),
                "define_microcode must be used on an inline module, not one defined \
                externally",
            ));
        }
    };

    Ok(Defs {
        module,
        microcode_type: MicrocodeTypeDef {
            name,
            vis,
            docs,
            ops,
        },
        allowed_types,
    })
}

/// Extract the docs from a list of attributes.
fn extract_docs(attrs: &[Attribute]) -> Vec<Attribute> {
    attrs
        .iter()
        .filter(|attr| match &attr.meta {
            Meta::NameValue(nv) => nv.path.is_ident("doc"),
            _ => false,
        })
        .cloned()
        .collect()
}

/// Processes the list of items in the module.
///
/// Finds ones tagged `#[microcode(Name)]` or `#[microcode_extern(Name)]` and converts
/// them into [`MicrocodeOp`s][MicrocodeOp]. For externs, discards the function defintion
/// from the module. For pure-function ops, removes any microcode related tags.
fn extract_and_filter_items(
    mod_name: &Ident,
    vis: &Visibility,
    items: &mut Vec<Item>,
) -> Result<(AllowedTypes, Vec<MicrocodeOp>)> {
    let mut ops = Vec::with_capacity(items.len());
    let mut allowed_types = None;

    let mut i = items.len();
    while i > 0 {
        i -= 1;
        match &mut items[i] {
            Item::Fn(func) => {
                if let Some(op) = extract_op_from_item(func)? {
                    let subtype = op.subtype;
                    ops.push(op);
                    // Discard externs.
                    if subtype == MicrocodeSubtype::Extern {
                        items.remove(i);
                    }
                }
            }
            Item::Macro(mac) => {
                if mac.mac.path.is_ident("allowed_types") {
                    if allowed_types.is_some() {
                        return Err(Error::new_spanned(
                            &mac.mac.path,
                            "allowed_types defined more than once.",
                        ));
                    }
                    allowed_types = Some(extract_allowed_types(mac, vis)?);
                    // Discard the allowed_types macro.
                    items.remove(i);
                }
            }
            _ => {}
        }
    }

    let allowed_types = match allowed_types {
        Some(allowed_types) => allowed_types,
        None => {
            return Err(Error::new(
                mod_name.span(),
                "define_microcode module must include an allowed_types macro call",
            ))
        }
    };

    Ok((allowed_types, ops))
}

/// Checks if the given item has either `#[microcode(Name)]` or
/// `#[microcode_extern(Name)]` and if so returns the `MicrocodeOp` that it defines,
/// otherwise returns `None`. If the item defines a microcode op, the attribute marking it
/// as such will be removed, as will the #[field] attribute on any of its parameters.
fn extract_op_from_item(item: &mut ItemFn) -> Result<Option<MicrocodeOp>> {
    Ok(
        if let Some((name, subtype)) = extract_microcode_marker(&mut item.attrs)? {
            let docs = extract_docs(&item.attrs);
            Some(MicrocodeOp {
                name,
                func_name: item.sig.ident.clone(),
                docs,
                args: extract_args(item.sig.inputs.iter_mut())?,
                returns: extract_returns(&item.sig.output)?,
                subtype,
            })
        } else {
            None
        },
    )
}

/// Reads through the microcode attributes and finds one labeled either `microcode` or
/// `microcode_extern`, and extracts the `Ident` from it.
fn extract_microcode_marker(
    attrs: &mut Vec<Attribute>,
) -> Result<Option<(Ident, MicrocodeSubtype)>> {
    let mut res = None;

    let mut i = attrs.len();
    while i > 0 {
        i -= 1;
        match &attrs[i].meta {
            Meta::Path(path) => {
                if path.is_ident("microcode") {
                    return Err(Error::new_spanned(
                        path,
                        "#[microcode] marker on requires an argument to specify the name \
                        of the enum value",
                    ));
                }
                if path.is_ident("microcode_extern") {
                    return Err(Error::new_spanned(
                        path,
                        "#[microcode_extern] marker on requires an argument to specify \
                        the name of the enum value",
                    ));
                }
                // Retain all other path attributes.
                continue;
            }
            Meta::List(list) => {
                let subtype = if list.path.is_ident("microcode") {
                    MicrocodeSubtype::Function
                } else if list.path.is_ident("microcode_extern") {
                    MicrocodeSubtype::Extern
                } else {
                    // Not a microcode attribute, don't delete.
                    continue;
                };
                if !res.is_none() {
                    return Err(Error::new_spanned(
                        &list.path,
                        "may only specify one #[microcode(...)] or \
                        #[microcode_extern(...)] attribute.",
                    ));
                }
                res = Some((syn::parse2::<Ident>(list.tokens.clone())?, subtype));
            }
            Meta::NameValue(nv) => {
                if nv.path.is_ident("microcode") {
                    return Err(Error::new_spanned(
                        &nv.path,
                        "`#[microcode(...)]` uses a parenthsized argument not `microcode \
                        = ...` syntax",
                    ));
                } else if nv.path.is_ident("microcode_extern") {
                    return Err(Error::new_spanned(
                        &nv.path,
                        "`#[microcode_extern(...)]` uses a parenthsized argument not \
                        `microcode_extern = ... syntax`",
                    ));
                }
                // Not a microcode attribute, retain it.
                continue;
            }
        }
        attrs.remove(i);
    }
    Ok(res)
}

/// Iterates over the arguments to a function and extracts them as [`MicrocodeArg`]. If
/// any are field arguments, the `#[field]` attribute is removed.
fn extract_args<'a>(inputs: impl Iterator<Item = &'a mut FnArg> + 'a) -> Result<Vec<MicrocodeArg>> {
    inputs
        .map(|arg| match arg {
            FnArg::Receiver(recv) => {
                return Err(Error::new(
                    recv.self_token.span,
                    "microcode funcs must be free functions, not associated functions",
                ));
            }
            FnArg::Typed(arg) => {
                let mut is_field = false;
                let mut docs = vec![];

                let mut i = arg.attrs.len();
                while i > 0 {
                    i -= 1;
                    match &arg.attrs[i].meta {
                        Meta::Path(path) => {
                            if path.is_ident("field") {
                                is_field = true;
                                arg.attrs.remove(i);
                            }
                        }
                        Meta::List(list) => {
                            if list.path.is_ident("field") {
                                return Err(Error::new_spanned(
                                    &list.path,
                                    "`#[field]` attribute does not take any args",
                                ));
                            }
                        }
                        Meta::NameValue(nv) => {
                            if nv.path.is_ident("field") {
                                return Err(Error::new_spanned(
                                    &nv.path,
                                    "`#[field]` attribute does not take any args",
                                ));
                            }
                            if nv.path.is_ident("doc") {
                                docs.push(arg.attrs.remove(i));
                            }
                        }
                    }
                }
                let field_info = if is_field {
                    match &*arg.pat {
                        syn::Pat::Ident(ident) => Some(FieldInfo {
                            name: ident.ident.clone(),
                            docs,
                        }),
                        _ => {
                            return Err(Error::new_spanned(
                                &arg.pat,
                                "argument with `#[field]` must use a single ident pattern.",
                            ));
                        }
                    }
                } else {
                    None
                };
                Ok(MicrocodeArg {
                    ty: arg.ty.as_ref().clone(),
                    field_info,
                })
            }
        })
        .collect()
}

/// Extract the return types for a microcode operator. For most types, the type is
/// produced directly, but for tuples, the tuple is unpacked.
fn extract_returns(returns: &ReturnType) -> Result<Vec<MicrocodeRes>> {
    Ok(match returns {
        ReturnType::Default => vec![],
        ReturnType::Type(_, ty) => match &**ty {
            Type::Tuple(tuple) => {
                if tuple.elems.len() == 1 {
                    // This check doesn't work if the tuple is behind a type alias.
                    return Err(Error::new_spanned(
                        tuple,
                        "microcode functions are not allowd to return single-element \
                        tuples. If you only have one value to return, return it directly.",
                    ));
                }
                tuple
                    .elems
                    .iter()
                    .map(|ty| MicrocodeRes { ty: ty.clone() })
                    .collect()
            }
            // non-tuple types are used directly.
            _ => vec![MicrocodeRes {
                ty: ty.as_ref().clone(),
            }],
        },
    })
}

/// Extract the allowed types from the allowed_types macro.
fn extract_allowed_types(mac: &ItemMacro, vis: &Visibility) -> Result<AllowedTypes> {
    let mapping: AllowedTypesMappings = syn::parse2(mac.mac.tokens.clone())?;

    Ok(AllowedTypes {
        docs: extract_docs(&mac.attrs),
        name: mapping.name.ident,
        vis: vis.clone(),
        types: mapping
            .types
            .content
            .into_iter()
            .map(|item| AllowedType {
                docs: extract_docs(&item.attributes),
                ty: item.ty,
                name: item.ident,
            })
            .collect(),
    })
}
