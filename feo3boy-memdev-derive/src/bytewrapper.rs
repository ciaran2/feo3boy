use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Error, Result, Type};

use crate::device_type::ByteWrapper;
use crate::get_crate;

/// Build the byte wrapper type for the derive input.
pub fn build_byte_wrapper(item: &DeriveInput, wrapper: &ByteWrapper) -> Result<TokenStream> {
    let field = match &item.data {
        Data::Struct(s) => match &s.fields {
            syn::Fields::Named(fields) => {
                if fields.named.len() != 1 {
                    return Err(Error::new(
                        item.ident.span(),
                        "#[memdev(byte, ...)] must be used on a struct with exactly 1 \
                        field of type u8",
                    ));
                }
                let field = &fields.named[0];
                check_type_u8(&field.ty)?;
                let name = field.ident.as_ref().unwrap();
                quote! { #name }
            }
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() != 1 {
                    return Err(Error::new(
                        item.ident.span(),
                        "#[memdev(byte, ...)] must be used on a struct with exactly 1 \
                        field of type u8",
                    ));
                }
                let field = &fields.unnamed[0];
                check_type_u8(&field.ty)?;
                quote! { 0 }
            }
            syn::Fields::Unit => {
                return Err(Error::new(
                    item.ident.span(),
                    "#[memdev(byte, ...)] does not work on unit structs. Must be a struct \
                    with exactly 1 field of type u8",
                ))
            }
        },
        _ => {
            return Err(Error::new(
                item.ident.span(),
                "#[memdev(byte, ...)] only works on structs",
            ))
        }
    };

    let feo3boy = get_crate("feo3boy");
    let ty = &item.ident;
    let readable = match &wrapper.readwrite.readable {
        Some((_, readable)) => readable.value.to_token_stream(),
        None => quote! { 0xffu8 },
    };
    let writable = match &wrapper.readwrite.writable {
        Some((_, writable)) => writable.value.to_token_stream(),
        None => quote! { 0xffu8 },
    };

    Ok(quote! {
        impl #feo3boy::memdev::MemDevice for #ty {
            fn read(&self, addr: #feo3boy::memdev::Addr) -> u8 {
                const READABLE_BITS: u8 = #readable;

                assert!(
                    addr.index() == 0,
                    concat!("Address {} out of range for ", stringify!(#ty)),
                    addr
                );
                self.#field & READABLE_BITS | !READABLE_BITS
            }

            fn write(&mut self, addr: #feo3boy::memdev::Addr, val: u8) {
                const WRITABLE_BITS: u8 = #writable;

                assert!(
                    addr.index() == 0,
                    concat!("Address {} out of range for ", stringify!(#ty)),
                    addr
                );
                self.#field = (self.#field & !WRITABLE_BITS) | (val & WRITABLE_BITS)
            }
        }
    })
}

/// Verify that the given ty is a path type with ident `u8`.
fn check_type_u8(ty: &Type) -> Result<()> {
    match ty {
        Type::Path(p) if p.qself.is_none() && p.path.is_ident("u8") => Ok(()),
        _ => Err(Error::new_spanned(
            ty,
            "Type of the field of a #[memdev(byte, ...)] must be u8.",
        )),
    }
}
