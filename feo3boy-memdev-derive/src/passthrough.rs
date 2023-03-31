use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Error, Result};

use crate::get_crate;

/// Build the byte wrapper type for the derive input.
pub fn build_passthrough(item: &DeriveInput) -> Result<TokenStream> {
    let field = match &item.data {
        Data::Struct(s) => match &s.fields {
            syn::Fields::Named(fields) => {
                if fields.named.len() != 1 {
                    return Err(Error::new(
                        item.ident.span(),
                        "#[memdev(passthrough)] must be used on a struct with exactly 1 \
                        field.",
                    ));
                }
                let field = &fields.named[0];
                field.ident.to_token_stream()
            }
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() != 1 {
                    return Err(Error::new(
                        item.ident.span(),
                        "#[memdev(passthrough)] must be used on a struct with exactly 1 \
                        field.",
                    ));
                }
                quote! { 0 }
            }
            syn::Fields::Unit => {
                return Err(Error::new(
                    item.ident.span(),
                    "#[memdev(passthrough)] does not work on unit structs. Must be a struct \
                    with exactly 1 field",
                ))
            }
        },
        _ => {
            return Err(Error::new(
                item.ident.span(),
                "#[memdev(passthrough)] only works on structs",
            ))
        }
    };

    let feo3boy = get_crate("feo3boy");
    let ty = &item.ident;

    Ok(quote! {
        impl #feo3boy::memdev::MemDevice for #ty {
            fn read(&self, addr: #feo3boy::memdev::Addr) -> u8 {
                #feo3boy::memdev::MemDevice::read(&self.#field, addr)
            }

            fn write(&mut self, addr: #feo3boy::memdev::Addr, val: u8) {
                #feo3boy::memdev::MemDevice::write(&mut self.#field, addr, val)
            }
        }
    })
}
