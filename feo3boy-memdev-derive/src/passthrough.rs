use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Error, Result};

use crate::get_crate;

/// Build the byte wrapper type for the derive input.
pub fn build_passthrough(item: &DeriveInput) -> Result<TokenStream> {
    let (field, field_ty) = match &item.data {
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
                (field.ident.to_token_stream(), field.ty.clone())
            }
            syn::Fields::Unnamed(fields) => {
                if fields.unnamed.len() != 1 {
                    return Err(Error::new(
                        item.ident.span(),
                        "#[memdev(passthrough)] must be used on a struct with exactly 1 \
                        field.",
                    ));
                }
                let field = &fields.unnamed[0];
                (quote! { 0 }, field.ty.clone())
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
            const LEN: usize = <#field_ty as #feo3boy::memdev::MemDevice>::LEN;

            fn read_byte_relative(&self, ctx: &#feo3boy::memdev::ReadCtx, addr: #feo3boy::memdev::RelativeAddr) -> u8 {
                #feo3boy::memdev::MemDevice::read_byte_relative(&self.#field, ctx, addr)
            }

            fn read_bytes_relative(&self, ctx: &#feo3boy::memdev::ReadCtx, addr: #feo3boy::memdev::RelativeAddr, data: &mut [u8]) {
                #feo3boy::memdev::MemDevice::read_bytes_relative(&self.#field, ctx, addr, data)
            }

            fn write_byte_relative(&mut self, ctx: &#feo3boy::memdev::WriteCtx, addr: #feo3boy::memdev::RelativeAddr, val: u8) {
                #feo3boy::memdev::MemDevice::write_byte_relative(&mut self.#field, ctx, addr, val)
            }

            fn write_bytes_relative(&mut self, ctx: &#feo3boy::memdev::WriteCtx, addr: #feo3boy::memdev::RelativeAddr, data: &[u8]) {
                #feo3boy::memdev::MemDevice::write_bytes_relative(&mut self.#field, ctx, addr, data)
            }
        }
    })
}
