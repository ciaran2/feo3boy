use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{DeriveInput, Result};

use crate::device_type::BitFlags;
use crate::get_crate;

/// Build an implementation of MemDevice for a BitFlags type. Requires the bitflags type
/// to be `Copy`.
pub fn build_flags(item: &DeriveInput, flags: &BitFlags) -> Result<TokenStream> {
    let feo3boy = get_crate("feo3boy");
    let ty = &item.ident;
    let readable = match &flags.readwrite.readable {
        Some((_, readable)) => readable.value.to_token_stream(),
        None => quote! { #ty::all() },
    };
    let writable = match &flags.readwrite.writable {
        Some((_, writable)) => writable.value.to_token_stream(),
        None => quote! { #ty::all() },
    };

    let read = quote! {
        (*self & READABLE_BITS).bits() | !READABLE_BITS.bits()
    };
    let write = quote! {
        *self = (*self - WRITABLE_BITS) | (#ty::from_bits_truncate(val) & WRITABLE_BITS)
    };

    Ok(quote! {
        impl #feo3boy::memdev::MemDevice for #ty {
            const LEN: usize = 1;

            fn read_byte_relative(&self, _ctx: &#feo3boy::memdev::ReadCtx, addr: #feo3boy::memdev::RelativeAddr) -> u8 {
                const READABLE_BITS: #ty = #readable;
                #feo3boy::check_addr!(#ty, addr);
                #read
            }

            fn write_byte_relative(&mut self, _ctx: &#feo3boy::memdev::WriteCtx, addr: #feo3boy::memdev::RelativeAddr, val: u8) {
                const WRITABLE_BITS: #ty = #writable;
                #feo3boy::check_addr!(#ty, addr);
                #write;
            }

            fn read_bytes_relative(&self, _ctx: &#feo3boy::memdev::ReadCtx, addr: #feo3boy::memdev::RelativeAddr, data: &mut [u8]) {
                const READABLE_BITS: #ty = #readable;
                #feo3boy::check_addr!(#ty, addr, data.len());
                match data.first_mut() {
                    Some(data) => *data = #read,
                    None => {}
                }
            }

            fn write_bytes_relative(&mut self, _ctx: &#feo3boy::memdev::WriteCtx, addr: #feo3boy::memdev::RelativeAddr, data: &[u8]) {
                const WRITABLE_BITS: #ty = #writable;
                #feo3boy::check_addr!(#ty, addr, data.len());
                match data.first().copied() {
                    Some(val) => #write,
                    None => {}
                }
            }
        }
    })
}
