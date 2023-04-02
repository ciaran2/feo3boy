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

    Ok(quote! {
        impl #feo3boy::memdev::MemDevice for #ty {
            fn read(&self, addr: #feo3boy::memdev::Addr) -> u8 {
                const READABLE_BITS: #ty = #readable;

                assert!(
                    addr.index() == 0,
                    concat!("Address {} out of range for ", stringify!(#ty)),
                    addr
                );
                (*self & READABLE_BITS).bits() | !READABLE_BITS.bits()
            }

            fn write(&mut self, addr: #feo3boy::memdev::Addr, val: u8) {
                const WRITABLE_BITS: #ty = #writable;

                assert!(
                    addr.index() == 0,
                    concat!("Address {} out of range for ", stringify!(#ty)),
                    addr
                );
                *self = (*self - WRITABLE_BITS) | (#ty::from_bits_truncate(val) & WRITABLE_BITS)
            }
        }
    })
}
