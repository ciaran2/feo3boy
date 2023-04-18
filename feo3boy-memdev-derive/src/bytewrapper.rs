use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Result};

use crate::device_type::ByteWrapper;
use crate::get_crate;

/// Build the byte wrapper type for the derive input.
pub fn build_byte_wrapper(item: &DeriveInput, wrapper: &ByteWrapper) -> Result<TokenStream> {
    let feo3boy = get_crate("feo3boy");
    let ty = &item.ident;
    let read = &wrapper.read.value;
    let write = &wrapper.write.value;

    Ok(quote! {
        impl #feo3boy::memdev::MemDevice for #ty {
            const LEN: usize = 1;

            fn read_byte_relative(&self, addr: #feo3boy::memdev::RelativeAddr) -> u8 {
                #feo3boy::check_addr!(#ty, addr);
                #read(self)
            }

            fn write_byte_relative(&mut self, addr: #feo3boy::memdev::RelativeAddr, val: u8) {
                #feo3boy::check_addr!(#ty, addr);
                #write(self, val)
            }

            fn read_bytes_relative(&self, addr: #feo3boy::memdev::RelativeAddr, data: &mut [u8]) {
                #feo3boy::check_addr!(#ty, addr, data.len());
                match data.first_mut() {
                    Some(data) => *data = #read(self),
                    None => {}
                }
            }

            fn write_bytes_relative(&mut self, addr: #feo3boy::memdev::RelativeAddr, data: &[u8]) {
                #feo3boy::check_addr!(#ty, addr, data.len());
                match data.first().copied() {
                    Some(val) => #write(self, val),
                    None => {}
                }
            }
        }
    })
}
