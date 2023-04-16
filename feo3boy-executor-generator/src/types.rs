use feo3boy_opcodes::compiler::variables::Conversion;
use feo3boy_opcodes::microcode::ValType;
use proc_macro2::TokenStream;
use quote::quote;

/// Generate all the type [`Conversions`][Conversion] in the given slice.
pub fn generate_conversions(conversions: &[Conversion]) -> TokenStream {
    conversions.iter().map(generate_conversion).collect()
}

/// Generate code to do the requested type-[`Conversion`].
pub fn generate_conversion(conversion: &Conversion) -> TokenStream {
    match conversion {
        Conversion::SameSize { from, to } => {
            let to_ty = get_type(to.val);
            let from_var = from.var;
            let to_var = to.var;
            match (from.val, to.val) {
                (ValType::Bool, ValType::U8) => quote! {
                    let #to_var: #to_ty = #from_var as #to_ty;
                },
                (ValType::Bool, ValType::Flags) => quote! {
                    let #to_var: #to_ty = #to_ty::from_bits_truncate(#from_var as u8);
                },
                (ValType::U8, ValType::Bool) => quote! {
                    let #to_var: #to_ty = #from_var != 0;
                },
                (ValType::U8, ValType::Flags) => quote! {
                    let #to_var: #to_ty = #to_ty::from_bits_truncate(#from_var);
                },
                (ValType::Flags, ValType::Bool) => quote! {
                    let #to_var: #to_ty = !#from_var.is_empty();
                },
                (ValType::Flags, ValType::U8) => quote! {
                    let #to_var: #to_ty = #from_var.bits();
                },
                (a, b) if a == b => quote! {
                    let #to_var: #to_ty = #from_var;
                },
                _ => panic!(
                    "Unsupported same-size conversion from {:?} to {:?}",
                    from.val, to.val,
                ),
            }
        }
        Conversion::Split { from, low, high } => quote! {
            let [#low, #high]: [u8; 2] = #from.to_le_bytes();
        },
        Conversion::Merge { low, high, to } => quote! {
            let #to: u16 = u16::from_le_bytes([#low, #high]);
        },
    }
}

/// Get the appropriate rust-type for a [`ValType`]
pub fn get_type(val: ValType) -> TokenStream {
    match val {
        ValType::Bool => quote! { bool },
        ValType::U16 => quote! { u16 },
        ValType::U8 => quote! { u8 },
        ValType::Flags => {
            let feo3boy_opcodes = crate::get_crate("feo3boy-opcodes");
            quote! { #feo3boy_opcodes::gbz80types::Flags }
        }
    }
}
