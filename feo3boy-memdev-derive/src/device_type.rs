//! Provides types (with parsing support) for representing the attributes used in the
//! #[memdev(...)] arguments.

use derive_syn_parse::Parse;
use syn::parse::{Parse, ParseStream};
use syn::{DeriveInput, Error, Expr, Meta, Result, Token};

/// Which type of device are we deriving a value for?
#[derive(Parse)]
pub enum DeviceType {
    /// The type is a wrapper around a single byte.
    #[peek(kw::bits, name = "BitsWrapper")]
    BitsWrapper(BitsWrapper),
    /// The type is a wrapper around bitflags.
    #[peek(kw::bitflags, name = "BitFlags")]
    BitFlags(BitFlags),
    /// The type is a wrapper around bitflags.
    #[peek(kw::byte, name = "ByteWrapper")]
    ByteWrapper(ByteWrapper),
    /// Passthrough mem device.
    #[peek(kw::passthrough, name = "PassThrough")]
    Passthrough(kw::passthrough),
}

impl DeviceType {
    /// Finds an attribute indicating which DeviceType this is.
    pub fn extract_from(input: &DeriveInput) -> Result<DeviceType> {
        let matches = input
            .attrs
            .iter()
            .filter_map(|attr| match &attr.meta {
                Meta::Path(path) => {
                    if path.is_ident("memdev") {
                        Some(Err(Error::new_spanned(
                            path,
                            "#[memdev] on the type definition requires an argument",
                        )))
                    } else {
                        None
                    }
                }
                Meta::List(list) => {
                    if list.path.is_ident("memdev") {
                        Some(syn::parse2::<DeviceType>(list.tokens.clone()))
                    } else {
                        None
                    }
                }
                Meta::NameValue(nv) => {
                    if nv.path.is_ident("memdev") {
                        Some(Err(Error::new_spanned(
                            &nv.path,
                            "`#[memdev]` attribute on a type uses list syntax",
                        )))
                    } else {
                        None
                    }
                }
            })
            .collect::<Result<Vec<_>>>()?;
        if matches.len() == 1 {
            Ok(matches.into_iter().next().unwrap())
        } else {
            Err(Error::new(
                input.ident.span(),
                format!(
                    "Expected exactly 1 #[memdev] attribute, got {}",
                    matches.len()
                ),
            ))
        }
    }
}

/// Contents of the `#[memdev(bits, readable = ..., writable = ...)]` expression.
#[derive(Parse)]
pub struct BitsWrapper {
    /// The bits token that starts this expression.
    pub byte: kw::bits,
    /// The values for the `readable = ...`  and `writable = ...` inputs.
    pub readwrite: ReadableWritable,
}

/// Contents of the `#[memdev(bitflags, readable = ..., writable = ...)]` expression.
#[derive(Parse)]
pub struct BitFlags {
    /// The bitflags token that starts this expression.
    pub byte: kw::bitflags,
    /// The values for the `readable = ...`  and `writable = ...` inputs.
    pub readwrite: ReadableWritable,
}

pub struct ReadableWritable {
    /// The value set for "readable" if any.
    pub readable: Option<(Token![,], Readable)>,
    /// The value set for "writable" if any.
    pub writable: Option<(Token![,], Writable)>,
}

impl Parse for ReadableWritable {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut readable = None;
        let mut writable = None;
        if input.peek(Token![,]) && input.peek2(kw::readable) {
            readable = Some((input.parse()?, input.parse()?));
        }
        if input.peek(Token![,]) && input.peek2(kw::writable) {
            writable = Some((input.parse()?, input.parse()?));
        }
        Ok(Self { readable, writable })
    }
}

/// The sequence `readable = <expr>`.
#[derive(Parse)]
pub struct Readable {
    pub readable: kw::readable,
    pub equals: Token![=],
    pub value: Expr,
}

/// The sequence `readable = <expr>`.
#[derive(Parse)]
pub struct Writable {
    pub writable: kw::writable,
    pub equals: Token![=],
    pub value: Expr,
}

/// Contests of the `#[memdev(byte, read = ..., write = ...)]` expression.
#[derive(Parse)]
pub struct ByteWrapper {
    /// The byte token that starts this expression.
    pub byte: kw::byte,
    pub comma1: Token![,],
    /// The value set for "read".
    pub read: Read,
    pub comma2: Token![,],
    /// The value set for "write".
    pub write: Write,
}

/// The sequence `read = <expr>`.
#[derive(Parse)]
pub struct Read {
    pub read: kw::read,
    pub equals: Token![=],
    pub value: Expr,
}

/// The sequence `readable = <expr>`.
#[derive(Parse)]
pub struct Write {
    pub write: kw::write,
    pub equals: Token![=],
    pub value: Expr,
}

mod kw {
    syn::custom_keyword!(bits);
    syn::custom_keyword!(bitflags);
    syn::custom_keyword!(byte);
    syn::custom_keyword!(passthrough);

    syn::custom_keyword!(readable);
    syn::custom_keyword!(read);
    syn::custom_keyword!(writable);
    syn::custom_keyword!(write);
}
