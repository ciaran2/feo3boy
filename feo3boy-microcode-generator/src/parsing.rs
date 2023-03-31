use derive_syn_parse::Parse;
use proc_macro2::Ident;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Bracket;
use syn::{bracketed, Attribute, Token};

/// The contents of the `allowed_types!` macro invocation.
#[derive(Parse)]
pub struct AllowedTypesMappings {
    pub name: AllowedTypesName,
    /// The comma token between the name and the types list.
    pub comma: Token![,],
    /// The listing of allowed types.
    pub types: AllowedTypesList,
    /// Optional trailing comma after the brackets.
    pub trailing_comma: Option<Token![,]>,
}

/// Defines the mapping of the allowed-types name.
#[derive(Parse)]
pub struct AllowedTypesName {
    /// The literal token `name`.
    pub name: kw::name,
    /// The assignment on the first line of the macro invocation.
    pub equals: Token![=],
    /// The identifier for the AllowedTypes generated type.
    pub ident: Ident,
}

/// List of allowed types. Of the form:
///
/// ```text
/// types = [
///     rust_type => EnumName,
///     rust_type2 => EnumName2,
/// ]
/// ```
pub struct AllowedTypesList {
    /// The "types" token.
    pub types: kw::types,
    /// The assignment between `types` and `[`
    pub equals: Token![=],
    /// The brackets for the type mappings.
    pub bracket: Bracket,
    /// The content of the type mappings.
    pub content: Punctuated<AllowedTypesMapping, Token![,]>,
}

impl Parse for AllowedTypesList {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Self {
            types: input.parse()?,
            equals: input.parse()?,
            bracket: bracketed!(content in input),
            content: content.parse_terminated(AllowedTypesMapping::parse, Token![,])?,
        })
    }
}

/// A mapping from a type to a name for that type in the allowed types enum.
pub struct AllowedTypesMapping {
    /// Attributes applied to this member.
    pub attributes: Vec<Attribute>,
    /// Rust name of the type.
    pub ty: Ident,
    /// Arrow between the type and the mapped name.
    pub arrow: Token![=>],
    /// Name to use for this type in the allowed types enum.
    pub ident: Ident,
}

impl Parse for AllowedTypesMapping {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(AllowedTypesMapping {
            attributes: Attribute::parse_outer(input)?,
            ty: input.parse()?,
            arrow: input.parse()?,
            ident: input.parse()?,
        })
    }
}

mod kw {
    syn::custom_keyword!(types);
    syn::custom_keyword!(name);
}
