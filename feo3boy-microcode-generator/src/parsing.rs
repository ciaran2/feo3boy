use proc_macro2::Ident;
use syn::parse::{Error, Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::Bracket;
use syn::{bracketed, Attribute, Token};

/// The contents of the `allowed_types!` macro invocation.
pub struct AllowedTypesMappings {
    pub name: AllowedTypesName,
    /// The comma token between the name and the types list.
    pub comma: Token![,],
    /// The listing of allowed types.
    pub types: AllowedTypesList,
    /// Optional trailing comma after the brackets.
    pub trailing_comma: Option<Token![,]>,
}

impl Parse for AllowedTypesMappings {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(AllowedTypesMappings {
            name: input.parse()?,
            comma: input.parse()?,
            types: input.parse()?,
            trailing_comma: input.parse()?,
        })
    }
}

/// Defines the mapping of the allowed-types name.
pub struct AllowedTypesName {
    /// The literal token `name`.
    pub name: Name,
    /// The assignment on the first line of the macro invocation.
    pub equals: Token![=],
    /// The identifier for the AllowedTypes generated type.
    pub ident: Ident,
}

impl Parse for AllowedTypesName {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            name: input.parse()?,
            equals: input.parse()?,
            ident: input.parse()?,
        })
    }
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
    pub types: Types,
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

/// The literal ident "types"
pub struct Types {
    pub ident: Ident,
}

impl Parse for Types {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        if ident != "types" {
            Err(Error::new(ident.span(), "Expected \"types\""))
        } else {
            Ok(Self { ident })
        }
    }
}

/// The literal ident "name"
pub struct Name {
    pub ident: Ident,
}

impl Parse for Name {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        if ident != "name" {
            Err(Error::new(ident.span(), "Expected \"name\""))
        } else {
            Ok(Self { ident })
        }
    }
}
