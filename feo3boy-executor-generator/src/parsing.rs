use derive_syn_parse::Parse;
use proc_macro2::Ident;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::token::Bracket;
use syn::{bracketed, Attribute, Path, Token, Visibility};

/// Definition of an executor.
///
/// ```ignore
/// name = MyExecutor,
/// externs = [
///     ExternName => crate::some::externdef,
/// ],
/// ```
#[derive(Parse)]
pub struct ExecutorDef {
    /// Docs to apply to the microcode executor.
    #[call(Attribute::parse_outer)]
    pub attrs: Vec<Attribute>,
    /// Visibility of the new executor.
    pub vis: Visibility,
    /// Ident name of the new executor.
    pub name: Ident,
    /// Comma between name and externs list.
    pub comma: Token![,],
    /// Listing of allowed types.
    pub externs: ExecutorExterns,
    /// Optional trailing comma after the brackets.
    pub trailing_comma: Option<Token![,]>,
}

/// A mapping of externs for an executor.
///
/// ```ignore
/// externs = [
///     ExternName => crate::some::externdef,
/// ]
/// ```
pub struct ExecutorExterns {
    /// The `externs` token.
    pub externs: kw::externs,
    /// The `=` in `externs = [...]`
    pub equals: Token![=],
    /// The `[]` in `externs = [...]`
    pub bracket: Bracket,
    /// The contents of the externs list.
    pub content: Punctuated<ExternMapping, Token![,]>,
}

impl Parse for ExecutorExterns {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            externs: input.parse()?,
            equals: input.parse()?,
            bracket: bracketed!(content in input),
            content: content.parse_terminated(ExternMapping::parse, Token![,])?,
        })
    }
}

/// A single entry in the ExecutorExterns mapping, not including the comma separator.
///
/// ```ignore
/// ExternName => crate::some::externdef
/// ```
#[derive(Parse)]
pub struct ExternMapping {
    /// Identifier name of the extern being mapped.
    pub name: Ident,
    /// The `=>` token between the name and the path.
    pub arrow: Token![=>],
    /// Path to the function that implements this extern.
    pub path: Path,
}

mod kw {
    syn::custom_keyword!(externs);
}
