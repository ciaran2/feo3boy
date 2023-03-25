use proc_macro2::Ident;
use syn::{Attribute, ItemMod, Signature, Type, Visibility};

pub struct Defs {
    /// A transformed version of the input module, with all of the special attributes
    /// stripped and any extern function defs removed.
    pub module: ItemMod,

    /// The `Microcode` type being defined.
    pub microcode_type: MicrocodeTypeDef,

    /// The allowed types in this microcode's microcode stack.
    pub allowed_types: AllowedTypes,
}

/// Definition of the microcode type.
pub struct MicrocodeTypeDef {
    /// Name used for the `Microcode` type.
    pub name: Ident,
    /// Visibility to apply to the microcode type.
    pub vis: Visibility,
    /// Doc comments from the definition module.
    pub docs: Vec<Attribute>,

    /// List of operations defined by this microcode.
    pub ops: Vec<MicrocodeOp>,

    /// Name of the type that defines the extern names.
    pub externs_name: Ident,

    /// Name of the type that defines the descriptors.
    pub descriptor_name: Ident,
}

/// Defines a microcode operation defined in the `Microcode` type.
pub struct MicrocodeOp {
    /// Name of the microcode operation. This is the name that is inserted into the enum.
    pub name: Ident,
    /// Name of the function that defines this operation.
    pub func_name: Ident,
    /// Docs for this operation, taken from the function.
    pub docs: Vec<Attribute>,
    /// Arguments to the function that defines this operation, in order of appearance.
    /// Order of `#[field]` arguments isn't significant, however other arguments should
    /// appear in the order they are to be popped off the stack.
    pub args: Vec<MicrocodeArg>,
    /// Return values of the microcode. The return values are values to be added to the
    /// microcode stack after the opcode completes. Values should be returned in the order
    /// they are to be pushed.
    pub returns: Vec<MicrocodeRes>,
    /// Which type of microcode this is.
    pub subtype: MicrocodeSubtype,
    /// Signature of the function that implements this operation.
    pub sig: Signature,
}

/// Defines the possible types of microcode operations.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MicrocodeSubtype {
    /// An externally-defined microcode operation. The behavior of the operation will be
    /// determined by the microcode executor.
    Extern,
    /// A microcode operation which is a pure function of values popped off the microcode
    /// stack to values pushed onto the microcode stack.
    Function,
}

/// Where a microcode argument can come from.
pub enum ArgSource {
    /// Argument comes from the microcode stack.
    Stack(StackInfo),
    /// Argument comes from a field in the microcode enum.
    Field(FieldInfo),
}

/// An argument to a microcode function. Most arguments come from the microcode stack,
/// however parameters can be declared as `#[field]`, which causes them to come from a
/// field in the Microcode enum instead. Note that `#[field]` arguments should not be used
/// in a branching way where possible (for example
pub struct MicrocodeArg {
    /// Type of the argument.
    ///
    /// Note: if the argument is a custom type, we don't do any special handling for
    /// figuring out where it is actually defined, and will assume it is also available at
    /// the top-level outside of the microcode definition module.
    pub ty: Type,
    /// How this argument is sourced.
    pub source: ArgSource,
}

/// Information about a stack-sourced argument.
pub struct StackInfo {
    /// Name of a stack-sourced argument (may be absent if the argument uses a
    /// non-ident pattern
    pub name: Option<Ident>,
}

/// Information about a microcode field parameter.
pub struct FieldInfo {
    /// Name of the field.
    pub name: Ident,
    /// Docs for the field.
    pub docs: Vec<Attribute>,
}

/// The output
pub struct MicrocodeRes {
    /// Type of the returned value. Must be a type which can be pushed to the microcode
    /// stack (though that will be determined by the executor).
    pub ty: Type,
}

/// Represents the enum of allowed types that will be generated.
pub struct AllowedTypes {
    /// Doc comments for the allowed types.
    pub docs: Vec<Attribute>,
    /// Name of the AllowedTypes enum.
    pub name: Ident,
    /// Visibility (copied from the defs module).
    pub vis: Visibility,
    /// The list of allowed types for the microcode stack.
    pub types: Vec<AllowedType>,
}

/// A type which is allowed in the Microcode as a value.
pub struct AllowedType {
    /// Docs fot this allowed type.
    pub docs: Vec<Attribute>,
    /// Type used in rust. For simplicity only idents are allowed.
    pub ty: Ident,
    /// Name used for thsi type in the AllowedTypes enum.
    pub name: Ident,
}
