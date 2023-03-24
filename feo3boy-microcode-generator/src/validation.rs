use syn::{Error, Result, Type};

use crate::defs::{Defs, MicrocodeOp};

/// Provides basic validation of the extracted definitions before code generation.
pub fn validate_defs(defs: &Defs) -> Result<()> {
    for op in &defs.microcode_type.ops {
        validate_op(op, defs)?;
    }

    Ok(())
}

/// Validates a particular microcode op.
fn validate_op(code: &MicrocodeOp, defs: &Defs) -> Result<()> {
    for arg in &code.args {
        if arg.field_info.is_none() {
            validate_type(&arg.ty, defs)?;
        }
    }
    for ret in &code.returns {
        validate_type(&ret.ty, defs)?;
    }
    Ok(())
}

/// Verify that the given type is in the allowed types mapping.
fn validate_type(ty: &Type, defs: &Defs) -> Result<()> {
    match ty {
        Type::Path(path) => {
            if path.qself.is_some() {
                return Err(Error::new_spanned(
                    ty,
                    "Only simple identifier types are allowed as microcode stack types.",
                ));
            }
            match path.path.get_ident() {
                Some(ident) => {
                    if defs
                        .allowed_types
                        .types
                        .iter()
                        .find(|allowed| ident == allowed.ty.to_string().as_str())
                        .is_none()
                    {
                        return Err(Error::new(
                            ident.span(),
                            "Does not match any of the allowed types.",
                        ));
                    }
                }
                None => {
                    return Err(Error::new_spanned(
                        ty,
                        "Only simple identifier types are allowed as microcode stack types.",
                    ))
                }
            }
        }
        _ => {
            return Err(Error::new_spanned(
                ty,
                "Only simple identifier types are allowed as microcode stack types.",
            ))
        }
    }
    Ok(())
}
