use std::collections::HashMap;

use feo3boy_opcodes::compiler::args::{Arg, Literal};
use feo3boy_opcodes::compiler::direct_executor_generation::{
    AssignedBlock, AssignedBranch, AssignedMicrocode, Conversion, FuncElement, VarAssignment,
};
use feo3boy_opcodes::compiler::OperationType;
use feo3boy_opcodes::microcode::{Microcode, ValType};
use feo3boy_opcodes::opcode::{CBOpcode, InternalFetch, Opcode};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{parse_quote, Attribute, Error, Item, ItemMod, Meta, Path, Result, Visibility};

use crate::get_crate;

/// Code generator for the DirectExecutor.
pub struct DirectExecutor {
    /// Identifier for the executor.
    name: Ident,
    /// Docs for the executor.
    docs: Vec<Attribute>,
    /// Visibility of the resulting executor.
    vis: Visibility,
    /// Module that defines the function-externs.
    externs: ItemMod,
    /// Mappings from microcode extern instruction names to the paths of functions that
    /// implement them, relative to the module the direct executor is defined in.
    extern_funcs: HashMap<String, Path>,
}

impl DirectExecutor {
    pub fn extract(name: Ident, mut externs: ItemMod) -> Result<Self> {
        let docs = extract_docs(&externs.attrs);
        let vis = externs.vis.clone();

        let extern_funcs = match externs.content {
            Some(ref mut content) => extract_extern_funcs(&externs.ident, &mut content.1)?,
            None => {
                return Err(Error::new(
                    externs.ident.span(),
                    "define_microcode must be used on an inline module, not one defined \
                    externally",
                ));
            }
        };

        Ok(Self {
            name,
            docs,
            vis,
            externs,
            extern_funcs,
        })
    }

    /// Generate the executor.
    pub fn generate(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let log = get_crate("log");

        let name = &self.name;
        let docs = &self.docs;
        let vis = &self.vis;
        let module = &self.externs;

        let ctx_ty = quote! { &mut impl #feo3boy::gbz80core::ExecutorContext<State = ()> };
        let prev_ime_ty = quote! { Option<#feo3boy::gbz80core::InterruptMasterState> };

        let fetch = self.generate_body(&FuncElement::new(InternalFetch.into()));
        let opcode = (0u8..=0xff).map(|opcode| {
            let op = Opcode::decode(opcode);
            let body = self.generate_body(&FuncElement::new(op.into()));
            let pat = format!("0x{opcode:0>2X}u8")
                .parse::<proc_macro2::Literal>()
                .unwrap();
            let trace = format!("Executing opcode 0x{opcode:0>2X}: {op}");
            quote! {
                #pat => {
                    #log::trace!(#trace);
                    #body
                }
            }
        });
        let cbopcode = (0u8..=0xff).map(|cbopcode| {
            let op = CBOpcode::decode(cbopcode);
            let body = self.generate_body(&FuncElement::new(op.into()));
            let pat = format!("0x{cbopcode:0>2X}u8")
                .parse::<proc_macro2::Literal>()
                .unwrap();
            let trace = format!("Executing cb opcode {cbopcode:0>2X}: {op}");
            quote! {
                #pat => {
                    #log::trace!(#trace);
                    #body
                }
            }
        });

        quote! {
            #(#docs)*
            #vis struct #name;

            impl #name {
                /// Execute a single Opcode.
                #[allow(unreachable_code)]
                fn run_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, opcode: u8) {
                    match opcode {
                        #(#opcode)*
                    }
                }

                /// Execute a single CBOpcode.
                #[allow(unreachable_code)]
                fn run_cb_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, cbopcode: u8) {
                    match cbopcode {
                        #(#cbopcode)*
                    }
                }
            }

            impl #feo3boy::gbz80core::executor::Executor for #name {
                type State = ();

                #[allow(unreachable_code)]
                fn run_single_instruction(ctx: #ctx_ty) {
                    let mut prev_ime: #prev_ime_ty = None;
                    #log::trace!("[Internal Fetch]");
                    #fetch
                }
            }

            #module
        }
    }

    /// Generate the body of an instruction function.
    fn generate_body(&self, body: &FuncElement) -> TokenStream {
        match body {
            FuncElement::Microcode(microcode) => self.generate_microcode(microcode),
            FuncElement::Block(block) => self.generate_block(block),
            FuncElement::Branch(branch) => self.generate_branch(branch),
        }
    }

    fn generate_microcode(&self, code: &AssignedMicrocode) -> TokenStream {
        let desc = code.microcode.descriptor();
        let feo3boy = get_crate("feo3boy");
        let feo3boy_opcodes = get_crate("feo3boy-opcodes");

        let executor = &self.name;

        let conversions = code
            .conversions
            .iter()
            .map(|conv| self.generate_conversion(conv));

        let args = code.args.iter().map(|arg| match arg {
            Arg::StackValue(VarAssignment { var, .. }) => quote! { #var },
            Arg::Literal(literal) => literal.constant_value(get_crate),
        });

        let return_vars = code.returns.iter().map(|ret| ret.var);
        let return_vals = code.returns.iter().map(|ret| get_type(ret.val));

        match desc.optype {
            OperationType::Extern => {
                match code.microcode {
                    Microcode::FetchNextInstruction => quote! {
                        if let Some(prev_ime) = prev_ime {
                            let cpu = #feo3boy::gbz80core::CpuContext::cpu_mut(ctx);
                            cpu.interrupt_master_enable.tick(prev_ime);
                        }
                        // FetchNextInstruction is just equivalent to a return in the
                        // direct executor, after accounting for IME tick.
                        return;
                    },
                    Microcode::ParseOpcode => quote! {
                        #(#conversions)*
                        return #executor::run_opcode(ctx, prev_ime, #(#args),*);
                    },
                    Microcode::ParseCBOpcode => quote! {
                        #(#conversions)*
                        return #executor::run_cb_opcode(ctx, prev_ime, #(#args),*);
                    },
                    Microcode::TickImeOnEnd => quote! {
                        debug_assert!(
                            prev_ime.is_none(),
                            "new call to TickImeOnEnd when prev_ime is already set",
                        );
                        prev_ime = Some(#feo3boy::gbz80core::CpuContext::cpu(ctx).interrupt_master_enable);
                    },
                    Microcode::Yield => quote! {
                        #feo3boy::gbz80core::ExecutorContext::yield1m(ctx);
                    },
                    Microcode::Skip { .. } | Microcode::SkipIf { .. } => {
                        panic!("Skip and SkipIf are not allowed in AssignedMicrocode");
                    }
                    _ => {
                        let path = match self.extern_funcs.get(&desc.name) {
                            Some(path) => path,
                            None => {
                                panic!("Extern definition for Microcode {} not found", desc.name);
                            }
                        };
                        quote! {
                            #(#conversions)*
                            let (#(#return_vars),*): (#(#return_vals),*) = #path(
                                ctx,
                                #(#args),*
                            );
                        }
                    }
                }
            }
            OperationType::Function { path } => {
                quote! {
                    #(#conversions)*
                    let (#(#return_vars),*): (#(#return_vals),*) = #feo3boy_opcodes::microcode::#path(
                        #(#args),*
                    );
                }
            }
        }
    }

    fn generate_block(&self, block: &AssignedBlock) -> TokenStream {
        let ops = block.elements.iter().map(|elem| self.generate_body(elem));
        quote! {
            #(#ops)*
        }
    }

    fn generate_branch(&self, branch: &AssignedBranch) -> TokenStream {
        let cond_conversion = branch
            .cond_conversion
            .iter()
            .map(|conv| self.generate_conversion(conv));
        let cond = &branch.cond;

        let code_if_true = self.generate_body(&branch.code_if_true);
        let true_end_conv = branch
            .true_end_conv
            .iter()
            .map(|conv| self.generate_conversion(conv));
        let true_ret_vars = branch.true_returns.iter().map(|ret| ret.var);

        let code_if_false = self.generate_body(&branch.code_if_false);
        let false_end_conv = branch
            .true_end_conv
            .iter()
            .map(|conv| self.generate_conversion(conv));
        let false_ret_vars = branch.false_returns.iter().map(|ret| ret.var);

        let push_vars = branch.pushes.iter().map(|push| push.var);
        let push_vals = branch.pushes.iter().map(|push| get_type(push.val));

        quote! {
            #(#cond_conversion)*
            let (#(#push_vars),*): (#(#push_vals),*) = if #cond {
                #code_if_true
                #(#true_end_conv)*
                (#(#true_ret_vars),*)
            } else {
                #code_if_false
                #(#false_end_conv)*
                (#(#false_ret_vars),*)
            };
        }
    }

    fn generate_conversion(&self, conversion: &Conversion) -> TokenStream {
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
}

fn get_type(val: ValType) -> TokenStream {
    match val {
        ValType::Bool => quote! { bool },
        ValType::U16 => quote! { u16 },
        ValType::U8 => quote! { u8 },
        ValType::Flags => {
            let feo3boy_opcodes = get_crate("feo3boy-opcodes");
            quote! { #feo3boy_opcodes::gbz80types::Flags }
        }
    }
}

/// Extract the docs from a list of attributes.
fn extract_docs(attrs: &[Attribute]) -> Vec<Attribute> {
    attrs
        .iter()
        .filter(|attr| match &attr.meta {
            Meta::NameValue(nv) => nv.path.is_ident("doc"),
            _ => false,
        })
        .cloned()
        .collect()
}

/// Find all functions in the module with the `#[microcode_extern(ExternName)]` marker and
/// map them to the function to use to implement that extern.
fn extract_extern_funcs(mod_name: &Ident, items: &mut Vec<Item>) -> Result<HashMap<String, Path>> {
    items
        .iter_mut()
        .map(|item| {
            Ok(match item {
                Item::Fn(func) => extract_microcode_marker(&mut func.attrs)?.map(|ident| {
                    let func_name = &func.sig.ident;
                    (ident.to_string(), parse_quote!(#mod_name :: #func_name))
                }),
                _ => None,
            })
        })
        .filter_map(Result::transpose)
        .collect()
}

/// Reads through the attributes and finds one labeled `microcode_extern` or and extracts
/// the `Ident` from it. Returns None if the value is not a `microcode_extern` def.
fn extract_microcode_marker(attrs: &mut Vec<Attribute>) -> Result<Option<Ident>> {
    let mut res = None;

    let mut i = attrs.len();
    while i > 0 {
        i -= 1;
        match &attrs[i].meta {
            Meta::Path(path) => {
                if path.is_ident("microcode_extern") {
                    return Err(Error::new_spanned(
                        path,
                        "#[microcode_extern] marker on requires an argument to specify \
                        the name of the extern being defined",
                    ));
                }
                // Retain all other path attributes.
                continue;
            }
            Meta::List(list) => {
                if !list.path.is_ident("microcode_extern") {
                    // Not a microcode attribute, don't delete.
                    continue;
                };
                if !res.is_none() {
                    return Err(Error::new_spanned(
                        &list.path,
                        "may only specify one #[microcode_extern(...)] attribute.",
                    ));
                }
                res = Some(syn::parse2::<Ident>(list.tokens.clone())?);
            }
            Meta::NameValue(nv) => {
                if nv.path.is_ident("microcode_extern") {
                    return Err(Error::new_spanned(
                        &nv.path,
                        "`#[microcode_extern(...)]` uses a parenthsized argument not \
                        `microcode_extern = ... syntax`",
                    ));
                }
                // Not a microcode attribute, retain it.
                continue;
            }
        }
        attrs.remove(i);
    }
    Ok(res)
}
