use std::collections::HashMap;

use feo3boy_opcodes::compiler::args::{Arg, Literal};
use feo3boy_opcodes::compiler::direct_executor_generation::{
    AssignedBlock, AssignedBranch, AssignedMicrocode, FuncElement,
};
use feo3boy_opcodes::compiler::variables::{Conversion, VarAssignment};
use feo3boy_opcodes::compiler::OperationType;
use feo3boy_opcodes::microcode::{Microcode, ValType};
use feo3boy_opcodes::opcode::{CBOpcode, InternalFetch, Opcode};
use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{Attribute, Path, Result, Visibility};

use crate::get_crate;
use crate::parsing::ExecutorDef;

/// Code generator for the DirectExecutor.
pub struct DirectExecutor {
    /// Attributes for the executor.
    attrs: Vec<Attribute>,
    /// Visibility of the resulting executor.
    vis: Visibility,
    /// Identifier for the executor.
    name: Ident,
    /// Mappings from microcode extern instruction names to the paths of functions that
    /// implement them, relative to the module the direct executor is defined in.
    externs: HashMap<String, Path>,
}

impl DirectExecutor {
    pub fn extract(executor: ExecutorDef) -> Result<Self> {
        let externs = executor
            .externs
            .content
            .into_iter()
            .map(|ext| (ext.name.to_string(), ext.path))
            .collect();

        Ok(Self {
            attrs: executor.attrs,
            name: executor.name,
            vis: executor.vis,
            externs,
        })
    }

    /// Generate the executor.
    pub fn generate(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let log = get_crate("log");

        let name = &self.name;
        let attrs = &self.attrs;
        let vis = &self.vis;

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
            #(#attrs)*
            #vis struct #name;

            impl #name {
                /// Execute a single Opcode.
                fn run_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, opcode: u8) {
                    match opcode {
                        #(#opcode)*
                    }
                }

                /// Execute a single CBOpcode.
                fn run_cb_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, cbopcode: u8) {
                    match cbopcode {
                        #(#cbopcode)*
                    }
                }
            }

            impl #feo3boy::gbz80core::executor::Executor for #name {
                type State = ();

                fn run_single_instruction(ctx: #ctx_ty) {
                    let mut prev_ime: #prev_ime_ty = None;
                    #log::trace!("[Internal Fetch]");
                    #fetch
                }
            }
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
                        let path = match self.externs.get(&desc.name) {
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
        let true_ret_vars = if branch.true_returns.is_empty() {
            None
        } else {
            Some(quote! { (#(#true_ret_vars),*) })
        };

        let code_if_false = self.generate_body(&branch.code_if_false);
        let false_end_conv = branch
            .true_end_conv
            .iter()
            .map(|conv| self.generate_conversion(conv));
        let false_ret_vars = branch.false_returns.iter().map(|ret| ret.var);
        let false_ret_vars = if branch.false_returns.is_empty() {
            None
        } else {
            Some(quote! { (#(#false_ret_vars),*) })
        };

        let push_vars = branch.pushes.iter().map(|push| push.var);
        let push_vals = branch.pushes.iter().map(|push| get_type(push.val));

        quote! {
            #(#cond_conversion)*
            let (#(#push_vars),*): (#(#push_vals),*) = if #cond {
                #code_if_true
                #(#true_end_conv)*
                #true_ret_vars
            } else {
                #code_if_false
                #(#false_end_conv)*
                #false_ret_vars
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
