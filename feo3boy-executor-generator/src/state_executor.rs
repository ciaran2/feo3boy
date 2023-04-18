use std::collections::{BTreeMap, HashMap};

use feo3boy_opcodes::compiler::args::{Arg, Literal};
use feo3boy_opcodes::compiler::instr::flow::{ElementPath, ElementPathBuf};
use feo3boy_opcodes::compiler::instr::InstrId;
use feo3boy_opcodes::compiler::state_executor_generation::{
    InstrStates, State, StateBlock, StateBranch, StateChange, StateElement, StateMicrocode,
};
use feo3boy_opcodes::compiler::variables::VarAssignment;
use feo3boy_opcodes::compiler::OperationType;
use feo3boy_opcodes::microcode::Microcode;
use feo3boy_opcodes::opcode::{CBOpcode, InternalFetch, Opcode};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Attribute, Path, Result, Visibility};

use crate::get_crate;
use crate::parsing::ExecutorDef;
use crate::types::{generate_conversions, get_type};

struct StateInfo {
    /// The actual state.
    state: State,
    /// Name to use for this state, if this state should actually be included in the enum
    /// output. The initial steps of Opocdes and CBOpcodes do not get states because they
    /// are executed directly by the corresponding function.
    name: Option<Ident>,
}

/// Code generator for a StateMachineExecutor.
pub struct StateExecutor {
    /// Attributes for the executor.
    attrs: Vec<Attribute>,
    /// Visibility of the resulting executor.
    vis: Visibility,
    /// Identifier for the executor.
    name: Ident,
    /// Name of the state type for the executor.
    state_name: Ident,
    /// Mappings from microcode extern instruction names to the paths of functions that
    /// implement them, relative to the module the direct executor is defined in.
    externs: HashMap<String, Path>,
    /// Collected map of all states for all InstrIds.
    states: BTreeMap<InstrId, BTreeMap<ElementPathBuf, StateInfo>>,
}

impl StateExecutor {
    /// Extract the executor def from this
    pub fn extract(executor: ExecutorDef) -> Result<Self> {
        let externs = executor
            .externs
            .content
            .into_iter()
            .map(|ext| (ext.name.to_string(), ext.path))
            .collect();

        let mut states: BTreeMap<_, BTreeMap<_, _>> = BTreeMap::new();
        {
            let instr = InternalFetch.into();
            for (i, (path, state)) in InstrStates::new(&instr).into_states().enumerate() {
                let state_info = StateInfo {
                    state,
                    name: Some(format_ident!("InternalFetchState{i}")),
                };
                states
                    .entry(instr.id())
                    .or_default()
                    .insert(path, state_info);
            }
        }
        for opcode in 0..=0xffu8 {
            let instr = Opcode::decode(opcode).into();
            for (i, (path, state)) in InstrStates::new(&instr).into_states().enumerate() {
                let state_info = StateInfo {
                    state,
                    name: if path != ElementPath::EMPTY {
                        Some(format_ident!("Opcode0x{opcode:0>2X}State{i}"))
                    } else {
                        None
                    },
                };
                states
                    .entry(instr.id())
                    .or_default()
                    .insert(path, state_info);
            }
        }
        for cbopcode in 0..=0xffu8 {
            let instr = CBOpcode::decode(cbopcode).into();
            for (i, (path, state)) in InstrStates::new(&instr).into_states().enumerate() {
                let state_info = StateInfo {
                    state,
                    name: if path != ElementPath::EMPTY {
                        Some(format_ident!("CBOpcode0x{cbopcode:0>2X}State{i}"))
                    } else {
                        None
                    },
                };
                states
                    .entry(instr.id())
                    .or_default()
                    .insert(path, state_info);
            }
        }

        let state_name = format_ident!("{}State", executor.name);

        Ok(Self {
            attrs: executor.attrs,
            name: executor.name,
            state_name,
            vis: executor.vis,
            externs,
            states,
        })
    }

    pub fn generate(&self) -> TokenStream {
        let ex_def = self.define_executor();
        let state_def = self.define_state();
        let sub_ex = self.define_subinstr_execution();
        let opcode_ex = self.define_opcode_execution();

        quote! {
            #ex_def
            #state_def
            #sub_ex
            #opcode_ex
        }
    }

    /// Get an iterator over all of the states.
    fn iter_states(&self) -> impl Iterator<Item = (InstrId, &ElementPath, &StateInfo)> {
        self.states.iter().flat_map(|(instr, instr_states)| {
            instr_states
                .iter()
                .map(|(path, state_info)| (*instr, path.as_path(), state_info))
        })
    }

    /// Define the top-level executor.
    fn define_executor(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");

        let attrs = &self.attrs;
        let vis = &self.vis;
        let name = &self.name;
        let state_name = &self.state_name;
        let ctx_ty = self.ctx_ty();

        quote! {
            #(#attrs)*
            #vis struct #name;

            impl #feo3boy::gbz80core::executor::Executor for #name {
                type State = #state_name;

                fn run_single_instruction(ctx: #ctx_ty) {
                    use #feo3boy::gbz80core::executor::{SubInstructionExecutor, PausePoint};
                    loop {
                        match <#name as SubInstructionExecutor>::tick_until_yield_or_fetch(ctx) {
                            PausePoint::Yield => #feo3boy::gbz80core::ExecutorContext::yield1m(ctx),
                            PausePoint::Fetch => break,
                        }
                    }
                }
            }
        }
    }

    /// Define the enum of states for this executor.
    fn define_state(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let prev_ime_ty = self.prev_ime_ty();

        let state_defs = self.iter_states().filter_map(|(instr, path, state_info)| {
            state_info.name.as_ref().map(|name| {
                if (instr, path) == (InstrId::InternalFetch, ElementPath::EMPTY) {
                    // The first state of InternalFetch has no fields and is the
                    // default state.
                    quote! {
                        #[default]
                        #name
                    }
                } else {
                    let fields = state_info.state.stored_stack.iter().map(|field| {
                        let var = &field.var;
                        let val = get_type(field.val);
                        quote! {
                            #var: #val
                        }
                    });
                    quote! {
                        #name {
                            prev_ime: #prev_ime_ty,
                            #(#fields,)*
                        }
                    }
                }
            })
        });
        let state_name = &self.state_name;
        let vis = &self.vis;
        let doc = format!("State for the [`{}`] executor", self.name);
        quote! {
            #[doc = #doc]
            #[derive(Clone, Debug, Default, Eq, PartialEq)]
            #vis enum #state_name {
                #(#state_defs,)*
            }

            impl #feo3boy::gbz80core::executor::ExecutorState for #state_name {}
        }
    }

    /// Define the code to match the current state and step the state machine.
    fn define_subinstr_execution(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let log = get_crate("log");
        let name = &self.name;
        let state_name = &self.state_name;
        let ctx_ty = &self.ctx_ty();
        let prev_ime_ty = self.prev_ime_ty();

        let states = self.iter_states().filter_map(|(instr, path, state_info)| {
            state_info.name.as_ref().map(|name| {
                let (fields, init);
                if (instr, path) == (InstrId::InternalFetch, ElementPath::EMPTY) {
                    fields = quote! {};
                    init = quote! {
                        let mut prev_ime: #prev_ime_ty = None;
                        #log::trace!("[Internal Fetch]");
                    };
                } else {
                    let stored = state_info.state.stored_stack.iter().map(|store| store.var);
                    fields = quote! {
                        {
                            mut prev_ime,
                            #(#stored,)*
                        }
                    };
                    init = quote! {}
                }
                let body = self.generate_body(instr, &state_info.state.element);
                quote! {
                    #state_name::#name #fields => {
                        #init
                        #body
                    }
                }
            })
        });

        quote! {
            impl #feo3boy::gbz80core::executor::SubInstructionExecutor for #name {
                fn tick_until_yield_or_fetch(ctx: #ctx_ty) -> #feo3boy::gbz80core::executor::PausePoint {
                    match *#feo3boy::gbz80core::ExecutorContext::executor(ctx) {
                        #(#states,)*
                    }
                }
            }
        }
    }

    fn define_opcode_execution(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let log = get_crate("log");
        let name = &self.name;
        let ctx_ty = self.ctx_ty();
        let prev_ime_ty = self.prev_ime_ty();

        let opcode = (0..=0xffu8).map(|opcode| {
            let op = Opcode::decode(opcode);
            let id = InstrId::Opcode(op);
            let body = self.generate_body(id, &self.states[&id][ElementPath::EMPTY].state.element);
            let pat = format!("0x{opcode:0>2X}u8")
                .parse::<proc_macro2::Literal>()
                .unwrap();
            let trace = format!("Executing opcode {opcode:0>2X}: {op}");
            quote! {
                #pat => {
                    #log::trace!(#trace);
                    #body
                }
            }
        });

        let cbopcode = (0..=0xffu8).map(|cbopcode| {
            let op = CBOpcode::decode(cbopcode);
            let id = InstrId::CBOpcode(op);
            let body = self.generate_body(id, &self.states[&id][ElementPath::EMPTY].state.element);
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
            impl #name {
                /// Execute a single Opcode.
                fn run_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, opcode: u8) -> #feo3boy::gbz80core::executor::PausePoint {
                    match opcode {
                        #(#opcode)*
                    }
                }

                /// Execute a single CBOpcode.
                fn run_cb_opcode(ctx: #ctx_ty, mut prev_ime: #prev_ime_ty, cbopcode: u8) -> #feo3boy::gbz80core::executor::PausePoint {
                    match cbopcode {
                        #(#cbopcode)*
                    }
                }
            }
        }
    }

    /// Generate a body for a State.
    fn generate_body(&self, instr: InstrId, body: &StateElement) -> TokenStream {
        match body {
            StateElement::Microcode(microcode) => self.generate_microcode(microcode),
            StateElement::Block(block) => self.generate_block(instr, block),
            StateElement::Branch(branch) => self.generate_branch(instr, branch),
            StateElement::ChangeState(state) => self.generate_state_change(instr, state),
        }
    }

    /// Generate code for a single microcode.
    fn generate_microcode(&self, code: &StateMicrocode) -> TokenStream {
        let desc = code.microcode.descriptor();
        let feo3boy = get_crate("feo3boy");
        let feo3boy_opcodes = get_crate("feo3boy-opcodes");

        let executor = &self.name;
        let state_name = &self.state_name;

        let conversions = generate_conversions(&code.conversions);
        let args = code.args.iter().map(|arg| match arg {
            Arg::StackValue(VarAssignment { var, .. }) => quote! { #var },
            Arg::Literal(literal) => literal.constant_value(get_crate),
        });

        let return_vars = code.returns.iter().map(|ret| ret.var);
        let return_vals = code.returns.iter().map(|ret| get_type(ret.val));

        let fetch_state_name = self
            .states
            .get(&InstrId::InternalFetch)
            .expect("InternalFetch states not found")
            .get(ElementPath::EMPTY)
            .expect("Initial State of InternalFetch not found")
            .name
            .as_ref()
            .expect("Fetch initial state did not have a name assigned.");

        match desc.optype {
            OperationType::Extern => match code.microcode {
                Microcode::FetchNextInstruction => quote! {
                    if let Some(prev_ime) = prev_ime {
                        let cpu = #feo3boy::gbz80core::CpuContext::cpu_mut(ctx);
                        cpu.interrupt_master_enable.tick(prev_ime);
                    }
                    *#feo3boy::gbz80core::ExecutorContext::executor_mut(ctx) = #state_name::#fetch_state_name;
                    return #feo3boy::gbz80core::executor::PausePoint::Fetch;
                },
                Microcode::ParseOpcode => quote! {
                    #conversions
                    return #executor::run_opcode(ctx, prev_ime, #(#args),*);
                },
                Microcode::ParseCBOpcode => quote! {
                    #conversions
                    return #executor::run_cb_opcode(ctx, prev_ime, #(#args),*);
                },
                Microcode::TickImeOnEnd => quote! {
                    debug_assert!(
                        prev_ime.is_none(),
                        "new call to TickImeOnEnd when prev_ime is already set",
                    );
                    prev_ime = Some(#feo3boy::gbz80core::CpuContext::cpu(ctx).interrupt_master_enable);
                },
                Microcode::Yield => panic!("Yields should have been converted to state changes"),
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
                        #conversions
                        let (#(#return_vars),*): (#(#return_vals),*) = #path(
                            ctx,
                            #(#args),*
                        );
                    }
                }
            },
            OperationType::Function { path } => {
                quote! {
                    #conversions
                    let (#(#return_vars),*): (#(#return_vals),*) = #feo3boy_opcodes::microcode::#path(
                        #(#args),*
                    );
                }
            }
        }
    }

    fn generate_block(&self, instr: InstrId, block: &StateBlock) -> TokenStream {
        block
            .elements
            .iter()
            .map(|elem| self.generate_body(instr, elem))
            .collect()
    }

    fn generate_branch(&self, instr: InstrId, branch: &StateBranch) -> TokenStream {
        let cond_conversion = generate_conversions(&branch.cond_conversion);
        let cond = &branch.cond;

        let code_if_true = self.generate_body(instr, &branch.code_if_true);
        let true_end_conv = generate_conversions(&branch.true_end_conv);
        let true_ret_vars = branch.true_returns.iter().map(|ret| ret.var);
        let true_ret_vars = if branch.true_returns.is_empty() {
            None
        } else {
            Some(quote! { (#(#true_ret_vars),*) })
        };

        let code_if_false = self.generate_body(instr, &branch.code_if_false);
        let false_end_conv = generate_conversions(&branch.false_end_conv);
        let false_ret_vars = branch.false_returns.iter().map(|ret| ret.var);
        let false_ret_vars = if branch.false_returns.is_empty() {
            None
        } else {
            Some(quote! { (#(#false_ret_vars),*) })
        };

        let push_vars = branch.pushes.iter().map(|push| push.var);
        let push_vals = branch.pushes.iter().map(|push| get_type(push.val));

        quote! {
            #cond_conversion
            let (#(#push_vars),*): (#(#push_vals),*) = if #cond {
                #code_if_true
                #true_end_conv
                #true_ret_vars
            } else {
                #code_if_false
                #false_end_conv
                #false_ret_vars
            };
        }
    }

    fn generate_state_change(&self, instr: InstrId, state_change: &StateChange) -> TokenStream {
        assert!(
            (instr, state_change.next_state.as_path()) != (InstrId::InternalFetch, ElementPath::EMPTY),
            "Cannot change to the internal_fetch initial state. That should be FetchNextInstruction instead",
        );

        let feo3boy = get_crate("feo3boy");
        let instr_states = &self.states[&instr];
        let state_name = &self.state_name;
        let next_state = instr_states
            .get(&state_change.next_state)
            .expect("Destination state not found")
            .name
            .as_ref()
            .expect("Destination state not named");

        let fields = state_change.saved_vars.iter().map(|var| var.var);

        quote! {
            *#feo3boy::gbz80core::ExecutorContext::executor_mut(ctx) = #state_name::#next_state {
                prev_ime,
                #(#fields,)*
            };
            return #feo3boy::gbz80core::executor::PausePoint::Yield;
        }
    }

    /// Get the type used for context in this executor.
    fn ctx_ty(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        let state_name = &self.state_name;
        quote! {
            &mut impl #feo3boy::gbz80core::ExecutorContext<State = #state_name>
        }
    }

    /// Get the type used for the prev_ime field.
    fn prev_ime_ty(&self) -> TokenStream {
        let feo3boy = get_crate("feo3boy");
        quote! {
            Option<#feo3boy::gbz80core::InterruptMasterState>
        }
    }
}
