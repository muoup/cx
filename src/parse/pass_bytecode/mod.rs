use std::iter;
use std::process::id;
use cranelift::codegen::verifier::verify_context;
use crate::log_error;
use crate::parse::pass_bytecode::bytecode::{BytecodeBuilder, VerifiedFunction, VirtualInstruction};
use crate::parse::pass_bytecode::context::{FnMap, TypeMap, VerifyContext};
use crate::parse::pass_bytecode::import_module::import_file;
use crate::parse::pass_bytecode::name_mangling::member_function_mangle;
use crate::parse::pass_bytecode::typeless_declarations::{gen_const_decls, gen_fn_decls, gen_imports, gen_type_decls};
use crate::parse::pass_bytecode::verify_expression::verify_expression;
use crate::parse::pass_bytecode::verify_type::{get_type_size, verify_fn_prototype, verify_type};
use crate::parse::value_type::ValueType;
use crate::util::ScopedMap;

pub mod context;
pub mod bytecode;
pub mod verify_type;

mod typeless_declarations;
mod verify_expression;
mod special_exprs;
mod name_mangling;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: FnMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<VerifiedFunction>,

    pub imports: Vec<String>
}

pub fn gen_bytecode(mut ast: &CXAST) -> Option<ProgramBytecode> {
    let imports = gen_imports(&ast)?;
    let mut builder = BytecodeBuilder::new();

    for (name, body) in function_bodies {
        let Some(prototype) = verify_context.get_function(name.as_str()).cloned() else {
            log_error!("Function {} not found", name);
        };

        verify_context.var_map.push_scope();
        builder.new_function(prototype.clone());
        verify_context.current_return_type = match prototype.return_type {
            ValueType::Unit => None,
            _ => Some(prototype.return_type.clone())
        };

        let mut iter = prototype.args.iter();

        for (i, arg) in iter.enumerate() {
            if arg.name.starts_with("__hidden") {
                continue;
            }

            let memory = builder.add_instruction(
                &verify_context,
                VirtualInstruction::Allocate {
                    size: get_type_size(&verify_context.type_map, &arg.type_)?
                },
                arg.type_.clone()
            )?;

            let value = builder.add_instruction(
                &verify_context,
                VirtualInstruction::FunctionParameter {
                    param_index: i as u32,
                },
                arg.type_.clone()
            )?;

            builder.add_instruction(
                &verify_context,
                VirtualInstruction::Store {
                    value,
                    memory,
                    type_: arg.type_.clone()
                },
                ValueType::Unit
            )?;

            verify_context.insert_variable(arg.name.clone(), memory);
        }

        for stmt in body.iter() {
            let Some(_) = verify_expression(&mut verify_context, &mut builder, stmt) else {
                log_error!("Failed to verify expression: {:?}", stmt);
            };
        }

        let last_instruction = builder.last_instruction()
            .map(|instr| &instr.instruction);

        verify_context.var_map.pop_scope();
        builder.finish_function();
    }

    builder.finish(verify_context.fn_map, verify_context.type_map, imports)
}

fn gen_fn_bytecode(
    builder: &mut BytecodeBuilder,
    func: &,
)