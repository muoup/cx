use std::iter;
use std::process::id;
use cranelift::codegen::verifier::verify_context;
use crate::log_error;
use crate::parse::pass_verified::bytecode::{BytecodeBuilder, VerifiedFunction, VirtualInstruction};
use crate::parse::ast::{GlobalStatement, ValueType, AST};
use crate::parse::pass_verified::context::{FnMap, TypeMap, VerifyContext};
use crate::parse::pass_verified::import_module::{import_file};
use crate::parse::pass_verified::name_mangling::member_function_mangle;
use crate::parse::pass_verified::typeless_declarations::{gen_const_decls, gen_fn_decls, gen_imports, gen_type_decls};
use crate::parse::pass_verified::verify_expression::verify_expression;
use crate::parse::pass_verified::verify_type::{get_type_size, verify_fn_prototype, verify_type};
use crate::util::ScopedMap;

pub mod context;
pub mod bytecode;
pub mod verify_type;

mod typeless_declarations;
mod verify_expression;
mod special_exprs;
mod import_module;
mod name_mangling;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: FnMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<VerifiedFunction>,

    pub imports: Vec<String>
}

pub fn verify_ast(mut ast: AST) -> Option<ProgramBytecode> {
    let imports = gen_imports(&ast)?;

    for file in imports.iter() {
        let Some(_) = import_file(&mut ast, file.as_str()) else {
            log_error!("Failed to import file {}", file);
        };
    }

    let mut type_map = gen_type_decls(&ast)?;
    let mut fn_map = gen_fn_decls(&ast)?;
    let mut constants_map = gen_const_decls(&ast)?;

    let function_bodies = ast.statements.into_iter()
        .filter_map(|stmt| {
            match stmt {
                GlobalStatement::Function { prototype, body } =>
                    Some((prototype.name, body?)),

                GlobalStatement::MemberFunction { struct_parent, prototype, body } =>
                    Some((member_function_mangle(&struct_parent, &prototype.name), body?)),

                _ => None
            }
        })
        .collect::<Vec<_>>();

    let type_map_iter = type_map.iter()
        .map(|(key, _type)| (key, verify_type(&type_map, _type.clone())));
    let mut type_map = TypeMap::new();

    for (_key, _type) in type_map_iter {
        type_map.insert(_key.clone(), _type?);
    }

    let fn_map_iter = fn_map.into_iter()
        .map(|(key, prototype)| (key, verify_fn_prototype(&type_map, prototype)));
    let mut fn_map = FnMap::new();

    for (_key, prototype) in fn_map_iter {
        fn_map.insert(_key, prototype?);
    }

    let mut verify_context = VerifyContext {
        type_map,
        fn_map,
        constants_map,
        var_map: ScopedMap::new(),

        current_return_type: None,
        merge_stack: Vec::new(),
    };

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

        if !matches!(last_instruction, Some(VirtualInstruction::Return { .. })) {
            if verify_context.current_return_type.is_some() {
                log_error!("Function {} does not have a return value", name);
            }

            builder.add_instruction(
                &verify_context,
                VirtualInstruction::Return { value: None },
                ValueType::Unit
            );
        }

        verify_context.var_map.pop_scope();
        builder.finish_function();
    }

    builder.finish(verify_context.fn_map, verify_context.type_map, imports)
}