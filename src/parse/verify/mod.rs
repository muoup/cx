use std::iter;
use std::process::id;
use cranelift::codegen::verifier::verify_context;
use crate::log_error;
use crate::parse::verify::bytecode::{BytecodeBuilder, VerifiedFunction, VirtualInstruction};
use crate::parse::ast::{GlobalStatement, ValueType, AST};
use crate::parse::verify::context::{FnMap, TypeMap, VerifyContext};
use crate::parse::verify::import_module::{import_file};
use crate::parse::verify::typeless_declarations::{gen_const_decls, gen_fn_decls, gen_imports, gen_type_decls};
use crate::parse::verify::verify_expression::verify_expression;
use crate::parse::verify::verify_type::{get_type_size, verify_fn_prototype, verify_type};
use crate::util::ScopedMap;

pub mod context;
pub mod bytecode;
pub mod verify_type;

mod typeless_declarations;
mod verify_expression;
mod special_exprs;
mod import_module;

#[derive(Debug)]
pub struct VerifiedAST {
    pub fn_map: FnMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<VerifiedFunction>
}

pub fn verify_ast(mut ast: AST) -> Option<VerifiedAST> {
    for file in gen_imports(&ast)?.iter() {
        let Some(_) = import_file(&mut ast, file.as_str()) else {
            log_error!("Failed to import file {}", file);
        };
    }

    let mut type_map = gen_type_decls(&ast)?;
    let mut fn_map = gen_fn_decls(&ast)?;
    let mut constants_map = gen_const_decls(&ast)?;

    let function_bodies = ast.statements.into_iter()
        .filter_map(|stmt| {
            let GlobalStatement::Function { prototype, body } = stmt else {
                return None;
            };

            Some((prototype.name, body?))
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
        merge_stack: Vec::new()
    };

    let mut builder = BytecodeBuilder::new();

    for (name, body) in function_bodies {
        let Some(prototype) = verify_context.get_function(name.as_str()).cloned() else {
            log_error!("Function {} not found", name);
        };

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
                    memory
                },
                arg.type_.clone()
            )?;

            verify_context.insert_variable(arg.name.clone(), memory);
        }

        for stmt in body.iter() {
            verify_expression(&mut verify_context, &mut builder, stmt)?;
        }

        // Add implicit return to void functions
        if verify_context.current_return_type.is_none() {
            builder.add_instruction(
                &verify_context,
                VirtualInstruction::Return { value: None },
                ValueType::Unit
            );
        }

        builder.finish_function();
    }

    builder.finish(verify_context.fn_map, verify_context.type_map)
}