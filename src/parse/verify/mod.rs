use std::iter;
use std::process::id;
use cranelift::codegen::verifier::verify_context;
use crate::log_error;
use crate::parse::verify::bytecode::{BytecodeBuilder, VerifiedFunction, VirtualInstruction};
use crate::parse::ast::{GlobalStatement, AST};
use crate::parse::verify::context::{FnMap, TypeMap, VerifyContext};
use crate::parse::verify::typeless_declarations::gen_declarations;
use crate::parse::verify::verify_expression::verify_expression;
use crate::parse::verify::verify_type::{verify_fn_prototype, verify_type};
use crate::util::ScopedMap;

pub mod context;
pub mod bytecode;
pub mod verify_type;

mod typeless_declarations;
mod verify_expression;

#[derive(Debug)]
pub struct VerifiedAST {
    pub fn_map: FnMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<VerifiedFunction>
}

pub fn verify_ast(ast: AST) -> Option<VerifiedAST> {
    let (mut type_map, mut fn_map) = gen_declarations(&ast)?;
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

        for (i, arg) in prototype.args.iter().enumerate() {
            let value = builder.add_instruction(
                &verify_context,
                VirtualInstruction::FunctionParameter {
                    param_index: i as u32,
                    name: arg.name.clone()
                },
                arg.type_.clone()
            )?;

            verify_context.insert_variable(arg.name.clone(), value);
        }

        for stmt in body.iter() {
            verify_expression(&mut verify_context, &mut builder, stmt)?;
        }

        builder.finish_function();
    }

    builder.finish(verify_context.fn_map, verify_context.type_map)
}