use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::value_type::{get_type_size, CXType};
use cx_data_bytecode::{ProgramBytecode, VirtualInstruction};
use cx_data_bytecode::node_type_map::ExprTypeMap;
use crate::builder::BytecodeBuilder;
use crate::instruction_gen::{generate_instruction, implicit_return};

pub mod instruction_gen;
mod builder;
mod implicit_cast;
mod cx_maps;
mod aux_routines;

pub fn generate_bytecode(ast: CXAST, env_type_map: ExprTypeMap) -> Option<ProgramBytecode> {
    let mut builder = BytecodeBuilder::new(ast.type_map, ast.function_map, env_type_map);

    for stmt in ast.global_stmts.iter() {
        let CXGlobalStmt::FunctionDefinition {
            prototype, body
        } = stmt else {
            continue;
        };

        builder.symbol_table.push_scope();
        builder.new_function(prototype);

        for (i, arg) in prototype.params.iter().enumerate() {
            let memory = builder.add_instruction(
                VirtualInstruction::Allocate {
                    size: get_type_size(&builder.cx_type_map, &arg.type_)?
                },
                arg.type_.clone()
            )?;

            let value = builder.add_instruction(
                VirtualInstruction::FunctionParameter {
                    param_index: i as u32,
                },
                arg.type_.clone()
            )?;

            builder.add_instruction(
                VirtualInstruction::Store {
                    value,
                    memory,
                    type_: builder.convert_cx_type(&arg.type_)?,
                },
                CXType::unit()
            )?;

            if let Some(name) = &arg.name {
                builder.symbol_table.insert(name.to_owned(), memory);
            }
        }

        let Some(_) = generate_instruction(&mut builder, body) else {
            panic!("Failed to generate body for function: {}", prototype.name);
        };
        implicit_return(&mut builder, prototype)?;

        builder.symbol_table.pop_scope();
        builder.finish_function();
    }

    builder.finish()
}