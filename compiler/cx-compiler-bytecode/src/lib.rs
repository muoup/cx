use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::value_type::{get_type_size, CXValType};
use cx_data_bytecode::builder::{BytecodeBuilder, BytecodeFunctionPrototype, BytecodeParameter, VirtualInstruction};
use cx_data_bytecode::ProgramBytecode;
use crate::instruction_gen::generate_instruction;

pub mod instruction_gen;
mod implicit_cast;


pub fn generate_bytecode(ast: CXAST) -> Option<ProgramBytecode> {
    let mut builder = BytecodeBuilder::new(ast.type_map, ast.function_map);

    for stmt in ast.global_stmts.iter() {
        let CXGlobalStmt::FunctionDefinition {
            prototype, body
        } = stmt else {
            continue;
        };

        builder.symbol_table.push_scope();
        builder.new_function(
            BytecodeFunctionPrototype {
                name: prototype.name.to_owned(),
                return_type: prototype.return_type.clone(),
                args: prototype.parameters.iter()
                    .map(|param| BytecodeParameter {
                        name: if let Some(name) = &param.name {
                            Some(name.to_string())
                        } else {
                            None
                        },
                        type_: param.type_.clone()
                    })
                    .collect()
            }
        );

        for (i, arg) in prototype.parameters.iter().enumerate() {
            let memory = builder.add_instruction(
                VirtualInstruction::Allocate {
                    size: get_type_size(&builder.type_map, &arg.type_)?
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
                    type_: arg.type_.clone()
                },
                CXValType::Unit
            )?;

            if let Some(name) = &arg.name {
                builder.symbol_table.insert(name.to_owned(), memory);
            }
        }

        generate_instruction(&mut builder, body)?;

        if !matches!(builder.last_instruction(), Some(instr) if matches!(instr.instruction, VirtualInstruction::Return { .. })) {
            if prototype.name.as_str() == "main" {
                let zero_literal = builder.add_instruction(
                    VirtualInstruction::Literal {
                        val: 0,
                    },
                    CXValType::Integer { bytes: 3, signed: true }
                )?;
                builder.add_instruction(
                    VirtualInstruction::Return {
                        value: Some(zero_literal)
                    },
                    CXValType::Unit
                )?;
            }

            builder.add_instruction(
                VirtualInstruction::Return {
                    value: None
                },
                CXValType::Unit
            )?;
        }

        builder.symbol_table.pop_scope();
        builder.finish_function();
    }

    builder.finish()
}