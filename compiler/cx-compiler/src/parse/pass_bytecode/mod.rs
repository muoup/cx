use crate::parse::pass_bytecode::builder::{BytecodeBuilder, BytecodeFunction, BytecodeFunctionPrototype, BytecodeParameter, VirtualInstruction};
use crate::parse::pass_bytecode::instruction_gen::generate_instruction;
use crate::parse::pass_bytecode::typing::get_type_size;
use crate::parse::pass_ast::{CXGlobalStmt, FunctionMap, TypeMap, CXAST};
use crate::parse::value_type::CXValType;
use std::fmt::{Display, Formatter};
use crate::{log_error, type_matches};

pub mod builder;
pub mod typing;
mod name_mangling;
mod instruction_gen;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: FunctionMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<BytecodeFunction>,
}

impl Display for ProgramBytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for func in self.fn_defs.iter() {
            writeln!(f, "{:#?}", func)?;
        }

        Ok(())
    }
}

pub fn gen_bytecode(ast: CXAST) -> Option<ProgramBytecode> {
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
                        val: 0
                    },
                    CXValType::Integer { bytes: 4, signed: true }
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