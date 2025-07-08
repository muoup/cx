use crate::aux_routines::allocate_variable;
use crate::builder::BytecodeBuilder;
use crate::instruction_gen::generate_instruction;
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_ast::parse::value_type::CXType;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_bytecode::{ProgramBytecode, VirtualInstruction};
use crate::cx_maps::convert_cx_prototype;
use crate::deconstructor::generate_deconstructor;

pub mod instruction_gen;
mod builder;
mod implicit_cast;
mod cx_maps;
mod aux_routines;
mod deconstructor;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_bytecode(ast: CXAST, type_check_data: TypeCheckData) -> Option<ProgramBytecode> {
    // this shouldn't be necessary, but this is again because of the coupling of type information
    // and the bytecode builder
    // (TODO)
    let deconstructors = type_check_data.deconstructor_data.clone();
    
    let mut builder = BytecodeBuilder::new(ast.type_map, ast.function_map, type_check_data);
    
    for deconstructor in deconstructors.iter() {
        generate_deconstructor(&mut builder, deconstructor);
    }
    
    for stmt in ast.global_stmts.iter() {
        let CXGlobalStmt::FunctionDefinition {
            prototype, body
        } = stmt else {
            continue;
        };

        builder.symbol_table.push_scope();

        let bc_prototype = builder.convert_cx_prototype(prototype).unwrap();
        builder.new_function(bc_prototype);

        for (i, arg) in prototype.params.iter().enumerate() {
            let memory = allocate_variable(
                &arg.name.as_ref().map(|n| n.as_string()).unwrap_or_else(|| format!("_fn_arg_{i}")),
                &mut builder,
                &arg.type_
            )?;

            let value = builder.add_instruction(
                VirtualInstruction::FunctionParameter {
                    param_index: i as u32,
                },
                arg.type_.clone()
            )?;
            
            let arg_type = builder.convert_cx_type(&arg.type_)?;

            builder.add_instruction(
                VirtualInstruction::Store {
                    value,
                    memory,
                    type_: arg_type
                },
                CXType::unit()
            )?;

            if let Some(name) = &arg.name {
                builder.symbol_table.insert(name.as_string(), memory);
            }
        }

        let Some(_) = generate_instruction(&mut builder, body) else {
            panic!("Failed to generate body for function: {}", prototype.name);
        };
        
        builder.symbol_table.pop_scope();
        builder.finish_function(false);
    }

    builder.finish()
}