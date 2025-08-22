use crate::builder::BytecodeBuilder;
use crate::deconstructor::generate_deconstructor;
use crate::global_stmts::{generate_destructor, generate_function};
use cx_data_ast::parse::ast::{CXGlobalStmt, CXAST};
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_bytecode::ProgramBytecode;

pub mod instruction_gen;
pub mod builder;
mod implicit_cast;
mod cx_maps;
mod aux_routines;
mod deconstructor;
mod global_stmts;

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
        match stmt {
            CXGlobalStmt::FunctionDefinition { prototype, body } =>
                generate_function(&mut builder, prototype, body)?,

            CXGlobalStmt::DestructorDefinition { type_name, body } =>
                generate_destructor(&mut builder, type_name, body)?,

            CXGlobalStmt::GlobalVariable { .. } => todo!("Global variables are not implemented yet"),
        
            _ => continue
        }
    }

    builder.finish()
}