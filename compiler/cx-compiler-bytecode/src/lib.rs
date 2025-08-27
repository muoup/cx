use crate::builder::BytecodeBuilder;
use crate::global_stmts::generate_function;
use cx_data_bytecode::ProgramBytecode;
use cx_data_typechecker::ast::TCAST;

pub mod instruction_gen;
pub mod builder;
mod implicit_cast;
mod cx_maps;
mod aux_routines;
mod deconstructor;
mod global_stmts;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_bytecode(ast: TCAST) -> Option<ProgramBytecode> {
    // this shouldn't be necessary, but this is again because of the coupling of type information
    // and the bytecode builder
    // (TODO)
    let mut builder = BytecodeBuilder::new(ast.type_map, ast.fn_map);
    
    // for deconstructor in deconstructors.iter() {
    //     generate_deconstructor(&mut builder, deconstructor);
    // }

    for fn_def in ast.function_defs.iter() {
        generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    }

    builder.finish()
}