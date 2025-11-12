use crate::builder::MIRBuilder;
use crate::global_stmts::{generate_function, generate_global_variable};
use cx_mir_data::MIRUnit;
use cx_typechecker_data::ast::TCAST;

mod aux_routines;
pub mod builder;
pub(crate) mod function_contracts;
mod cx_maps;
mod deconstructor;
mod global_stmts;
mod implicit_cast;
pub mod instruction_gen;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_mir(ast: TCAST) -> Option<MIRUnit> {
    let mut builder = MIRBuilder::new(&ast);

    for global_var in ast.global_variables.iter() {
        generate_global_variable(&mut builder, global_var);
    }

    for fn_def in ast.function_defs.iter() {
        generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    }
    
    for deconstructor in builder.defined_deconstructors.clone().into_iter() {
        deconstructor::generate_deconstructor(&mut builder, &deconstructor)
            .unwrap_or_else(|| panic!("Failed to generate deconstructor for type {deconstructor}"));
    }

    builder.finish()
}
