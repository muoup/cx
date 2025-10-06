use crate::builder::MIRBuilder;
use crate::deconstructor::{deconstructor_prototype, generate_deconstructor};
use crate::global_stmts::{generate_function, generate_global_variable};
use cx_mir_data::ProgramMIR;
use cx_typechecker_data::ast::TCAST;
use cx_util::bytecode_error_log;

mod aux_routines;
pub mod builder;
mod cx_maps;
mod deconstructor;
mod global_stmts;
mod implicit_cast;
pub mod instruction_gen;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_bytecode(ast: TCAST) -> Option<ProgramMIR> {
    let mut builder = MIRBuilder::new(&ast);

    for global_var in ast.global_variables.iter() {
        generate_global_variable(&mut builder, global_var);
    }

    for fn_def in ast.function_defs.iter() {
        generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    }

    builder.finish()
}
