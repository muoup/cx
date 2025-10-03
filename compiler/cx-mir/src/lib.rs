use crate::builder::MIRBuilder;
use crate::global_stmts::{generate_function, generate_global_variable};
use cx_mir_data::ProgramMIR;
use cx_typechecker_data::ast::TCAST;
use cx_util::bytecode_error_log;
use crate::deconstructor::{deconstructor_prototype, generate_deconstructor};

pub mod instruction_gen;
pub mod builder;
mod implicit_cast;
mod cx_maps;
mod aux_routines;
mod deconstructor;
mod global_stmts;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_bytecode(ast: TCAST) -> Option<ProgramMIR> {
    let mut builder = MIRBuilder::new(&ast);

    for _type in ast.destructors_required.iter() {
        let prototype = deconstructor_prototype(_type)?;
        builder.fn_map.insert(prototype.name.clone(), prototype);
    }

    for _type in ast.destructors_required.iter() {
        let Some(_) = generate_deconstructor(&mut builder, _type) else {
            bytecode_error_log!(builder, "Failed to generate deconstructor for type {}", _type);
        };
    }

    for global_var in ast.global_variables.iter() {
        generate_global_variable(&mut builder, global_var);
    }

    for fn_def in ast.function_defs.iter() {
        generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    }

    builder.finish()
}