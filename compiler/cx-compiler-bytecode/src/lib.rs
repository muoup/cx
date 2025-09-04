use crate::builder::BytecodeBuilder;
use crate::global_stmts::generate_function;
use cx_data_bytecode::ProgramBytecode;
use cx_data_typechecker::ast::{TCStructureData, TCAST};
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

pub fn generate_bytecode(ast: TCAST) -> Option<ProgramBytecode> {
    let mut builder = BytecodeBuilder::new(&ast);

    for _type in ast.destructors_required.iter() {
        let prototype = deconstructor_prototype(_type)?;
        builder.fn_map.insert(prototype.name.clone(), prototype);
    }

    for _type in ast.destructors_required.iter() {
        let Some(_) = generate_deconstructor(&mut builder, _type) else {
            bytecode_error_log!(builder, "Failed to generate deconstructor for type {}", _type);
        };
    }

    for fn_def in ast.function_defs.iter() {
        generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    }

    builder.finish()
}