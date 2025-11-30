use cx_bytecode_data::MIRUnit;
use cx_typechecker_data::ast::TCAST;

pub mod builder;

pub(crate) mod mir_lowering;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_mir(_ast: TCAST) -> Option<MIRUnit> {
    // let mut builder = MIRBuilder::new(&ast);

    todo!();
    
    // for global_var in ast.global_variables.iter() {
    //     generate_global_variable(&mut builder, global_var);
    // }

    // for fn_def in ast.function_defs.iter() {
    //     generate_function(&mut builder, &fn_def.prototype, &fn_def.body)?;
    // }

    // builder.finish()
}
