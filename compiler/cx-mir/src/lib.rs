use cx_mir_data::program::MIRUnit;
use cx_typechecker_data::ast::TCAST;

mod builder;
mod lowering;

pub fn lower_to_mir(tc_ast: &TCAST) -> MIRUnit {
    lowering::lower_tcast_to_mir(tc_ast)
}
