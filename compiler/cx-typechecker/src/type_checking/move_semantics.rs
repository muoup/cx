use crate::{
    builder::MIRBuilder,
    environment::TypeEnvironment,
    type_checking::accumulation::TypecheckResult,
};
use cx_typechecker_data::mir::types::MIRType;
use cx_util::{CXResult, identifier::CXIdent};

// TODO: Lifetime management moved to MIR→LMIR lowering
// These functions are stubbed for now

pub fn acknowledge_declared_object(
    _env: &mut TypeEnvironment,
    _name: String,
    _type: MIRType,
) -> CXResult<TypecheckResult> {
    // Lifetime tracking moved to lowering pass
    Ok(TypecheckResult::new(cx_typechecker_data::mir::expression::MIRExpression {
        kind: cx_typechecker_data::mir::expression::MIRExpressionKind::Unit,
        _type: MIRType::unit(),
    }))
}

pub fn invoke_scope_destructions(_builder: &mut MIRBuilder) {
    // Destructor logic moved to MIR→LMIR lowering
}
