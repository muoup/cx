use crate::{
    builder::{Lifetime, MIRBuilder},
    environment::TypeEnvironment,
    type_checking::accumulation::TypecheckResult,
};
use cx_typechecker_data::mir::{
    expression::{MIRExpressionKind, MIRInstruction, MIRRegister},
    types::MIRType,
};
use cx_util::{CXResult, identifier::CXIdent};

pub fn acknowledge_declared_object(
    env: &mut TypeEnvironment,
    name: String,
    _type: MIRType,
) -> CXResult<TypecheckResult> {
    env.builder.add_lifetime(Lifetime {
        name: name.clone(),
        _type: _type.clone(),
    });

    Ok(TypecheckResult::standard_expr(
        MIRType::unit(),
        MIRExpressionKind::LifetimeStart {
            variable: CXIdent::from(name),
            _type,
        },
    ))
}

pub fn acknowledge_destructed_object(builder: &mut MIRBuilder, lifetime: Lifetime) -> CXResult<TypecheckResult> {
    Ok(TypecheckResult::standard_expr(
        MIRType::unit(),
        MIRExpressionKind::LifetimeEnd {
            variable: CXIdent::from(lifetime.name),
            _type: lifetime._type,
        },
    ))
}

pub fn invoke_remaining_destructions(builder: &mut MIRBuilder) {
    let scopes = builder
        .lifetime_stack_ref()
        .iter()
        .rev()
        .flat_map(|scope| scope.iter().rev().cloned())
        .collect::<Vec<Lifetime>>();

    for lifetime in scopes.into_iter() {
        acknowledge_destructed_object(builder, lifetime);
    }
}
