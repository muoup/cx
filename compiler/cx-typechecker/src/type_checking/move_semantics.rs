use crate::{
    builder::{Lifetime, MIRBuilder},
    environment::TypeEnvironment,
};
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRRegister},
    types::CXType,
};

pub fn acknowledge_declared_object(
    env: &mut TypeEnvironment,
    name: String,
    register: MIRRegister,
    _type: CXType,
) {
    env.builder.add_lifetime(Lifetime {
        name: name.clone(),
        region: register.clone(),
        _type: _type.clone(),
    });
    env.builder.add_instruction(MIRInstruction::LifetimeStart {
        name,
        region: register,
        _type,
    });
}

pub fn acknowledge_destructed_object(builder: &mut MIRBuilder, lifetime: Lifetime) {
    builder.add_instruction(MIRInstruction::LifetimeEnd {
        name: lifetime.name,
        region: lifetime.region,
        _type: lifetime._type,
    });
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
