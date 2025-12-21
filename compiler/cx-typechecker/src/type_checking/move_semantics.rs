use crate::{
    builder::{Lifetime, MIRBuilder},
    environment::TypeEnvironment,
};
use cx_typechecker_data::mir::{
    expression::{MIRInstruction, MIRRegister},
    types::MIRType,
};

pub fn acknowledge_declared_object(
    env: &mut TypeEnvironment,
    name: String,
    register: MIRRegister,
    _type: MIRType,
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

pub fn invoke_scope_destructors(builder: &mut MIRBuilder) {
    let lifetimes: Vec<_> = builder.lifetime_stack_ref().last()
        .expect("No lifetime scope found")
        .clone();

    for lifetime in lifetimes.into_iter() {
        acknowledge_destructed_object(builder, lifetime);
    }
}

pub fn invoke_remaining_destructions(builder: &mut MIRBuilder, exclude: Option<&MIRRegister>) {
    let scopes = builder
        .lifetime_stack_ref()
        .iter()
        .rev()
        .flat_map(|scope| scope.iter().rev().cloned())
        .collect::<Vec<Lifetime>>();

    for lifetime in scopes.into_iter() {
        if let Some(exclude_reg) = exclude {
            if &lifetime.region == exclude_reg {
                continue;
            }
        }
        
        acknowledge_destructed_object(builder, lifetime);
    }
}
