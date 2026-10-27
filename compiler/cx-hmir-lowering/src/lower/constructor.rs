use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRInstructionKind,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
        generation::Constructor,
    },
    eval::{aggregate::variant, types},
    lower::{mir, types::lower_signature},
    value::constant::HMIRFunctionRef,
};

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    let function = HMIRFunctionRef::Constructor { ty, variant: index };
    if let Some(id) = env.generation().generated(function, &[]) {
        return Ok(id);
    }
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        unreachable!("a constructor builds a tagged union")
    };
    let name = fields[index].name().expect("a variant is named");
    let name = CXIdent::new(format!(
        "{name}.__cx_variant.{}",
        env.generation().generated_count()
    ));
    let signature = lower_signature(env, &variant::signature(env, ty, index), span)?;
    let prototype = MIRFnPrototype::new(signature, LinkageMode::Static, name, None);
    let id = env.module_mut().declare_function(prototype);
    env.generation_mut().construct(Constructor::new(ty, index, id));
    Ok(id)
}

pub(crate) fn lower(env: &mut HMIREnvironment, constructor: &Constructor) -> CXResult<MIRFunction> {
    let (ty, index) = (constructor.ty(), constructor.variant());
    let span = TokenRange::internal();
    let prototype = env
        .module()
        .function(constructor.function())
        .prototype()
        .clone();
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, None, runtime);

    let scope = emit::open_scope(&mut context, &span);
    let payload = match variant::payload(context.env(), ty, index) {
        Some(payload) => {
            let reference = types::reference(context.env_mut(), payload, None, &span);
            let param = mir::parameter(&mut context, payload, None, &span)?.with_ty(reference);
            let moved = emit::lift(&mut context, &param, None, &span)?;
            emit::invalidate(&mut context, &param, MIRInvalidationKind::Move, &span);
            Some(moved)
        }
        None => None,
    };
    let value = variant::construct(&mut context, ty, index, payload, &span)?;
    let value = Some(mir::operand(&mut context, &value, &span)?);
    mir::drop_scope(&mut context, scope, &span);
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Return { value }, span.clone());
    emit::close_scope(&mut context, &span);

    Ok(context.into_runtime().finish())
}
