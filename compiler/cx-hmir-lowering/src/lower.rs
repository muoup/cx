pub(crate) mod constructor;
pub(crate) mod function;
pub(crate) mod global;
pub(crate) mod types;

use crate::{
    env::{
        HMIREnvironment,
        generation::{Generated, Generation, Instance},
    },
    eval::{self, aggregate::variant},
    lower::types::lower_signature,
    symbol,
};
use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunctionStage, HMIRUnit, ty::HMIRTypeID,
};
use cx_log::CXResult;
use cx_mir::{MIRFnPrototype, MIRFunctionID};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let mut defs = unit.defs().map(|(id, _)| *id).collect::<Vec<_>>();
    defs.sort();

    for id in defs {
        symbol::validate(env, id)?;
    }

    while let Some(id) = env.generation_mut().next() {
        match id {
            Generation::Definition(id) => generate(id, env, None)?,
            Generation::Instance(instance) => match *instance.source() {
                Generated::Template { def, .. } => generate(def, env, Some(&instance))?,
                Generated::Constructor { ty, variant } => {
                    let function = constructor::lower(env, ty, variant, instance.function())?;
                    env.module_mut()
                        .define_function(instance.function(), function);
                }
            },
        }
    }

    Ok(())
}

// The function generated for 'source', declared and queued for generation the first time it is asked for.
// 'signature' is that of the source; the parameters it has bound are not parameters of the generated function
pub(crate) fn declare(
    env: &mut HMIREnvironment,
    source: Generated,
    signature: &HMIRFnSignature<HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    if let Some(function) = env.generation().generated(&source) {
        return Ok(function);
    }
    let count = env.generation().generated_count();
    let (name, original, bindings) = match &source {
        Generated::Template { def, bindings } => {
            let original = symbol::link_name(env, *def, None)?.expect("a template names a function");
            let name = format!("{original}.__cx_specialize.{count}");
            (name, Some(original), bindings.as_slice())
        }
        Generated::Constructor { ty, variant } => {
            let name = variant::field(env, *ty, *variant).name().expect("a variant is named");
            (format!("{name}.__cx_variant.{count}"), None, [].as_slice())
        }
    };
    let bound = |index| bindings.get(index).is_some_and(Option::is_some);
    let unbound = eval::types::residual(signature, bound);
    let prototype = MIRFnPrototype::new(
        lower_signature(env, &unbound, span)?,
        LinkageMode::Static,
        CXIdent::new(name),
        original,
    );
    let function = env.module_mut().declare_function(prototype);
    env.generation_mut()
        .generate(Instance::new(source, function, signature.clone()));
    Ok(function)
}

fn generate(
    id: HMIRDefID,
    env: &mut HMIREnvironment,
    specialization: Option<&Instance>,
) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let source = unit.resolve_def(id).expect("symbol exists");
    let selected = symbol::select(source, None)?;
    let kind = source.declarations()[selected].kind();

    match kind {
        Some(HMIRDefKind::Function(function)) => {
            if let Some(specialization) = specialization {
                return function::lower(env, id, function, specialization.signature(), Some(specialization));
            }
            if function.signature().stage() != HMIRFunctionStage::Runtime
                || symbol::needs_specialization(function)
                || function.def().is_none()
            {
                return Ok(());
            }
            
            let signature = lower_signature(env, id, None)?;
            function::lower(env, id, function, &signature, None)
        }
        Some(HMIRDefKind::Global(_)) => global::lower(env, id),
        Some(HMIRDefKind::ComptimeGlobal(_)) | None => Ok(()),
    }
}
