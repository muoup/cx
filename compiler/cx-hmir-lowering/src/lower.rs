pub(crate) mod constructor;
pub(crate) mod declaration;
pub(crate) mod function;
pub(crate) mod global;
pub(crate) mod types;

use crate::{
    env::{HMIREnvironment, generation::Generation},
    eval::aggregate::variant,
    lower::types::lower_signature,
    value::HMIRValue,
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
    declaration::resolve(env, unit)?;

    while let Some(generation) = env.generation_mut().next() {
        let function = env.generation().function(&generation);
        match (generation, function) {
            (Generation::Definition(def), _) => generate(def, env, None)?,
            (Generation::Template { def, comptime_bindings }, Some(function)) => {
                generate(def, env, Some((function, &comptime_bindings)))?
            }
            (Generation::Constructor { ty, variant }, Some(function)) => {
                let lowered = constructor::lower(env, ty, variant, function)?;
                env.module_mut().define_function(function, lowered);
            }
            (generation, None) => unreachable!("{generation:?} was requested undeclared"),
        }
    }

    Ok(())
}

// The function generated for 'generation', which no definition declares: it is declared and requested the first
// time it is asked for. 'signature' is that of the generated function
pub(crate) fn declare(
    env: &mut HMIREnvironment,
    generation: Generation,
    signature: &HMIRFnSignature<HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    if let Some(function) = env.generation().function(&generation) {
        return Ok(function);
    }
    let count = env.generation().requested_count();
    let (name, original) = match &generation {
        Generation::Template { def, .. } => {
            let source = env.symbols().resolve_definition(*def);
            let original = declaration::link_name(source).expect("a template names a function");
            (format!("{original}.__cx_specialize.{count}"), Some(original))
        }
        Generation::Constructor { ty, variant } => {
            let name = variant::field(env, *ty, *variant).name().expect("a variant is named");
            (format!("{name}.__cx_variant.{count}"), None)
        }
        Generation::Definition(_) => unreachable!("a definition declares its own function"),
    };
    let prototype = MIRFnPrototype::new(
        lower_signature(env, signature, span)?,
        LinkageMode::Static,
        CXIdent::new(name),
        original,
    );
    let function = env.module_mut().declare_function(prototype);
    env.generation_mut().request(generation, Some(function));
    Ok(function)
}

// 'template' is the function declared for a template of the definition, and the arguments of its comptime
// parameters
fn generate(
    id: HMIRDefID,
    env: &mut HMIREnvironment,
    template: Option<(MIRFunctionID, &[HMIRValue])>,
) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(id.brand())
        .expect("symbol owner is available");
    let source = unit.resolve_def(id).expect("symbol exists");
    match declaration::canonical(source).kind() {
        Some(HMIRDefKind::Function(function)) => {
            let standalone = function.signature().stage() == HMIRFunctionStage::Runtime
                && !declaration::needs_specialization(function)
                && function.def().is_some();
            if template.is_none() && !standalone {
                return Ok(());
            }
            function::lower(env, id, function, template)
        }
        Some(HMIRDefKind::Global(_)) => global::lower(env, id),
        Some(HMIRDefKind::ComptimeGlobal(_)) | None => Ok(()),
    }
}
