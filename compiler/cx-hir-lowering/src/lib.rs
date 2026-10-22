mod body;
mod builder;
mod def;
mod expr;
mod external;
mod log;
mod plan;
mod resolve;
mod symbols;
mod ty;

use cx_hir::{ast::HIR, registry::GlobalSymbolRegistry};
use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit};
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_target::ArchitectureConfig;

use crate::{
    body::BodyLowering, builder::HMIRUnitBuilder, def::lower_def, plan::plan_defs,
    resolve::Resolver, symbols::symbol_table,
};

pub use external::generate_external_hmir;

pub fn generate_hmir(
    hir: &HIR,
    namespace: NamespacePath,
    registry: &GlobalSymbolRegistry,
    architecture: ArchitectureConfig,
) -> CXResult<HMIRUnit> {
    let plans = plan_defs(hir, &namespace);

    let mut resolver = Resolver::new(registry, architecture, plans.len());
    for (index, plan) in plans.iter().enumerate() {
        resolver.declare_def(plan.name().clone(), HMIRDefID::new(index));
    }

    let symbols = symbol_table(hir, &namespace, registry, &plans);
    let mut unit = HMIRUnitBuilder::new(namespace);
    for (index, plan) in plans.iter().enumerate() {
        let namespace = plan.namespace().clone();
        let context = BodyLowering::new(&resolver, unit.types_mut(), namespace, false)
            .owned_by(HMIRDefID::new(index));
        let kind = lower_def(context, plan)?;

        unit.push_def(HMIRDef::new(plan.name().clone(), plan.span().clone(), kind));
    }
    for def in resolver.take_statics() {
        unit.push_def(def);
    }

    Ok(unit.finish(symbols))
}
