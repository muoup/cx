mod body;
mod def;
mod expr;
mod external;
mod plan;
mod resolve;
mod ty;

use cx_hir::{ast::HIR, registry::GlobalSymbolRegistry};
use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit};
use cx_namespace::module::NamespacePath;
use cx_target::ArchitectureConfig;

use crate::def::lower_def;

use crate::{body::BodyLowering, plan::plan_defs, resolve::Resolver};

pub use external::generate_external_hmir;

pub fn generate_hmir(
    hir: &HIR,
    namespace: NamespacePath,
    registry: &GlobalSymbolRegistry,
    architecture: ArchitectureConfig,
) -> HMIRUnit {
    let plans = plan_defs(hir, &namespace);

    let mut resolver = Resolver::new(registry, architecture, plans.len());
    for (index, plan) in plans.iter().enumerate() {
        resolver.declare_def(plan.name().clone(), HMIRDefID::new(index));
    }

    let mut unit = HMIRUnit::new(namespace);
    for plan in &plans {
        let cx = BodyLowering::new(&resolver, unit.types_mut(), plan.namespace().clone(), false);
        let kind = lower_def(cx, plan);
        unit.push_def(HMIRDef::new(plan.name().clone(), plan.span().clone(), kind));
    }
    for def in resolver.take_statics() {
        unit.push_def(def);
    }
    unit
}
