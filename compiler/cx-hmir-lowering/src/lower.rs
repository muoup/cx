use crate::{
    env::{HMIREnvironment, lowering::FnLoweringContext},
    lower::dispatch::lower_expression,
};
use cx_hmir::{
    HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit,
    expr::constant::HMIRConstant, ty::HMIRTypeID, unit::function::HMIRFnDefinition,
};
use cx_log::CXResult;
use cx_mir::{MIRPlaceID, MIRRegister};

mod constant;
mod dispatch;
mod types;

pub struct HMIRValue {
    ty: HMIRTypeID,
    value: HMIRValueKind,
}

pub enum HMIRValueKind {
    Constant(HMIRConstant),
    Register(MIRRegister),
    Place(MIRPlaceID),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, value: HMIRValueKind) -> Self {
        Self { ty, value }
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.value
    }
}

pub fn lower_unit(unit: &HMIRUnit, env: &mut HMIREnvironment) -> CXResult<()> {
    fn is_root(signature: &HMIRFunction) -> bool {
        signature.signature().stage() == HMIRFunctionStage::Runtime
            && !signature
                .signature()
                .params()
                .iter()
                .any(|param| param.comptime())
    }

    for (_, def) in unit.defs() {
        match def.kind() {
            HMIRDefKind::Function(func) => {
                let Some(def) = func.def() else {
                    continue;
                };

                if !is_root(func) {
                    continue;
                }

                lower_function(func, def, env)?;
            }

            HMIRDefKind::Global(global) => {
                lower_global(global, env)?;
            }

            _ => {}
        }
    }

    Ok(())
}

pub fn lower_function(
    function: &HMIRFunction,
    def: &HMIRFnDefinition,
    env: &mut HMIREnvironment,
) -> CXResult<()> {
    let mut lowering_env = FnLoweringContext::new(env, function);

    for param in def.params() {
        let param_ty = function.signature().params()[*param].ty();
        let param_value = lowering_env.
        lowering_env.insert_local(*param, param_value);
    }

    lower_expression(&mut lowering_env, function.body().expr(def.root()))?;

    Ok(())
}

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<()> {
    todo!()
}
