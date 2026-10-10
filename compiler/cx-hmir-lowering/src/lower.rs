use crate::{
    env::{HMIREnvironment, lowering::FnLoweringContext},
    lower::{
        control::lower_return_value,
        dispatch::lower_expression,
        emit::emit_return,
        shape::{TypeShape, is_nodrop, shape},
    },
};
use cx_hmir::{
    HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit,
    body::HMIRLocalID,
    expr::{HMIRExprKind, constant::HMIRConstant},
    ty::HMIRTypeID,
    unit::function::HMIRFnDefinition,
};
use cx_log::CXResult;
use cx_mir::{
    MIRBindable, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRInstructionKind, MIRPlaceDecl,
    MIRPlaceID, MIRRegister,
};
use cx_tokens::TokenRange;

mod binding;
mod coerce;
mod constant;
mod control;
mod dispatch;
mod emit;
mod op;
mod shape;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    value: HMIRValueKind,
}

#[derive(Debug, Clone)]
pub enum HMIRValueKind {
    Constant(HMIRConstant),
    Register(MIRRegister),
    Place(MIRPlaceID),
}

impl HMIRValue {
    pub fn new(ty: HMIRTypeID, value: HMIRValueKind) -> Self {
        Self { ty, value }
    }

    pub fn constant(ty: HMIRTypeID, constant: HMIRConstant) -> Self {
        Self::new(ty, HMIRValueKind::Constant(constant))
    }

    pub fn register(ty: HMIRTypeID, register: MIRRegister) -> Self {
        Self::new(ty, HMIRValueKind::Register(register))
    }

    pub fn place(ty: HMIRTypeID, place: MIRPlaceID) -> Self {
        Self::new(ty, HMIRValueKind::Place(place))
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn kind(&self) -> &HMIRValueKind {
        &self.value
    }

    pub fn with_ty(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }
}

const RETURN_SLOT: HMIRLocalID = HMIRLocalID::new(usize::MAX);

pub(crate) fn return_type(cx: &FnLoweringContext) -> HMIRTypeID {
    cx.get_local(&RETURN_SLOT)
        .expect("return slot is bound before the body is lowered")
        .ty()
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
    let body = function.body();
    let signature = function.signature();
    let root = body.expr(def.root());
    let span = root.span();

    let mut params = Vec::new();
    let mut mir_params = Vec::new();
    for (param, local) in signature.params().iter().zip(def.params()) {
        let ty = env.eval_type(body, param.ty())?;
        let mir = env.lower_type(ty, body.expr(param.ty()).span())?;
        let name = body.local(*local).name().cloned();
        let nodrop = is_nodrop(env, ty);

        mir_params.push(MIRFnParam::new(name, mir, nodrop));
        params.push((ty, mir, nodrop));
    }

    let return_ty = env.eval_type(body, signature.return_type())?;
    let return_mir = env.lower_type(return_ty, body.expr(signature.return_type()).span())?;

    let prototype = MIRFnPrototype::new(
        MIRFnSignature::new(
            mir_params,
            return_mir,
            signature.is_variadic(),
            signature.contract().is_safe(),
        ),
        function.linkage(),
        function.symbol_name().clone(),
        None,
    );
    let id = env.module_mut().declare_function(prototype);

    let mut cx = FnLoweringContext::new(env, id);
    cx.insert_local(
        RETURN_SLOT,
        HMIRValue::constant(return_ty, HMIRConstant::Unit),
    );

    let scope = cx.builder().push_scope(span);
    for (local, (ty, mir, nodrop)) in def.params().iter().zip(params) {
        let name = body.local(*local).name().cloned();
        let place = cx
            .builder()
            .allocate_place(MIRPlaceDecl::new(mir, name, scope, nodrop, false));

        cx.builder().add_parameter(place);
        cx.builder().emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
        cx.insert_local(*local, HMIRValue::place(ty, place));
    }

    let value = lower_expression(&mut cx, body, root)?;
    if !cx.builder().terminated() {
        match root.kind() {
            HMIRExprKind::Block { .. } => lower_fallthrough(&mut cx, return_ty, span)?,
            _ => lower_return_value(&mut cx, Some(value), span)?,
        }
    }

    cx.builder().pop_scope();
    let mir_body = cx.finish();
    env.module_mut().define_function(id, mir_body);

    Ok(())
}

fn lower_fallthrough(
    cx: &mut FnLoweringContext,
    return_ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<()> {
    let is_main = {
        let function = cx.builder().function();
        cx.env().module().function(function).prototype().symbol_name.as_str() == "main"
    };

    match shape(cx, return_ty) {
        TypeShape::Void => emit_return(cx, None, span),
        TypeShape::Int(_) if is_main => {
            let zero = HMIRValue::constant(
                return_ty,
                HMIRConstant::Int {
                    value: 0,
                    ty: return_ty,
                },
            );
            let zero = cx.materialize(&zero, span)?;
            emit_return(cx, Some(zero), span);
        }
        _ => cx.builder().emit(MIRInstructionKind::Unreachable, span),
    }

    Ok(())
}

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<()> {
    todo!()
}
