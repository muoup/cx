use crate::{
    env::{HMIREnvironment, lowering::FnLoweringContext},
    lower::{control::lower_return_value, dispatch::lower_expression},
};
use cx_hmir::{
    HMIRBody, HMIRDefKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRUnit,
    expr::{HMIRExprKind, constant::HMIRConstant},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnDefinition,
};
use cx_log::CXResult;
use cx_mir::{MIRBindable, MIRInstructionKind, MIRPlaceDecl, MIRPlaceID, MIRRegister};
use cx_tokens::TokenRange;

mod binding;
mod coerce;
pub(crate) mod constant;
mod control;
mod dispatch;
mod emit;
mod op;
mod promote;
pub(crate) mod types;

#[derive(Debug, Clone)]
pub struct HMIRValue {
    ty: HMIRTypeID,
    value: HMIRValueKind,
}

#[derive(Debug, Clone)]
pub enum HMIRValueKind {
    Constant(HMIRConstant),
    Register(MIRRegister),
    PlaceRef(MIRPlaceID),
    Indirect(MIRRegister),
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
        Self::new(ty, HMIRValueKind::PlaceRef(place))
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

pub fn lower_unit<'hmir>(
    unit: &'hmir HMIRUnit,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    fn is_root(function: &HMIRFunction) -> bool {
        function.signature().stage() == HMIRFunctionStage::Runtime
            && !function
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

pub fn lower_function<'hmir>(
    function: &'hmir HMIRFunction,
    def: &HMIRFnDefinition,
    env: &mut HMIREnvironment<'_, 'hmir>,
) -> CXResult<()> {
    let body = function.body();
    let signature = function.signature();
    let root = body.expr(def.root());
    let span = root.span();

    let mut context = FnLoweringContext::new(env, signature);
    let scope = emit::open_scope(&mut context, span.clone());

    for (index, (param, local)) in signature.params().iter().zip(def.params()).enumerate() {
        let ty = context.env_mut().eval_type(body, param.ty())?;
        let mir = &context.builder().prototype().signature.params()[index];
        let decl = MIRPlaceDecl::new(mir.ty(), mir.name().cloned(), scope, mir.nodrop(), false);
        let place = context.builder_mut().allocate_place(decl);

        context.builder_mut().add_parameter(place);
        context.builder_mut().emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span.clone(),
        );
        context.insert_local(*local, HMIRValue::place(ty, place));
    }

    let value = lower_expression(&mut context, body, root)?;
    if !emit::terminated(context.builder()) {
        match root.kind() {
            HMIRExprKind::Block { .. } => lower_fallthrough(&mut context, body, span)?,
            _ => lower_return_value(&mut context, body, Some(value), span)?,
        }
    }

    emit::close_scope(&mut context, span.clone());

    let function = context.finish();
    let id = env
        .module_mut()
        .declare_function(function.prototype().clone());
    env.module_mut().define_function(id, function);

    Ok(())
}

fn lower_fallthrough(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
) -> CXResult<()> {
    let return_type = context.signature().return_type();
    let ty = context.env_mut().eval_type(body, return_type)?;
    let is_main = context.builder().prototype().symbol_name.as_str() == "main";

    match context.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Void => lower_return_value(context, body, None, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRConstant::Int { value: 0, ty });
            lower_return_value(context, body, Some(zero), span)
        }
        _ => {
            context
                .builder_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
            Ok(())
        }
    }
}

pub fn lower_global(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<()> {
    todo!()
}
