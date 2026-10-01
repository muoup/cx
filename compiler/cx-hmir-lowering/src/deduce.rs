use std::rc::Rc;

use cx_hmir::{
    HMIRConstant, HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRNativeOp, HMIRTypeDesc,
    HMIRTypeOp,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    program::{DefKey, Program, def_body},
    staging_error,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

impl Program<'_> {
    // Leading comptime type parameters; the explicit template arguments of a call fill them first
    pub(crate) fn template_params(&self, def: DefKey) -> Vec<HMIRLocalID> {
        let unit = self.unit(def.unit());
        let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
            return Vec::new();
        };
        let body = function.body();
        function
            .signature()
            .params()
            .iter()
            .take_while(|param| {
                let local = body.local(**param);
                local.is_comptime()
                    && matches!(
                        body.expr(local.ty()).kind(),
                        HMIRExprKind::Constant(HMIRConstant::Type(ty))
                            if matches!(unit.types().get(*ty), HMIRTypeDesc::Type)
                    )
            })
            .copied()
            .collect()
    }

    pub(crate) fn param_count(&self, def: DefKey) -> usize {
        let unit = self.unit(def.unit());
        match unit.def(def.def()).kind() {
            HMIRDefKind::Function(function) => function.signature().params().len(),
            _ => 0,
        }
    }

    // Completes the template prefix of 'def' from explicit arguments ('None' for '_') and the
    // types of the arguments that follow it
    pub(crate) fn deduce_template(
        &mut self,
        def: DefKey,
        explicit: Vec<Option<StaticValue>>,
        actual: &[Option<TypeID>],
        span: &TokenRange,
    ) -> CXResult<Vec<StaticValue>> {
        let unit = self.unit(def.unit());
        let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
            return Err(staging_error(
                span,
                "template arguments for a non-function".into(),
            ));
        };
        let template = self.template_params(def);
        let mut frame = EvalFrame::new(unit.clone(), def, Rc::new((def, Vec::new())));
        for (param, value) in template.iter().zip(explicit) {
            if let Some(value) = value {
                frame.bind(*param, value);
            }
        }
        let rest = &function.signature().params()[template.len()..];
        for (param, actual) in rest.iter().zip(actual) {
            if let Some(actual) = actual {
                let declared = function.body().local(*param).ty();
                self.unify(&mut frame, declared, *actual, &template);
            }
        }
        template
            .iter()
            .map(|param| {
                frame.local(*param).cloned().ok_or_else(|| {
                    let name = function
                        .body()
                        .local(*param)
                        .name()
                        .map(|name| name.as_string())
                        .unwrap_or_else(|| param.to_string());
                    staging_error(
                        span,
                        format!(
                            "cannot deduce comptime argument '{name}' of '{}'",
                            unit.def(def.def()).name()
                        ),
                    )
                })
            })
            .collect()
    }

    pub(crate) fn deduce_static(
        &mut self,
        def: DefKey,
        args: Vec<StaticValue>,
        span: &TokenRange,
    ) -> CXResult<Vec<StaticValue>> {
        let template = self.template_params(def).len();
        let rest = self.param_count(def) - template;
        if args.len() < rest {
            return Err(staging_error(span, "too few arguments".into()));
        }
        let explicit_count = (args.len() - rest).min(template);
        let actual = args[explicit_count..]
            .iter()
            .map(|arg| self.static_type(arg, span).ok())
            .collect::<Vec<_>>();
        let explicit = args[..explicit_count].iter().cloned().map(Some).collect();
        let mut values = self.deduce_template(def, explicit, &actual, span)?;
        values.extend(args.into_iter().skip(explicit_count));
        Ok(values)
    }

    fn unify(
        &mut self,
        frame: &mut EvalFrame,
        expr: HMIRExprID,
        actual: TypeID,
        template: &[HMIRLocalID],
    ) {
        let unit = frame.unit().clone();
        let body = def_body(unit.def(frame.def().def())).expect("deduced def has a body");
        let actual_kind = self.types().kind(actual).clone();
        match body.expr(expr).kind() {
            HMIRExprKind::Local(local) => {
                if template.contains(local) && frame.local(*local).is_none() {
                    frame.bind(*local, StaticValue::Type(actual));
                }
            }
            HMIRExprKind::Comptime(inner) => self.unify(frame, *inner, actual, template),
            HMIRExprKind::Native(HMIRNativeOp::Type(op)) => match (op, actual_kind) {
                (HMIRTypeOp::Pointer(inner), TypeKind::Pointer(actual))
                | (
                    HMIRTypeOp::Pointer(inner),
                    TypeKind::Array {
                        element: actual, ..
                    },
                )
                | (HMIRTypeOp::Reference(inner), TypeKind::Reference(actual))
                | (
                    HMIRTypeOp::Array { element: inner, .. },
                    TypeKind::Array {
                        element: actual, ..
                    },
                )
                | (HMIRTypeOp::Array { element: inner, .. }, TypeKind::Pointer(actual)) => {
                    self.unify(frame, *inner, actual, template)
                }
                (HMIRTypeOp::Pointer(inner), TypeKind::Str) => {
                    let char = self.types_mut().int(cx_hmir::HMIRIntWidth::I8, true);
                    self.unify(frame, *inner, char, template)
                }
                (HMIRTypeOp::Expr { result: inner, .. }, TypeKind::Expr { result, .. }) => {
                    self.unify(frame, *inner, result, template)
                }
                (HMIRTypeOp::Reference(inner), _) | (HMIRTypeOp::Expr { result: inner, .. }, _) => {
                    self.unify(frame, *inner, actual, template)
                }
                (HMIRTypeOp::Function { params, ret, .. }, kind) => {
                    let function = match kind {
                        TypeKind::Function(function) => Some(function),
                        TypeKind::Pointer(inner) => match self.types().kind(inner) {
                            TypeKind::Function(function) => Some(function.clone()),
                            _ => None,
                        },
                        _ => None,
                    };
                    if let Some(function) = function {
                        for (param, actual) in params.iter().zip(function.params()) {
                            self.unify(frame, *param, *actual, template);
                        }
                        self.unify(frame, *ret, function.ret(), template);
                    }
                }
                _ => {}
            },
            HMIRExprKind::Call { callee, args } => {
                let Some(nominal) = self.types().nominal_of(actual) else {
                    return;
                };
                let key = nominal.key().clone();
                let Ok(StaticValue::Function { def, args: bound }) = self.eval(frame, *callee)
                else {
                    return;
                };
                if def != key.owner() || bound.len() > key.args().len() {
                    return;
                }
                for (arg, value) in args.iter().zip(&key.args()[bound.len()..]) {
                    match value {
                        StaticValue::Type(ty) => self.unify(frame, *arg, *ty, template),
                        value => {
                            if let HMIRExprKind::Local(local) = body.expr(*arg).kind()
                                && template.contains(local)
                                && frame.local(*local).is_none()
                            {
                                frame.bind(*local, value.clone());
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}
