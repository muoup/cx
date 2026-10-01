pub(crate) mod control;
pub(crate) mod expr;
pub(crate) mod ops;
mod type_relations;
mod types;

use std::{collections::HashMap, rc::Rc};

use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody, HMIRCoerceMode, HMIRConstant, HMIRControlOp,
    HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRLocalID, HMIRNativeOp,
    HMIROwnershipOp, HMIRPattern, HMIRTypeOp, HMIRUnit,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    function::{Expect, Stop, inspect::inspect},
    lower::{Context, Output, lower},
    program::{DefKey, Instance, Program, UnitID, def_body},
    staging_error,
    ty::{FunctionType, TypeID, TypeKind},
    value::{FrameRef, Quote, QuoteRef, StaticValue},
};

const LOOP_LIMIT: usize = 1 << 20;

// Runtime bindings visible to a comptime evaluation started from runtime code
#[derive(Debug, Clone)]
pub(crate) struct RuntimeView {
    origin: Option<FrameRef>,
    types: HashMap<HMIRLocalID, TypeID>,
}

pub(crate) struct EvalFrame {
    unit: Rc<HMIRUnit>,
    unit_id: UnitID,
    def: DefKey,
    owner: Rc<Instance>,
    locals: HashMap<HMIRLocalID, StaticValue>,
    runtime: Option<RuntimeView>,
}

pub(crate) enum Flow {
    Normal(StaticValue),
    Return(StaticValue),
    Yield(StaticValue),
    Break,
    Continue,
}

pub(crate) struct Signature {
    runtime: Vec<HMIRLocalID>,
    params: Vec<(Option<CXIdent>, TypeID)>,
    ret: TypeID,
    variadic: bool,
    linkage: LinkageMode,
    link_name: CXIdent,
}

impl RuntimeView {
    pub(crate) fn new(origin: Option<FrameRef>, types: HashMap<HMIRLocalID, TypeID>) -> Self {
        Self { origin, types }
    }
}

impl EvalFrame {
    pub(crate) fn new(unit: Rc<HMIRUnit>, def: DefKey, owner: Rc<Instance>) -> Self {
        Self {
            unit,
            unit_id: def.unit(),
            def,
            owner,
            locals: HashMap::new(),
            runtime: None,
        }
    }

    pub(crate) fn with_runtime(mut self, view: RuntimeView) -> Self {
        self.runtime = Some(view);
        self
    }

    pub(crate) fn bind(&mut self, local: HMIRLocalID, value: StaticValue) {
        self.locals.insert(local, value);
    }

    pub(crate) fn unit(&self) -> &Rc<HMIRUnit> {
        &self.unit
    }

    pub(crate) fn def(&self) -> DefKey {
        self.def
    }

    pub(crate) fn owner(&self) -> &Rc<Instance> {
        &self.owner
    }

    pub(crate) fn local(&self, local: HMIRLocalID) -> Option<&StaticValue> {
        self.locals.get(&local)
    }

    pub(crate) fn locals(&self) -> &HashMap<HMIRLocalID, StaticValue> {
        &self.locals
    }

    pub(crate) fn runtime_types(&self) -> impl Iterator<Item = (HMIRLocalID, TypeID)> + '_ {
        self.runtime
            .iter()
            .flat_map(|view| view.types.iter().map(|(local, ty)| (*local, *ty)))
    }

    pub(crate) fn quote(&self, params: Vec<HMIRLocalID>, body: HMIRExprID) -> StaticValue {
        let (origin, runtime_types) = match &self.runtime {
            Some(view) => (view.origin, view.types.clone()),
            None => (None, HashMap::new()),
        };
        StaticValue::Quote(QuoteRef::new(Quote::new(
            self.unit_id,
            self.def.def(),
            (*self.owner).clone(),
            params,
            body,
            self.locals.clone(),
            runtime_types,
            origin,
        )))
    }

    pub(crate) fn body(&self) -> &HMIRBody {
        def_body(self.unit.def(self.def.def())).expect("evaluated def has a body")
    }
}

impl Signature {
    pub(crate) fn runtime(&self) -> &[HMIRLocalID] {
        &self.runtime
    }

    pub(crate) fn params(&self) -> &[(Option<CXIdent>, TypeID)] {
        &self.params
    }

    pub(crate) fn ret(&self) -> TypeID {
        self.ret
    }

    pub(crate) fn is_variadic(&self) -> bool {
        self.variadic
    }

    pub(crate) fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub(crate) fn link_name(&self) -> &CXIdent {
        &self.link_name
    }
}

impl Program<'_> {
    pub(crate) fn frame_for(&mut self, instance: &Instance) -> EvalFrame {
        let unit = self.unit(instance.0.unit());
        let mut frame = EvalFrame::new(unit.clone(), instance.0, Rc::new(instance.clone()));
        if let HMIRDefKind::Function(function) = unit.def(instance.0.def()).kind() {
            let comptime = function
                .signature()
                .params()
                .iter()
                .filter(|param| function.body().local(**param).is_comptime());
            for (param, arg) in comptime.zip(&instance.1) {
                frame.bind(*param, arg.clone());
            }
        }
        frame
    }

    pub(crate) fn signature(
        &mut self,
        instance: &Instance,
        span: &TokenRange,
    ) -> CXResult<Rc<Signature>> {
        if let Some(signature) = self.signatures_mut().get(instance) {
            return Ok(signature.clone());
        }
        let unit = self.unit(instance.0.unit());
        let def = unit.def(instance.0.def());
        let HMIRDefKind::Function(function) = def.kind() else {
            return Err(staging_error(
                span,
                format!("'{}' is not a function", def.name()),
            ));
        };
        let mut frame = self.frame_for(instance);
        let body = function.body();
        let mut runtime = Vec::new();
        let mut params = Vec::new();
        for param in function.signature().params() {
            let local = body.local(*param);
            if local.is_comptime() {
                continue;
            }
            let ty = self.eval_type(&mut frame, local.ty())?;
            if self.types().is_void(ty) {
                continue;
            }
            let ty = match self.types().kind(ty).clone() {
                TypeKind::Array { element, .. } => self.types_mut().pointer(element),
                TypeKind::Function(_) => self.types_mut().pointer(ty),
                _ => ty,
            };
            runtime.push(*param);
            params.push((local.name().cloned(), ty));
        }
        let ret = self.eval_type(&mut frame, function.signature().return_type())?;
        let signature = Rc::new(Signature {
            runtime,
            params,
            ret,
            variadic: function.signature().is_variadic(),
            linkage: function.signature().linkage(),
            link_name: function.signature().link_name().clone(),
        });
        self.signatures_mut()
            .insert(instance.clone(), signature.clone());
        Ok(signature)
    }

    pub(crate) fn function_type(
        &mut self,
        instance: &Instance,
        span: &TokenRange,
    ) -> CXResult<TypeID> {
        let signature = self.signature(instance, span)?;
        let params = signature.params().iter().map(|(_, ty)| *ty).collect();
        let function = FunctionType::new(params, signature.ret(), signature.is_variadic());
        Ok(self.types_mut().intern(TypeKind::Function(function)))
    }

    pub(crate) fn global_type(&mut self, key: DefKey, span: &TokenRange) -> CXResult<TypeID> {
        if let Some(ty) = self.global_types_mut().get(&key) {
            return Ok(*ty);
        }
        let unit = self.unit(key.unit());
        let HMIRDefKind::Global(global) = unit.def(key.def()).kind() else {
            return Err(staging_error(span, "expected a global".into()));
        };
        let mut frame = EvalFrame::new(unit.clone(), key, Rc::new((key, Vec::new())));
        let mut ty = self.eval_type(&mut frame, global.ty())?;
        if let TypeKind::Array {
            element,
            length: None,
        } = self.types().kind(ty).clone()
            && let Some(initializer) = global.initializer()
            && let Some(length) = self.static_length(&mut frame, initializer, element)?
        {
            ty = self.types_mut().intern(TypeKind::Array {
                element,
                length: Some(length),
            });
        }
        self.global_types_mut().insert(key, ty);
        Ok(ty)
    }

    // The element count an initializer gives an array declared without a length
    fn static_length(
        &mut self,
        frame: &mut EvalFrame,
        initializer: HMIRExprID,
        _element: TypeID,
    ) -> CXResult<Option<u64>> {
        let unit = frame.unit.clone();
        let body = def_body(unit.def(frame.def.def())).expect("global has a body");
        Ok(match body.expr(initializer).kind() {
            HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Initialize {
                fields,
                ..
            })) => Some(fields.len() as u64),
            HMIRExprKind::Constant(HMIRConstant::Str(string)) => Some(string.len() as u64 + 1),
            _ => match self.eval(frame, initializer)? {
                StaticValue::Str(string) => Some(string.len() as u64 + 1),
                StaticValue::Aggregate { fields, .. } => Some(fields.len() as u64),
                _ => None,
            },
        })
    }

    pub(crate) fn eval_global_initializer(
        &mut self,
        key: DefKey,
        initializer: HMIRExprID,
        ty: TypeID,
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        let unit = self.unit(key.unit());
        let mut frame = EvalFrame::new(unit, key, Rc::new((key, Vec::new())));
        let value = self.eval_expecting(&mut frame, initializer, Some(ty))?;
        self.coerce_static(value, ty, span)
    }

    // A global read at compile time decays when it is an array and is otherwise its
    // initial value
    fn read_global(&mut self, value: StaticValue, span: &TokenRange) -> CXResult<StaticValue> {
        let StaticValue::Global(key) = value else {
            return Ok(value);
        };
        let ty = self.global_type(key, span)?;
        if let TypeKind::Array { element, .. } = self.types().kind(ty).clone() {
            return Ok(StaticValue::GlobalAddress {
                def: key,
                offset: 0,
                ty: self.types_mut().pointer(element),
            });
        }
        let unit = self.unit(key.unit());
        let HMIRDefKind::Global(global) = unit.def(key.def()).kind() else {
            return Err(staging_error(span, "expected a global".into()));
        };
        match global.initializer() {
            Some(initializer) => self.eval_global_initializer(key, initializer, ty, span),
            None => Err(staging_error(
                span,
                "global without an initializer read at compile time".into(),
            )),
        }
    }

    pub(crate) fn def_value(&mut self, key: DefKey, span: &TokenRange) -> CXResult<StaticValue> {
        let unit = self.unit(key.unit());
        let def = unit.def(key.def());
        match def.kind() {
            HMIRDefKind::Function(_) => Ok(StaticValue::Function {
                def: key,
                args: Vec::new(),
            }),
            HMIRDefKind::Global(_) => Ok(StaticValue::Global(key)),
            HMIRDefKind::Type(ty) => Ok(StaticValue::Type(self.import_type(
                key.unit(),
                *ty,
                span,
            )?)),
            HMIRDefKind::ComptimeGlobal(global) => {
                let instance = (key, Vec::new());
                if let Some(value) = self.generated_mut().get(&instance) {
                    return Ok(value.clone());
                }
                if !self.active_mut().insert(instance.clone()) {
                    return Err(staging_error(
                        span,
                        format!("'{}' depends on itself", def.name()),
                    ));
                }
                let mut frame = EvalFrame::new(unit.clone(), key, Rc::new(instance.clone()));
                let result = (|| {
                    let value = self.eval(&mut frame, global.initializer())?;
                    let ty = self.eval(&mut frame, global.ty())?;
                    match (ty, &value) {
                        (StaticValue::Type(ty), StaticValue::Int { .. })
                            if self.types().int_info(ty).is_some() =>
                        {
                            self.coerce_static(value, ty, span)
                        }
                        _ => Ok(value),
                    }
                })();
                self.active_mut().remove(&instance);
                let value = result?;
                self.generated_mut().insert(instance, value.clone());
                Ok(value)
            }
        }
    }

    pub(crate) fn call_static(
        &mut self,
        callee: StaticValue,
        args: Vec<StaticValue>,
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        let StaticValue::Function { def, args: bound } = callee else {
            return Err(staging_error(
                span,
                "called a non-function at compile time".into(),
            ));
        };
        let unit = self.unit(def.unit());
        let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
            return Err(staging_error(
                span,
                "called a non-function at compile time".into(),
            ));
        };
        let Some(root) = function.root() else {
            return Err(staging_error(
                span,
                format!("'{}' has no body to evaluate", unit.def(def.def()).name()),
            ));
        };
        let params = function.signature().params().len();
        let mut all = bound;
        all.extend(args);
        if all.len() < params {
            all = self.deduce_static(def, all, span)?;
        }
        if all.len() != params {
            return Err(staging_error(
                span,
                format!(
                    "'{}' expects {params} arguments, found {}",
                    unit.def(def.def()).name(),
                    all.len()
                ),
            ));
        }

        let memoize = all
            .iter()
            .all(|arg| matches!(arg, StaticValue::Type(_) | StaticValue::Int { .. }));
        let instance = (def, all);
        if memoize && let Some(value) = self.generated_mut().get(&instance) {
            return Ok(value.clone());
        }
        if memoize && !self.active_mut().insert(instance.clone()) {
            return Err(staging_error(
                span,
                format!("'{}' depends on itself", unit.def(def.def()).name()),
            ));
        }

        let mut frame = EvalFrame::new(unit.clone(), def, Rc::new(instance.clone()));
        let body = function.body();
        for (param, arg) in function.signature().params().iter().zip(&instance.1) {
            let declared = body.local(*param).ty();
            let arg = match self.eval(&mut frame, declared) {
                Ok(StaticValue::Type(ty))
                    if !matches!(
                        self.types().kind(ty),
                        TypeKind::Expr { .. } | TypeKind::Type
                    ) =>
                {
                    self.coerce_static(arg.clone(), ty, span)?
                }
                _ => arg.clone(),
            };
            frame.bind(*param, arg);
        }
        let result = self.exec(&mut frame, root, None);
        if memoize {
            self.active_mut().remove(&instance);
        }
        let value = match result? {
            Flow::Normal(value) | Flow::Return(value) => value,
            _ => {
                return Err(staging_error(
                    span,
                    "loop control escaped a function".into(),
                ));
            }
        };
        if memoize {
            self.generated_mut().insert(instance, value.clone());
        }
        Ok(value)
    }

    pub(crate) fn eval(&mut self, frame: &mut EvalFrame, id: HMIRExprID) -> CXResult<StaticValue> {
        self.eval_expecting(frame, id, None)
    }

    pub(crate) fn eval_expecting(
        &mut self,
        frame: &mut EvalFrame,
        id: HMIRExprID,
        expect: Option<TypeID>,
    ) -> CXResult<StaticValue> {
        match self.exec(frame, id, expect)? {
            Flow::Normal(value) => Ok(value),
            _ => {
                let span = frame.body().expr(id).span().clone();
                Err(staging_error(
                    &span,
                    "control flow escaped a comptime expression".into(),
                ))
            }
        }
    }

    pub(crate) fn eval_type(&mut self, frame: &mut EvalFrame, id: HMIRExprID) -> CXResult<TypeID> {
        let value = self.eval(frame, id)?;
        match value {
            StaticValue::Type(ty) => Ok(ty),
            StaticValue::Unit => Ok(self.types_mut().void()),
            other => {
                let span = frame.body().expr(id).span().clone();
                Err(staging_error(
                    &span,
                    format!("expected a type, found {other:?}"),
                ))
            }
        }
    }

    // Like 'eval_type' but a hole yields None
    pub(crate) fn eval_type_hint(
        &mut self,
        frame: &mut EvalFrame,
        id: HMIRExprID,
    ) -> CXResult<Option<TypeID>> {
        if matches!(frame.body().expr(id).kind(), HMIRExprKind::Hole(_)) {
            return Ok(None);
        }
        self.eval_type(frame, id).map(Some)
    }

    pub(crate) fn exec(
        &mut self,
        frame: &mut EvalFrame,
        id: HMIRExprID,
        expect: Option<TypeID>,
    ) -> CXResult<Flow> {
        match lower(Context::Comptime(self, frame), id, Expect::of(expect)) {
            Ok(Output::Comptime(flow)) => Ok(flow),
            Err(Stop::Error(error)) => Err(error),
            _ => unreachable!(),
        }
    }

    pub(crate) fn static_condition(
        &mut self,
        frame: &mut EvalFrame,
        condition: HMIRExprID,
        span: &TokenRange,
    ) -> CXResult<bool> {
        self.eval(frame, condition)?
            .is_truthy()
            .ok_or_else(|| staging_error(span, "condition is not a compile-time boolean".into()))
    }

    pub(crate) fn exec_native(
        &mut self,
        frame: &mut EvalFrame,
        id: HMIRExprID,
        op: &HMIRNativeOp,
        span: &TokenRange,
        expect: Option<TypeID>,
    ) -> CXResult<Flow> {
        let value = match op {
            HMIRNativeOp::BinOp { op, lhs, rhs } => match op {
                HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                    let lhs = self.static_condition(frame, *lhs, span)?;
                    let result = match (op, lhs) {
                        (HMIRBinaryOp::LAnd, false) => false,
                        (HMIRBinaryOp::LOr, true) => true,
                        _ => self.static_condition(frame, *rhs, span)?,
                    };
                    StaticValue::bool(result, self.types_mut())
                }
                _ => {
                    let lhs = self.eval(frame, *lhs)?;
                    let lhs = self.read_global(lhs, span)?;
                    let rhs = self.eval(frame, *rhs)?;
                    let rhs = self.read_global(rhs, span)?;
                    self.fold_binary(*op, lhs, rhs, span)?
                }
            },
            HMIRNativeOp::UnOp { op, operand } => self.exec_unary(frame, *op, *operand, span)?,
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            } => {
                let target = self.eval_type_hint(frame, *target)?;
                let value = self.eval_expecting(frame, *value, target)?;
                match (mode, target) {
                    (HMIRCoerceMode::Truthy, _) => {
                        let truthy = value.is_truthy().ok_or_else(|| {
                            staging_error(span, "value has no compile-time truthiness".into())
                        })?;
                        StaticValue::bool(truthy, self.types_mut())
                    }
                    (_, Some(ty)) => self.coerce_static(value, ty, span)?,
                    (_, None) => value,
                }
            }
            HMIRNativeOp::Assign { target, op, value } => {
                let unit = frame.unit.clone();
                let body = def_body(unit.def(frame.def.def())).expect("evaluated def has a body");
                let HMIRExprKind::Local(local) = body.expr(*target).kind() else {
                    return Err(staging_error(
                        span,
                        "comptime assignment to a non-local".into(),
                    ));
                };
                let mut value = self.eval(frame, *value)?;
                let current = frame.locals.get(local).cloned();
                if let Some(op) = op {
                    let current = current.clone().ok_or_else(|| {
                        staging_error(span, "compound assignment to an unset local".into())
                    })?;
                    value = self.fold_binary(*op, current, value, span)?;
                }
                if let Some(ty) = current.and_then(|current| current.simple_type(self.types_mut()))
                    && self.types().int_info(ty).is_some()
                {
                    value = self.coerce_static(value, ty, span)?;
                }
                frame.bind(*local, value.clone());
                value
            }
            HMIRNativeOp::AddressOf(inner) => self.static_address(frame, *inner, span)?,
            HMIRNativeOp::Dereference(inner) => expr::dereference(self, frame, *inner, span)?,
            HMIRNativeOp::Type(HMIRTypeOp::Aggregate {
                kind,
                semantics,
                fields,
            }) => self.eval_aggregate_type(frame, id, *kind, *semantics, fields, span)?,
            HMIRNativeOp::Type(op) => self.exec_type_op(frame, op, span)?,
            HMIRNativeOp::Control(control) => {
                return Ok(match control {
                    HMIRControlOp::Return(value) => Flow::Return(match value {
                        Some(value) => self.eval(frame, *value)?,
                        None => StaticValue::Unit,
                    }),
                    HMIRControlOp::Yield(value) => Flow::Yield(match value {
                        Some(value) => self.eval(frame, *value)?,
                        None => StaticValue::Unit,
                    }),
                    HMIRControlOp::Break => Flow::Break,
                    HMIRControlOp::Continue => Flow::Continue,
                    HMIRControlOp::Unsafe(inner) => return self.exec(frame, *inner, expect),
                    HMIRControlOp::Goto(_)
                    | HMIRControlOp::Defer(_)
                    | HMIRControlOp::Unreachable => {
                        return Err(staging_error(
                            span,
                            format!("'{}' at compile time", control.path()),
                        ));
                    }
                });
            }
            HMIRNativeOp::OwnershipOp(op) => match op {
                HMIROwnershipOp::Move(inner) | HMIROwnershipOp::Leak(inner) => {
                    return self.exec(frame, *inner, expect);
                }
                HMIROwnershipOp::Allocate(_) | HMIROwnershipOp::Adopt(_) => {
                    return Err(staging_error(
                        span,
                        format!("'{}' at compile time", op.path()),
                    ));
                }
            },
            HMIRNativeOp::AggregateOp(op) => self.exec_aggregate(frame, op, span, expect)?,
        };
        Ok(Flow::Normal(value))
    }

    fn exec_aggregate(
        &mut self,
        frame: &mut EvalFrame,
        op: &HMIRAggregateOp,
        span: &TokenRange,
        expect: Option<TypeID>,
    ) -> CXResult<StaticValue> {
        match op {
            HMIRAggregateOp::Initialize { ty, fields } => {
                let ty = match self.eval_type_hint(frame, *ty)? {
                    Some(ty) => ty,
                    None => expect.ok_or_else(|| {
                        staging_error(span, "cannot infer the initializer's type".into())
                    })?,
                };
                self.static_initializer(frame, ty, fields, span)
            }
            HMIRAggregateOp::Member { base, name } => {
                let base = self.eval(frame, *base)?;
                let StaticValue::Aggregate { ty, fields } = base else {
                    return Err(staging_error(
                        span,
                        format!("no compile-time member '{name}'"),
                    ));
                };
                let (index, _) = self
                    .types()
                    .field(ty, name.as_str())
                    .ok_or_else(|| staging_error(span, format!("no member '{name}'")))?;
                fields
                    .into_iter()
                    .find(|(field, _)| *field == index)
                    .map(|(_, value)| value)
                    .ok_or_else(|| staging_error(span, format!("member '{name}' is not set")))
            }
            HMIRAggregateOp::Index { base, index } => {
                let base = self.eval(frame, *base)?;
                let index = self.eval(frame, *index)?.as_int();
                match (base, index) {
                    (StaticValue::Aggregate { fields, .. }, Some(index)) => fields
                        .into_iter()
                        .find(|(field, _)| *field as i128 == index)
                        .map(|(_, value)| value)
                        .ok_or_else(|| staging_error(span, "index out of bounds".into())),
                    (StaticValue::Str(string), Some(index)) => {
                        let byte = string.as_bytes().get(index as usize).copied().unwrap_or(0);
                        let char = self.types_mut().int(HMIRIntWidth::I8, true);
                        Ok(StaticValue::int(byte as i128, char))
                    }
                    _ => Err(staging_error(span, "compile-time index".into())),
                }
            }
            HMIRAggregateOp::Is { value, pattern } => {
                let value = self.eval(frame, *value)?;
                let matched = match (pattern, &value) {
                    (HMIRPattern::Integer(expected), StaticValue::Int { value, .. }) => {
                        *value == *expected as i128
                    }
                    (HMIRPattern::Variant { index, .. }, StaticValue::Aggregate { fields, .. }) => {
                        fields.first().is_some_and(|(field, _)| field == index)
                    }
                    (HMIRPattern::Binding(_), _) => true,
                    _ => return Err(staging_error(span, "compile-time pattern test".into())),
                };
                Ok(StaticValue::bool(matched, self.types_mut()))
            }
        }
    }

    fn static_initializer(
        &mut self,
        frame: &mut EvalFrame,
        ty: TypeID,
        fields: &[(Option<CXIdent>, HMIRExprID)],
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        if let [(None, value)] = fields
            && !matches!(
                self.types().kind(ty),
                TypeKind::Array { .. } | TypeKind::Nominal(_)
            )
        {
            let value = self.eval_expecting(frame, *value, Some(ty))?;
            return self.coerce_static(value, ty, span);
        }
        let mut values = Vec::with_capacity(fields.len());
        let mut next = 0;
        for (name, value) in fields {
            let index = match name {
                Some(name) => self
                    .types()
                    .field(ty, name.as_str())
                    .map(|(index, _)| index)
                    .ok_or_else(|| staging_error(span, format!("no member '{name}'")))?,
                None => next,
            };
            let member = self.member_type(ty, index, span)?;
            let value = self.eval_expecting(frame, *value, Some(member))?;
            values.push((index, self.coerce_static(value, member, span)?));
            next = index + 1;
        }
        Ok(StaticValue::Aggregate { ty, fields: values })
    }

    fn static_address(
        &mut self,
        frame: &mut EvalFrame,
        inner: HMIRExprID,
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        let unit = frame.unit.clone();
        let body = def_body(unit.def(frame.def.def())).expect("evaluated def has a body");
        if let HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Index {
            base,
            index,
        })) = body.expr(inner).kind()
        {
            let base = self.static_address(frame, *base, span)?;
            let index = self
                .eval(frame, *index)?
                .as_int()
                .ok_or_else(|| staging_error(span, "non-constant index".into()))?;
            if let StaticValue::GlobalAddress { def, offset, ty } = base {
                let element = match self.types().kind(ty).clone() {
                    TypeKind::Pointer(array) => match self.types().kind(array).clone() {
                        TypeKind::Array { element, .. } => element,
                        _ => array,
                    },
                    _ => return Err(staging_error(span, "address of a non-pointer".into())),
                };
                let size = self.types_mut().size_of(element, span)? as i64;
                return Ok(StaticValue::GlobalAddress {
                    def,
                    offset: offset + size * index as i64,
                    ty: self.types_mut().pointer(element),
                });
            }
            return Err(staging_error(span, "compile-time address".into()));
        }
        match self.eval(frame, inner)? {
            StaticValue::Global(def) => {
                let ty = self.global_type(def, span)?;
                Ok(StaticValue::GlobalAddress {
                    def,
                    offset: 0,
                    ty: self.types_mut().pointer(ty),
                })
            }
            function @ StaticValue::Function { .. } => Ok(function),
            _ => Err(staging_error(span, "compile-time address".into())),
        }
    }

    fn exec_type_op(
        &mut self,
        frame: &mut EvalFrame,
        op: &HMIRTypeOp,
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        types::exec_type_op(self, frame, op, span)
    }

    pub(crate) fn quote_type(&mut self, quote: &Quote) -> Option<TypeID> {
        let unit = self.unit(quote.unit());
        let key = DefKey::new(quote.unit(), quote.def());
        let mut frame = EvalFrame::new(unit, key, Rc::new(quote.owner().clone())).with_runtime(
            RuntimeView::new(quote.origin(), quote.runtime_types().clone()),
        );
        for (local, value) in quote.env() {
            frame.bind(*local, value.clone());
        }
        let params = quote
            .params()
            .iter()
            .map(|param| {
                let ty = frame.body().local(*param).ty();
                self.eval_type(&mut frame, ty)
            })
            .collect::<CXResult<Vec<_>>>()
            .ok()?;
        let result = self.type_hint(&mut frame, quote.body())?;
        Some(self.types_mut().intern(TypeKind::Expr { params, result }))
    }

    pub(crate) fn type_hint(&mut self, frame: &mut EvalFrame, id: HMIRExprID) -> Option<TypeID> {
        inspect(self, frame, id).ok()
    }

    pub(crate) fn static_type(
        &mut self,
        value: &StaticValue,
        span: &TokenRange,
    ) -> CXResult<TypeID> {
        if let Some(ty) = value.simple_type(self.types_mut()) {
            return Ok(ty);
        }
        match value {
            StaticValue::Function { def, args } => self.function_type(&(*def, args.clone()), span),
            StaticValue::Global(def) => self.global_type(*def, span),
            StaticValue::Quote(quote) => self.quote_type(quote.get()).ok_or_else(|| {
                staging_error(span, "cannot infer the quoted expression's type".into())
            }),
            _ => unreachable!("simple_type covers the remaining values"),
        }
    }
}
