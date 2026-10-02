pub(crate) mod control;
pub(crate) mod expr;
pub(crate) mod liveness;
pub(crate) mod ops;
mod types;

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody, HMIRCoerceMode, HMIRConstant, HMIRControlOp,
    HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRLocalID, HMIRNativeOp,
    HMIROwnershipOp, HMIRPattern, HMIRTypeOp, HMIRUnit,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    deduce::deduce_static,
    eval::{
        ops::{coerce_static, exec_unary, fold_binary},
        types::eval_aggregate_type,
    },
    function::{Expect, Stop, inspect::inspect},
    lower::{LowerContext, LowerOutput, lower},
    module::member_type,
    program::{DefKey, Instance, Program, UnitID, def_body},
    staging_error,
    ty::{FunctionType, TypeID, TypeKind},
    value::{FrameRef, Quote, QuoteRef, StaticValue, truncate_int},
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
    ret: Option<TypeID>,
    moved: HashSet<HMIRLocalID>,
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
            ret: None,
            moved: HashSet::new(),
        }
    }

    pub(crate) fn with_runtime(mut self, view: RuntimeView) -> Self {
        self.runtime = Some(view);
        self
    }

    pub(crate) fn bind(&mut self, local: HMIRLocalID, value: StaticValue) {
        self.moved.remove(&local);
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

    pub(crate) fn as_local(&self, id: HMIRExprID) -> Option<HMIRLocalID> {
        match self.body().expr(id).kind() {
            HMIRExprKind::Local(local) => Some(*local),
            _ => None,
        }
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

pub(crate) fn eval_frame_for(cx: &mut Program<'_>, instance: &Instance) -> EvalFrame {
    let unit = cx.unit(instance.0.unit());
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

pub(crate) fn eval_signature(
    cx: &mut Program<'_>,
    instance: &Instance,
    span: &TokenRange,
) -> CXResult<Rc<Signature>> {
    if let Some(signature) = cx.signatures_mut().get(instance) {
        return Ok(signature.clone());
    }
    let unit = cx.unit(instance.0.unit());
    let def = unit.def(instance.0.def());
    let HMIRDefKind::Function(function) = def.kind() else {
        return Err(staging_error(
            span,
            format!("'{}' is not a function", def.name()),
        ));
    };
    let mut frame = eval_frame_for(cx, instance);
    let body = function.body();
    let mut runtime = Vec::new();
    let mut params = Vec::new();
    for param in function.signature().params() {
        let local = body.local(*param);
        if local.is_comptime() {
            continue;
        }
        let ty = eval_type(cx, &mut frame, local.ty())?;
        if cx.types().is_void(ty) {
            continue;
        }
        let ty = match cx.types().kind(ty).clone() {
            TypeKind::Array { element, .. } => cx.types_mut().pointer_to(element),
            TypeKind::Function(_) => cx.types_mut().pointer_to(ty),
            _ => ty,
        };
        runtime.push(*param);
        params.push((local.name().cloned(), ty));
    }
    let ret = eval_type(cx, &mut frame, function.signature().return_type())?;
    let signature = Rc::new(Signature {
        runtime,
        params,
        ret,
        variadic: function.signature().is_variadic(),
        linkage: function.signature().linkage(),
        link_name: function.signature().link_name().clone(),
    });
    cx.signatures_mut()
        .insert(instance.clone(), signature.clone());
    Ok(signature)
}

pub(crate) fn eval_function_type(
    cx: &mut Program<'_>,
    instance: &Instance,
    span: &TokenRange,
) -> CXResult<TypeID> {
    let signature = eval_signature(cx, instance, span)?;
    let params = signature.params().iter().map(|(_, ty)| *ty).collect();
    let function = FunctionType::new(params, signature.ret(), signature.is_variadic());
    Ok(cx.types_mut().intern(TypeKind::Function(function)))
}

pub(crate) fn eval_global_type(
    cx: &mut Program<'_>,
    key: DefKey,
    span: &TokenRange,
) -> CXResult<TypeID> {
    if let Some(ty) = cx.global_types_mut().get(&key) {
        return Ok(*ty);
    }
    let unit = cx.unit(key.unit());
    let HMIRDefKind::Global(global) = unit.def(key.def()).kind() else {
        return Err(staging_error(span, "expected a global".into()));
    };
    let mut frame = EvalFrame::new(unit.clone(), key, Rc::new((key, Vec::new())));
    let mut ty = eval_type(cx, &mut frame, global.ty())?;
    if let TypeKind::Array {
        element,
        length: None,
    } = cx.types().kind(ty).clone()
        && let Some(initializer) = global.initializer()
        && let Some(length) = static_length(cx, &mut frame, initializer)?
    {
        ty = cx.types_mut().intern(TypeKind::Array {
            element,
            length: Some(length),
        });
    }
    cx.global_types_mut().insert(key, ty);
    Ok(ty)
}

// The element count an initializer gives an array declared without a length
fn static_length(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    initializer: HMIRExprID,
) -> CXResult<Option<u64>> {
    let unit = frame.unit.clone();
    let body = def_body(unit.def(frame.def.def())).expect("global has a body");
    Ok(match body.expr(initializer).kind() {
        HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Initialize {
            fields,
            ..
        })) => Some(fields.len() as u64),
        HMIRExprKind::Constant(HMIRConstant::Str(string)) => Some(string.len() as u64 + 1),
        _ => match eval(cx, frame, initializer, None)? {
            StaticValue::Str(string) => Some(string.len() as u64 + 1),
            StaticValue::Aggregate { fields, .. } => Some(fields.len() as u64),
            _ => None,
        },
    })
}

pub(crate) fn eval_global_initializer(
    cx: &mut Program<'_>,
    key: DefKey,
    initializer: HMIRExprID,
    ty: TypeID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let unit = cx.unit(key.unit());
    let mut frame = EvalFrame::new(unit, key, Rc::new((key, Vec::new())));
    let value = eval(cx, &mut frame, initializer, Some(ty))?;
    coerce_static(cx, value, ty, span)
}

// A global read at compile time decays when it is an array and is otherwise its
// initial value
fn read_global(
    cx: &mut Program<'_>,
    value: StaticValue,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let StaticValue::Global(key) = value else {
        return Ok(value);
    };
    let ty = eval_global_type(cx, key, span)?;
    if let TypeKind::Array { element, .. } = cx.types().kind(ty).clone() {
        return Ok(StaticValue::GlobalAddress {
            def: key,
            offset: 0,
            ty: cx.types_mut().pointer_to(element),
        });
    }
    let unit = cx.unit(key.unit());
    let HMIRDefKind::Global(global) = unit.def(key.def()).kind() else {
        return Err(staging_error(span, "expected a global".into()));
    };
    match global.initializer() {
        Some(initializer) => eval_global_initializer(cx, key, initializer, ty, span),
        None => Err(staging_error(
            span,
            "global without an initializer read at compile time".into(),
        )),
    }
}

pub(crate) fn def_value(
    cx: &mut Program<'_>,
    key: DefKey,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let unit = cx.unit(key.unit());
    let def = unit.def(key.def());
    match def.kind() {
        HMIRDefKind::Function(_) => Ok(StaticValue::Function {
            def: key,
            args: Vec::new(),
        }),
        HMIRDefKind::Global(_) => Ok(StaticValue::Global(key)),
        HMIRDefKind::Type(ty) => Ok(StaticValue::Type(cx.import_type(key.unit(), *ty, span)?)),
        HMIRDefKind::ComptimeGlobal(global) => {
            let instance = (key, Vec::new());
            if let Some(value) = cx.generated_mut().get(&instance) {
                return Ok(value.clone());
            }
            if !cx.active_mut().insert(instance.clone()) {
                return Err(staging_error(
                    span,
                    format!("'{}' depends on itself", def.name()),
                ));
            }
            let mut frame = EvalFrame::new(unit.clone(), key, Rc::new(instance.clone()));
            let result = (|| {
                let value = eval(cx, &mut frame, global.initializer(), None)?;
                let ty = eval(cx, &mut frame, global.ty(), None)?;
                match (ty, &value) {
                    (StaticValue::Type(ty), StaticValue::Int { .. })
                        if cx.types().int_info(ty).is_some() =>
                    {
                        coerce_static(cx, value, ty, span)
                    }
                    _ => Ok(value),
                }
            })();
            cx.active_mut().remove(&instance);
            let value = result?;
            cx.generated_mut().insert(instance, value.clone());
            Ok(value)
        }
    }
}

// Fewer arguments than parameters bind the leading comptime parameters and yield the function;
// a 'None' argument is a hole deduced from the arguments after the template prefix
pub(crate) fn call_static(
    cx: &mut Program<'_>,
    callee: StaticValue,
    args: Vec<Option<StaticValue>>,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let StaticValue::Function { def, args: bound } = callee else {
        return Err(staging_error(
            span,
            "called a non-function at compile time".into(),
        ));
    };
    let unit = cx.unit(def.unit());
    let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
        return Err(staging_error(
            span,
            "called a non-function at compile time".into(),
        ));
    };
    let params = function.signature().params();
    let mut all = bound.into_iter().map(Some).collect::<Vec<_>>();
    all.extend(args);
    let curried = all.len() < params.len()
        && params[..all.len()]
            .iter()
            .all(|param| function.body().local(*param).is_comptime());
    if all.len() != params.len() && !curried {
        return Err(staging_error(
            span,
            format!(
                "'{}' expects {} arguments, found {}",
                unit.def(def.def()).name(),
                params.len(),
                all.len()
            ),
        ));
    }
    let all = match all.iter().cloned().collect::<Option<Vec<_>>>() {
        Some(all) => all,
        None if curried => return Err(staging_error(span, "cannot infer this type".into())),
        None => deduce_static(cx, def, all, span)?,
    };
    if curried {
        return Ok(StaticValue::Function { def, args: all });
    }
    let Some(root) = function.root() else {
        return Err(staging_error(
            span,
            format!("'{}' has no body to evaluate", unit.def(def.def()).name()),
        ));
    };

    let memoize = all
        .iter()
        .all(|arg| matches!(arg, StaticValue::Type(_) | StaticValue::Int { .. }));
    let instance = (def, all);
    if memoize && let Some(value) = cx.generated_mut().get(&instance) {
        return Ok(value.clone());
    }
    if memoize && !cx.active_mut().insert(instance.clone()) {
        return Err(staging_error(
            span,
            format!("'{}' depends on itself", unit.def(def.def()).name()),
        ));
    }

    let mut frame = EvalFrame::new(unit.clone(), def, Rc::new(instance.clone()));
    let body = function.body();
    for (param, arg) in function.signature().params().iter().zip(&instance.1) {
        let declared = body.local(*param).ty();
        let arg = match eval(cx, &mut frame, declared, None) {
            Ok(StaticValue::Type(ty)) if !matches!(cx.types().kind(ty), TypeKind::Type) => {
                coerce_static(cx, arg.clone(), ty, span)?
            }
            _ => arg.clone(),
        };
        frame.bind(*param, arg);
    }
    frame.ret = eval_type_hint(cx, &mut frame, function.signature().return_type())
        .ok()
        .flatten();
    let result = exec(cx, &mut frame, root, None);
    if memoize {
        cx.active_mut().remove(&instance);
    }
    let value = match result? {
        Flow::Normal(value) | Flow::Return(value) => {
            liveness::require_consumed(cx, &frame)?;
            value
        }
        _ => {
            return Err(staging_error(
                span,
                "loop control escaped a function".into(),
            ));
        }
    };
    let value = match frame.ret {
        Some(ty) if matches!(cx.types().kind(ty), TypeKind::Expr { .. }) => {
            coerce_static(cx, value, ty, span)?
        }
        _ => value,
    };
    if memoize {
        cx.generated_mut().insert(instance, value.clone());
    }
    Ok(value)
}

pub(crate) fn eval(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    expect: Option<TypeID>,
) -> CXResult<StaticValue> {
    match exec(cx, frame, id, expect)? {
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

pub(crate) fn eval_type(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
) -> CXResult<TypeID> {
    let value = eval(cx, frame, id, None)?;
    match value {
        StaticValue::Type(ty) => Ok(ty),
        StaticValue::Unit => Ok(cx.types_mut().void()),
        other => {
            let span = frame.body().expr(id).span().clone();
            Err(staging_error(
                &span,
                format!("expected a type, found {}", other.describe()),
            ))
        }
    }
}

// Like 'eval_type' but a hole yields None
pub(crate) fn eval_type_hint(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
) -> CXResult<Option<TypeID>> {
    if matches!(frame.body().expr(id).kind(), HMIRExprKind::Hole(_)) {
        return Ok(None);
    }
    eval_type(cx, frame, id).map(Some)
}

pub(crate) fn exec(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    expect: Option<TypeID>,
) -> CXResult<Flow> {
    match lower(LowerContext::Comptime(cx, frame), id, Expect::of(expect)) {
        Ok(LowerOutput::Comptime(flow)) => Ok(flow),
        Err(Stop::Error(error)) => Err(error),
        _ => unreachable!(),
    }
}

pub(crate) fn static_condition(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    span: &TokenRange,
) -> CXResult<bool> {
    eval(cx, frame, condition, None)?
        .is_truthy()
        .ok_or_else(|| staging_error(span, "condition is not a compile-time boolean".into()))
}

pub(crate) fn exec_native(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    op: &HMIRNativeOp,
    span: &TokenRange,
    expect: Option<TypeID>,
) -> CXResult<Flow> {
    let value = match op {
        HMIRNativeOp::BinOp { op, lhs, rhs } => match op {
            HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                let lhs = static_condition(cx, frame, *lhs, span)?;
                let result = match (op, lhs) {
                    (HMIRBinaryOp::LAnd, false) => false,
                    (HMIRBinaryOp::LOr, true) => true,
                    _ => static_condition(cx, frame, *rhs, span)?,
                };
                StaticValue::bool(result, cx.types_mut())
            }
            _ => {
                let lhs = eval(cx, frame, *lhs, None)?;
                let lhs = read_global(cx, lhs, span)?;
                let rhs = eval(cx, frame, *rhs, None)?;
                let rhs = read_global(cx, rhs, span)?;
                fold_binary(cx, *op, lhs, rhs, span)?
            }
        },
        HMIRNativeOp::UnOp { op, operand } => exec_unary(cx, frame, *op, *operand, span)?,
        HMIRNativeOp::Coerce {
            mode,
            value,
            target,
        } => {
            let target = eval_type_hint(cx, frame, *target)?;
            let value = eval(cx, frame, *value, target)?;
            match (mode, target) {
                (HMIRCoerceMode::Truthy, _) => {
                    let truthy = value.is_truthy().ok_or_else(|| {
                        staging_error(span, "value has no compile-time truthiness".into())
                    })?;
                    StaticValue::bool(truthy, cx.types_mut())
                }
                (_, Some(ty)) => coerce_static(cx, value, ty, span)?,
                (_, None) => value,
            }
        }
        HMIRNativeOp::Assign { target, op, value } => {
            let Some(local) = frame.as_local(*target) else {
                return Err(staging_error(
                    span,
                    "comptime assignment to a non-local".into(),
                ));
            };
            let mut value = eval(cx, frame, *value, None)?;
            let current = frame.locals.get(&local).cloned();
            if let Some(op) = op {
                let current = current.clone().ok_or_else(|| {
                    staging_error(span, "compound assignment to an unset local".into())
                })?;
                value = fold_binary(cx, *op, current, value, span)?;
            }
            if let Some(ty) = current.and_then(|current| current.simple_type(cx.types_mut()))
                && cx.types().int_info(ty).is_some()
            {
                value = coerce_static(cx, value, ty, span)?;
            }
            frame.bind(local, value.clone());
            value
        }
        HMIRNativeOp::AddressOf(inner) => static_address(cx, frame, *inner, span)?,
        HMIRNativeOp::Dereference(inner) => expr::dereference(cx, frame, *inner, span)?,
        HMIRNativeOp::Type(HMIRTypeOp::Aggregate {
            kind,
            semantics,
            fields,
        }) => eval_aggregate_type(cx, frame, id, *kind, *semantics, fields, span)?,
        HMIRNativeOp::Type(op) => types::exec_type_op(cx, frame, op, span)?,
        HMIRNativeOp::Control(control) => {
            return Ok(match control {
                HMIRControlOp::Return(value) => Flow::Return(match value {
                    Some(value) => eval(cx, frame, *value, frame.ret)?,
                    None => StaticValue::Unit,
                }),
                HMIRControlOp::Yield(value) => Flow::Yield(match value {
                    Some(value) => eval(cx, frame, *value, None)?,
                    None => StaticValue::Unit,
                }),
                HMIRControlOp::Break => Flow::Break,
                HMIRControlOp::Continue => Flow::Continue,
                HMIRControlOp::Unsafe(inner) => return exec(cx, frame, *inner, expect),
                HMIRControlOp::Goto(_) | HMIRControlOp::Defer(_) | HMIRControlOp::Unreachable => {
                    return Err(staging_error(
                        span,
                        format!("'{}' at compile time", control.path()),
                    ));
                }
            });
        }
        HMIRNativeOp::OwnershipOp(op) => match op {
            HMIROwnershipOp::Move(inner) | HMIROwnershipOp::Leak(inner) => {
                let flow = exec(cx, frame, *inner, expect)?;
                liveness::consume(frame, *inner);
                return Ok(flow);
            }
            HMIROwnershipOp::Allocate(_) | HMIROwnershipOp::Adopt(_) => {
                return Err(staging_error(
                    span,
                    format!("'{}' at compile time", op.path()),
                ));
            }
        },
        HMIRNativeOp::AggregateOp(op) => exec_aggregate(cx, frame, op, span, expect)?,
    };
    Ok(Flow::Normal(value))
}

fn exec_aggregate(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    op: &HMIRAggregateOp,
    span: &TokenRange,
    expect: Option<TypeID>,
) -> CXResult<StaticValue> {
    match op {
        HMIRAggregateOp::Initialize { ty, fields } => {
            let ty = match eval_type_hint(cx, frame, *ty)? {
                Some(ty) => ty,
                None => expect.ok_or_else(|| {
                    staging_error(span, "cannot infer the initializer's type".into())
                })?,
            };
            static_initializer(cx, frame, ty, fields, span)
        }
        HMIRAggregateOp::Unpack { value, bindings } => {
            let StaticValue::Aggregate { ty, fields } = eval(cx, frame, *value, None)? else {
                return Err(staging_error(
                    span,
                    "@unpack takes an owned structure".into(),
                ));
            };
            for (name, local) in bindings {
                let (index, _) = cx
                    .types()
                    .field(ty, name.as_str())
                    .ok_or_else(|| staging_error(span, format!("no member '{name}'")))?;
                let value = fields
                    .iter()
                    .find(|(field, _)| *field == index)
                    .map(|(_, value)| value.clone())
                    .ok_or_else(|| staging_error(span, format!("member '{name}' is not set")))?;
                frame.bind(*local, value);
            }
            Ok(StaticValue::Unit)
        }
        HMIRAggregateOp::Member { base, name } => {
            let base = eval(cx, frame, *base, None)?;
            let StaticValue::Aggregate { ty, fields } = read_global(cx, base, span)? else {
                return Err(staging_error(
                    span,
                    format!("no compile-time member '{name}'"),
                ));
            };
            let (index, _) = cx
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
            let base = eval(cx, frame, *base, None)?;
            let index = eval(cx, frame, *index, None)?.as_int();
            match (base, index) {
                (StaticValue::Aggregate { fields, .. }, Some(index)) => fields
                    .into_iter()
                    .find(|(field, _)| *field as i128 == index)
                    .map(|(_, value)| value)
                    .ok_or_else(|| staging_error(span, "index out of bounds".into())),
                (StaticValue::Str(string), Some(index)) => {
                    let byte = string.as_bytes().get(index as usize).copied().unwrap_or(0);
                    let char = cx.types_mut().int(HMIRIntWidth::I8, true);
                    Ok(StaticValue::int(byte as i128, char))
                }
                _ => Err(staging_error(span, "compile-time index".into())),
            }
        }
        HMIRAggregateOp::Is { value, pattern } => {
            let value = eval(cx, frame, *value, None)?;
            let ty = eval_static_type(cx, &value, span)?;
            if cx.types().is_pointer(ty) {
                return Err(staging_error(
                    span,
                    "pattern subject is a pointer; dereference it explicitly".into(),
                ));
            }
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
            Ok(StaticValue::bool(matched, cx.types_mut()))
        }
    }
}

fn static_initializer(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    ty: TypeID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
    span: &TokenRange,
) -> CXResult<StaticValue> {
    if let [(None, value)] = fields
        && !matches!(
            cx.types().kind(ty),
            TypeKind::Array { .. } | TypeKind::Nominal(_)
        )
    {
        let value = eval(cx, frame, *value, Some(ty))?;
        return coerce_static(cx, value, ty, span);
    }
    let mut values = Vec::with_capacity(fields.len());
    let mut next = 0;
    for (name, value) in fields {
        let index = match name {
            Some(name) => cx
                .types()
                .field(ty, name.as_str())
                .map(|(index, _)| index)
                .ok_or_else(|| staging_error(span, format!("no member '{name}'")))?,
            None => next,
        };
        let member = member_type(cx, ty, index, span)?;
        let value = eval(cx, frame, *value, Some(member))?;
        let value = coerce_static(cx, value, member, span)?;
        values.push((index, truncate_bitfield(cx, ty, index, value)));
        next = index + 1;
    }
    Ok(StaticValue::Aggregate { ty, fields: values })
}

fn truncate_bitfield(
    cx: &Program<'_>,
    ty: TypeID,
    index: usize,
    value: StaticValue,
) -> StaticValue {
    let bits = cx
        .types()
        .nominal_of(ty)
        .and_then(|nominal| nominal.fields().get(index))
        .and_then(|field| field.bit_width());
    match (bits, value) {
        (Some(bits), StaticValue::Int { value, ty }) => {
            let signed = cx.types().int_info(ty).is_some_and(|(_, signed)| signed);
            StaticValue::int(truncate_int(value, bits as u32, signed), ty)
        }
        (_, value) => value,
    }
}

fn static_address(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    inner: HMIRExprID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let unit = frame.unit.clone();
    let body = def_body(unit.def(frame.def.def())).expect("evaluated def has a body");
    if let HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Index { base, index })) =
        body.expr(inner).kind()
    {
        let base = static_address(cx, frame, *base, span)?;
        let index = eval(cx, frame, *index, None)?
            .as_int()
            .ok_or_else(|| staging_error(span, "non-constant index".into()))?;
        if let StaticValue::GlobalAddress { def, offset, ty } = base {
            let element = match cx.types().kind(ty).clone() {
                TypeKind::Pointer(array) => match cx.types().kind(array).clone() {
                    TypeKind::Array { element, .. } => element,
                    _ => array,
                },
                _ => return Err(staging_error(span, "address of a non-pointer".into())),
            };
            let size = cx.types_mut().size_of(element, span)? as i64;
            return Ok(StaticValue::GlobalAddress {
                def,
                offset: offset + size * index as i64,
                ty: cx.types_mut().pointer_to(element),
            });
        }
        return Err(staging_error(span, "compile-time address".into()));
    }
    match eval(cx, frame, inner, None)? {
        StaticValue::Global(def) => {
            let ty = eval_global_type(cx, def, span)?;
            Ok(StaticValue::GlobalAddress {
                def,
                offset: 0,
                ty: cx.types_mut().pointer_to(ty),
            })
        }
        function @ StaticValue::Function { .. } => Ok(function),
        _ => Err(staging_error(span, "compile-time address".into())),
    }
}

pub(crate) fn eval_quote_type(cx: &mut Program<'_>, quote: &Quote) -> Option<TypeID> {
    let unit = cx.unit(quote.unit());
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
            eval_type(cx, &mut frame, ty)
        })
        .collect::<CXResult<Vec<_>>>()
        .ok()?;
    let result = type_hint(cx, &mut frame, quote.body())?;
    Some(cx.types_mut().intern(TypeKind::Expr { params, result }))
}

pub(crate) fn type_hint(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
) -> Option<TypeID> {
    inspect(cx, frame, id, None).ok()
}

pub(crate) fn eval_static_type(
    cx: &mut Program<'_>,
    value: &StaticValue,
    span: &TokenRange,
) -> CXResult<TypeID> {
    if let Some(ty) = value.simple_type(cx.types_mut()) {
        return Ok(ty);
    }
    match value {
        StaticValue::Function { def, args } => eval_function_type(cx, &(*def, args.clone()), span),
        StaticValue::Global(def) => eval_global_type(cx, *def, span),
        StaticValue::Quote(quote) => eval_quote_type(cx, quote.get())
            .ok_or_else(|| staging_error(span, "cannot infer the quoted expression's type".into())),
        _ => unreachable!("simple_type covers the remaining values"),
    }
}
