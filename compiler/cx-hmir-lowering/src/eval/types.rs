use cx_hmir::{
    HMIRAggregateKind, HMIRConstant, HMIRDefKind, HMIRDefRef, HMIRExprID, HMIRExprKind,
    HMIRFieldDef, HMIRMemberStep, HMIRMoveSemantics, HMIROp, HMIRTypeOp,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::ty::layout::calculate_field_layout;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, def_value, eval, eval_type},
    function::inspect::inspect,
    program::{Instance, Program, untagged_name},
    staging_error,
    ty::{Field, FunctionType, HMIRTypeID, HMIRTypeKind, NominalKey, TypeTable},
    value::StaticValue,
};

pub(super) fn exec_type_op(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    op: &HMIRTypeOp,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let types = |cx: &mut Program<'_>, frame: &mut EvalFrame, exprs: &[HMIRExprID]| {
        exprs
            .iter()
            .map(|expr| eval_type(cx, frame, *expr))
            .collect::<CXResult<Vec<_>>>()
    };
    Ok(match op {
        HMIRTypeOp::Member { ty, name } => {
            let ty = eval_type(cx, frame, *ty)?;
            let Some((_, field)) = cx.types().field(ty, name.as_str()) else {
                return Err(staging_error(
                    span,
                    &typecheck::UNKNOWN_MEMBER,
                    (cx.types().display(ty), name.to_string()),
                ));
            };
            StaticValue::Type(field.ty())
        }
        HMIRTypeOp::TypeOf(operand) => StaticValue::Type(inspect(cx, frame, *operand, None)?),
        HMIRTypeOp::PointerInner(operand) | HMIRTypeOp::ReferenceInner(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            let (inner, expected) = match op {
                HMIRTypeOp::PointerInner(_) => (cx.types().pointer_inner(ty), "a pointer type"),
                HMIRTypeOp::ReferenceInner(_) => {
                    (cx.types().reference_inner(ty), "a reference type")
                }
                _ => unreachable!(),
            };
            let inner = inner.ok_or_else(|| {
                staging_error(
                    span,
                    &typecheck::TYPE_REQUIREMENT,
                    (
                        op.path().to_string(),
                        expected.into(),
                        Some(format!("'{}'", cx.types().display(ty))),
                    ),
                )
            })?;
            StaticValue::Type(inner)
        }
        HMIRTypeOp::Decay(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            StaticValue::Type(decay(cx.types_mut(), ty))
        }
        HMIRTypeOp::Pointer(inner) => {
            let inner = eval_pointee(cx, frame, *inner)?;
            if cx.types().is_unreachable(inner) {
                return Err(staging_error(
                    span,
                    &typecheck::INVALID_POINTEE,
                    cx.types().display(inner),
                ));
            }
            StaticValue::Type(cx.types_mut().pointer_to(inner))
        }
        HMIRTypeOp::Reference(inner) => {
            let inner = eval_type(cx, frame, *inner)?;
            StaticValue::Type(cx.types_mut().reference_to(inner))
        }
        HMIRTypeOp::Const(inner) => {
            let inner = eval_type(cx, frame, *inner)?;
            StaticValue::Type(cx.types_mut().const_of(inner))
        }
        HMIRTypeOp::Array { element, length } => {
            let element = eval_type(cx, frame, *element)?;
            let length = match length {
                Some(length) => {
                    let eager = std::mem::replace(&mut cx.deferral_mut().eager, true);
                    let value = eval(cx, frame, *length, None);
                    cx.deferral_mut().eager = eager;
                    let length = value?.as_int().ok_or_else(|| {
                        staging_error(
                            span,
                            &mir::EXPECTED_CONSTANT,
                            ("array length".into(), "integer".into()),
                        )
                    })?;
                    Some(length.max(0) as u64)
                }
                None => None,
            };
            StaticValue::Type(cx.types_mut().array_of(element, length))
        }
        HMIRTypeOp::Function {
            params,
            ret,
            variadic,
        } => {
            let params = types(cx, frame, params)?
                .into_iter()
                .filter(|param| !cx.types().is_void(*param))
                .collect();
            let ret = eval_type(cx, frame, *ret)?;
            StaticValue::Type(
                cx.types_mut()
                    .intern(HMIRTypeKind::Function(FunctionType::new(
                        params, ret, *variadic,
                    ))),
            )
        }
        HMIRTypeOp::Expr { params, result } => {
            let params = types(cx, frame, params)?;
            let result = eval_type(cx, frame, *result)?;
            StaticValue::Type(
                cx.types_mut()
                    .intern(HMIRTypeKind::StagedExpr { params, result }),
            )
        }
        HMIRTypeOp::Aggregate { .. } => unreachable!("aggregate types are evaluated with their id"),
        // A string literal is the array of its bytes and their terminator, which is not a type
        // the literal has anywhere else
        HMIRTypeOp::SizeOf(operand)
            if let HMIRExprKind::Constant(HMIRConstant::Str(literal)) =
                frame.body().expr(*operand).kind() =>
        {
            let size = literal.len() as i128 + 1;
            let size_type = cx.types_mut().size_type();
            StaticValue::int(size, size_type)
        }
        HMIRTypeOp::SizeOf(operand) | HMIRTypeOp::AlignOf(operand) => {
            let ty = inspect(cx, frame, *operand, None)?;
            let ty = match cx.types().kind(ty) {
                HMIRTypeKind::Type => eval_type(cx, frame, *operand)?,
                _ => ty,
            };
            let size = if matches!(op, HMIRTypeOp::SizeOf(_)) {
                cx.types_mut().size_of(ty, span)?
            } else {
                cx.types_mut().align_of(ty, span)?
            };
            let size_type = cx.types_mut().size_type();
            StaticValue::int(size as i128, size_type)
        }
        HMIRTypeOp::OffsetOf { ty, member } => {
            let mut current = eval_type(cx, frame, *ty)?;
            let mut offset = 0;
            for step in member {
                match step {
                    HMIRMemberStep::Field(name) => {
                        let path = cx.types().member_path(current, name.as_str());
                        let path = path.ok_or_else(|| {
                            staging_error(
                                span,
                                &typecheck::UNKNOWN_MEMBER,
                                (cx.types().display(current), name.to_string()),
                            )
                        })?;
                        for index in path {
                            let nominal = cx.types().nominal_of(current);
                            let field = &nominal
                                .expect("member path is through aggregates")
                                .fields()[index];
                            if field.bit_width().is_some() {
                                return Err(staging_error(
                                    span,
                                    &typecheck::INVALID_FORM,
                                    ("offsetof".into(), "bit-field member".into()),
                                ));
                            }
                            let field_ty = field.ty();
                            let aggregate = cx.types_mut().mir(current, span)?;
                            let layout =
                                calculate_field_layout(cx.types().mir_types(), aggregate, index);
                            offset +=
                                layout.expect("aggregate lays out its fields").offset() as u64;
                            current = field_ty;
                        }
                    }
                    HMIRMemberStep::Index(index) => {
                        let Some(element) = cx.types().array_inner(current) else {
                            return Err(staging_error(
                                span,
                                &typecheck::TYPE_MISMATCH,
                                (
                                    "offsetof subscript".into(),
                                    "an array member".into(),
                                    format!("'{}'", cx.types().display(current)),
                                ),
                            ));
                        };
                        let index = eval(cx, frame, *index, None)?.as_int().ok_or_else(|| {
                            staging_error(
                                span,
                                &mir::EXPECTED_CONSTANT,
                                ("offsetof subscript".into(), "integer".into()),
                            )
                        })?;
                        offset += index as u64 * cx.types_mut().size_of(element, span)?;
                        current = element;
                    }
                }
            }
            let size_type = cx.types_mut().size_type();
            StaticValue::int(offset as i128, size_type)
        }
        HMIRTypeOp::IsInt(operand)
        | HMIRTypeOp::IsFloat(operand)
        | HMIRTypeOp::IsPointer(operand)
        | HMIRTypeOp::IsSigned(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            let kind = cx.types().kind(ty);
            let result = match op {
                HMIRTypeOp::IsInt(_) => matches!(kind, HMIRTypeKind::Int { .. }),
                HMIRTypeOp::IsFloat(_) => matches!(kind, HMIRTypeKind::Float { .. }),
                HMIRTypeOp::IsPointer(_) => cx.types().is_pointer(ty),
                _ => cx.types().is_signed(ty),
            };
            StaticValue::bool(result, cx.types_mut())
        }
        HMIRTypeOp::Equal(left, right) => {
            let left = eval_type(cx, frame, *left)?;
            let right = eval_type(cx, frame, *right)?;
            StaticValue::bool(left == right, cx.types_mut())
        }
    })
}

pub(super) fn decay(types: &mut TypeTable, ty: HMIRTypeID) -> HMIRTypeID {
    let ty = types.reference_inner(ty).unwrap_or(ty);
    types.decayed(ty)
}

// A named aggregate pointed to while another aggregate is being defined only gets its nominal,
// and is defined once nothing is in progress. Defining it on the spot would make mutually
// referential aggregates depend on which of them is reached first.
// Expressions inside a type (array lengths) may look through the pointer, so they opt out.
fn eval_pointee(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
) -> CXResult<HMIRTypeID> {
    let deferral = cx.deferral_mut();
    if deferral.defining > 0
        && !deferral.eager
        && let Some(ty) = defer_named_type(cx, frame, id)?
    {
        return Ok(ty);
    }
    eval_type(cx, frame, id)
}

fn defer_named_type(
    cx: &mut Program<'_>,
    frame: &EvalFrame,
    id: HMIRExprID,
) -> CXResult<Option<HMIRTypeID>> {
    let expr = frame.body().expr(id);
    let mut def = match expr.kind() {
        HMIRExprKind::Native(HMIROp::Type(HMIRTypeOp::Const(inner))) => {
            let inner = defer_named_type(cx, frame, *inner)?;
            return Ok(inner.map(|inner| cx.types_mut().const_of(inner)));
        }
        HMIRExprKind::Def(def) => def.clone(),
        _ => return Ok(None),
    };
    let mut unit = frame.def().unit();
    let mut seen = Vec::new();
    loop {
        if matches!(def, HMIRDefRef::Candidates(_)) {
            return Ok(None);
        }
        let key = cx.resolve(unit, &def, expr.span())?;
        let instance = (key, Vec::new());
        if seen.contains(&key)
            || cx.generated_mut().contains_key(&instance)
            || cx.active_mut().contains(&instance)
        {
            return Ok(None);
        }
        seen.push(key);
        let source = cx.unit(key.unit());
        let HMIRDefKind::ComptimeGlobal(global) = source.def(key.def()).kind() else {
            return Ok(None);
        };
        let root = global.initializer();
        match global.body().expr(root).kind() {
            HMIRExprKind::Def(alias) => (unit, def) = (key.unit(), alias.clone()),
            HMIRExprKind::Native(HMIROp::Type(HMIRTypeOp::Aggregate {
                kind,
                semantics,
                unsafe_move,
                traits_of: None,
                ..
            })) => {
                let name = nominal_name(cx, &instance);
                let nominal = NominalKey::new(key, Vec::new(), root);
                let (ty, pending) =
                    cx.types_mut()
                        .intern_nominal(nominal, name, *kind, *semantics, *unsafe_move);
                if pending {
                    cx.deferral_mut().pending.push(key);
                }
                return Ok(Some(ty));
            }
            _ => return Ok(None),
        }
    }
}

pub(super) fn eval_aggregate_type(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    kind: HMIRAggregateKind,
    (mut semantics, mut unsafe_move): (HMIRMoveSemantics, bool),
    traits_of: Option<HMIRExprID>,
    fields: &[HMIRFieldDef],
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let owner = frame.owner().clone();
    let key = NominalKey::new(owner.0, owner.1.clone(), id);
    let name = nominal_name(cx, &owner);
    if let Some(traits_of) = traits_of {
        let source = eval_type(cx, frame, traits_of)?;
        let (inherited, inherited_unsafe_move) = cx.types().owned_traits(source);
        semantics = semantics.max(inherited);
        unsafe_move |= inherited_unsafe_move;
    }
    let (ty, pending) = cx
        .types_mut()
        .intern_nominal(key, name, kind, semantics, unsafe_move);
    if !pending {
        return Ok(StaticValue::Type(ty));
    }
    if cx.active_mut().contains(&*owner) && !cx.generated_mut().contains_key(&*owner) {
        cx.generated_mut()
            .insert((*owner).clone(), StaticValue::Type(ty));
    }
    cx.deferral_mut().defining += 1;
    let defined = eval_fields(cx, frame, kind, (semantics, unsafe_move), fields, span);
    cx.deferral_mut().defining -= 1;
    cx.types_mut().define_nominal(ty, defined?);
    if cx.deferral_mut().defining == 0 {
        while let Some(deferred) = cx.deferral_mut().pending.pop() {
            def_value(cx, deferred, span)?;
        }
    }
    Ok(StaticValue::Type(ty))
}

fn eval_fields(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    kind: HMIRAggregateKind,
    (semantics, unsafe_move): (HMIRMoveSemantics, bool),
    fields: &[HMIRFieldDef],
    span: &TokenRange,
) -> CXResult<Vec<Field>> {
    let mut defined = Vec::with_capacity(fields.len());
    for field in fields {
        let field_ty = eval_type(cx, frame, field.ty())?;
        if cx.types().is_void(field_ty) && kind != HMIRAggregateKind::TaggedUnion {
            return Err(staging_error(span, &typecheck::VOID_FIELD, ()));
        }
        if let Some(problem) = cx.types().object_problem(field_ty) {
            let name = field
                .name()
                .map(ToString::to_string)
                .unwrap_or_else(|| "<anonymous>".into());
            return Err(staging_error(
                frame.body().expr(field.ty()).span(),
                &typecheck::INVALID_OBJECT_TYPE,
                (format!("field '{name}'"), problem.into()),
            ));
        }
        let (field_semantics, field_unsafe_move) = cx.types().owned_traits(field_ty);
        let required = if field_semantics > semantics {
            Some(match field_semantics {
                HMIRMoveSemantics::Nodrop => "@nodrop",
                _ => "@nocopy",
            })
        } else if field_unsafe_move && !unsafe_move {
            Some("@unsafe_move")
        } else {
            None
        };
        if let Some(required) = required {
            let name = field
                .name()
                .map(ToString::to_string)
                .unwrap_or_else(|| "<anonymous>".into());
            return Err(staging_error(
                span,
                &typecheck::FIELD_TRAIT,
                (name, required.to_string(), required.to_string()),
            ));
        }
        defined.push(Field::new(
            field.name().cloned(),
            field_ty,
            field.bit_width(),
        ));
    }
    Ok(defined)
}

fn nominal_name(cx: &mut Program<'_>, owner: &Instance) -> String {
    let name = cx.def_name(owner.0);
    let base = untagged_name(&name.name).to_string();
    if owner.1.is_empty() {
        return base;
    }
    let args = owner
        .1
        .iter()
        .map(|arg| match arg {
            StaticValue::Type(ty) => cx.types().display(*ty),
            StaticValue::Int { value, .. } => value.to_string(),
            _ => "_".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("{base}<{args}>")
}
