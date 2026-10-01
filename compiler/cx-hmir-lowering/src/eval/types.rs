use cx_hmir::{HMIRAggregateKind, HMIRExprID, HMIRFieldDef, HMIRMoveSemantics, HMIRTypeOp};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, eval, eval_type},
    function::inspect::inspect,
    program::{Instance, Program, untagged_name},
    staging_error,
    ty::{Field, FunctionType, NominalKey, TypeID, TypeKind, TypeTable},
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
        HMIRTypeOp::TypeOf(operand) => StaticValue::Type(inspect(cx, frame, *operand, None)?),
        HMIRTypeOp::PointerInner(operand) | HMIRTypeOp::ReferenceInner(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            let inner = match op {
                HMIRTypeOp::PointerInner(_) => cx.types().pointer_inner(ty),
                HMIRTypeOp::ReferenceInner(_) => cx.types().reference_inner(ty),
                _ => unreachable!(),
            }
            .ok_or_else(|| {
                staging_error(
                    span,
                    format!("{} is invalid for '{}'", op.path(), cx.types().display(ty)),
                )
            })?;
            StaticValue::Type(inner)
        }
        HMIRTypeOp::Decay(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            StaticValue::Type(decay(cx.types_mut(), ty))
        }
        HMIRTypeOp::Pointer(inner) => {
            let inner = eval_type(cx, frame, *inner)?;
            StaticValue::Type(cx.types_mut().pointer_to(inner))
        }
        HMIRTypeOp::Reference(inner) => {
            let inner = eval_type(cx, frame, *inner)?;
            StaticValue::Type(cx.types_mut().reference_to(inner))
        }
        HMIRTypeOp::Array { element, length } => {
            let element = eval_type(cx, frame, *element)?;
            let length = match length {
                Some(length) => {
                    let value = eval(cx, frame, *length, None)?;
                    let length = value.as_int().ok_or_else(|| {
                        staging_error(span, "array length is not a compile-time integer".into())
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
            StaticValue::Type(cx.types_mut().intern(TypeKind::Function(FunctionType::new(
                params, ret, *variadic,
            ))))
        }
        HMIRTypeOp::Expr { params, result } => {
            let params = types(cx, frame, params)?;
            let result = eval_type(cx, frame, *result)?;
            StaticValue::Type(cx.types_mut().intern(TypeKind::Expr { params, result }))
        }
        HMIRTypeOp::Aggregate { .. } => unreachable!("aggregate types are evaluated with their id"),
        HMIRTypeOp::SizeOf(operand) | HMIRTypeOp::AlignOf(operand) => {
            let ty = inspect(cx, frame, *operand, None)?;
            let ty = match cx.types().kind(ty) {
                TypeKind::Type => eval_type(cx, frame, *operand)?,
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
        HMIRTypeOp::IsInt(operand)
        | HMIRTypeOp::IsFloat(operand)
        | HMIRTypeOp::IsPointer(operand)
        | HMIRTypeOp::IsSigned(operand) => {
            let ty = eval_type(cx, frame, *operand)?;
            let kind = cx.types().kind(ty);
            let result = match op {
                HMIRTypeOp::IsInt(_) => matches!(kind, TypeKind::Int { .. }),
                HMIRTypeOp::IsFloat(_) => matches!(kind, TypeKind::Float { .. }),
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

pub(super) fn decay(types: &mut TypeTable, ty: TypeID) -> TypeID {
    let ty = types.reference_inner(ty).unwrap_or(ty);
    if let Some(element) = types.array_inner(ty) {
        return types.pointer_to(element);
    }
    if types.is_function(ty) {
        return types.pointer_to(ty);
    }
    if matches!(types.kind(ty), TypeKind::Str) {
        return types.char_pointer();
    }
    ty
}

pub(super) fn eval_aggregate_type(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    kind: HMIRAggregateKind,
    semantics: HMIRMoveSemantics,
    fields: &[HMIRFieldDef],
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let owner = frame.owner().clone();
    let key = NominalKey::new(owner.0, owner.1.clone(), id);
    let name = nominal_name(cx, &owner);
    let (ty, pending) = cx.types_mut().intern_nominal(key, name, kind, semantics);
    if !pending {
        return Ok(StaticValue::Type(ty));
    }
    if cx.active_mut().contains(&*owner) && !cx.generated_mut().contains_key(&*owner) {
        cx.generated_mut()
            .insert((*owner).clone(), StaticValue::Type(ty));
    }
    let mut defined = Vec::with_capacity(fields.len());
    for field in fields {
        let field_ty = eval_type(cx, frame, field.ty())?;
        if cx.types().is_void(field_ty) && kind != HMIRAggregateKind::TaggedUnion {
            return Err(staging_error(span, "aggregate field of type void".into()));
        }
        defined.push(Field::new(
            field.name().cloned(),
            field_ty,
            field.bit_width(),
        ));
    }
    cx.types_mut().define_nominal(ty, defined);
    Ok(StaticValue::Type(ty))
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
