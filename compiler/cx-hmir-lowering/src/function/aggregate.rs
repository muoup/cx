use cx_hmir::{
    HMIRAggregateKind, HMIRAggregateOp, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRLocalID,
    HMIRNativeOp, HMIRPattern, HMIRTypeOp,
};
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRConstant, MIRFloatIntrinsic,
    MIRInstructionKind, MIRIntIntrinsic, MIRIntType, MIRInternalIntrinsic, MIRRegisterID,
    MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
    ty::layout::{MIRFieldLayout, calculate_field_layout},
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    function::{
        Expect, FunctionLowering, LowerResult, Operand, OperandKind, PatternBinding,
        coerce::{lower_convert, lower_nonnull_pointer},
        expr::lower_expr,
        lower_eval,
        operand::{
            lower_auto_deref, lower_copy, lower_int_constant, lower_spill, lower_store, lower_value,
        },
        ops::lower_pointer_offset,
        promote::{lower_decay, lower_promote},
    },
    module::{member_type, variant_index},
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

pub(super) fn lower_aggregate(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIRAggregateOp,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIRAggregateOp::Member { base, name } => {
            let base = lower_expr(cx, frame, base, Expect::Any)?;
            lower_member(cx, base, &name, span)
        }
        HMIRAggregateOp::Index { base, index } => {
            let base = lower_expr(cx, frame, base, Expect::Any)?;
            let base = lower_promote(cx, base, span)?;
            let index = lower_expr(cx, frame, index, Expect::Any)?;
            let index = lower_promote(cx, index, span)?;
            lower_index(cx, base, index, span)
        }
        HMIRAggregateOp::Initialize { ty, fields } => {
            lower_initialize(cx, frame, ty, &fields, expect, span)
        }
        HMIRAggregateOp::Is { value, pattern } => lower_is(cx, frame, value, pattern, span),
        HMIRAggregateOp::Unpack { value, bindings } => {
            lower_unpack(cx, frame, value, &bindings, span)
        }
    }
}

fn lower_unpack(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    value: HMIRExprID,
    bindings: &[(CXIdent, HMIRLocalID)],
    span: &TokenRange,
) -> LowerResult<Operand> {
    let operand = lower_expr(cx, frame, value, Expect::Any)?;
    let ty = operand.ty();
    let types = cx.program.types();
    let fields = match types.nominal_of(ty) {
        Some(nominal) if !operand.is_lvalue() && !types.is_reference(ty) => nominal.fields(),
        _ => {
            return cx.error(
                span,
                format!(
                    "@unpack takes an owned structure, found '{}'; move the value into it",
                    types.display(ty)
                ),
            );
        }
    };
    let unbound = fields.iter().find(|field| {
        types.is_nodrop(field.ty())
            && !bindings
                .iter()
                .any(|(name, _)| field.name().is_some_and(|field| field == name))
    });
    if let Some(field) = unbound {
        return cx.error(
            span,
            format!(
                "@unpack of '{}' must bind @nodrop field '{}'",
                types.display(ty),
                field.name().map(CXIdent::as_str).unwrap_or("_")
            ),
        );
    }

    let shell = cx.place(ty, None, span)?;
    let value = lower_value(cx, operand, span)?;
    lower_store(cx, MIRTarget::Place(shell), value, ty, None, span)?;
    cx.initialize(shell, span);
    let mut fields = Vec::with_capacity(bindings.len());
    for (name, local) in bindings {
        let field = lower_member(cx, Operand::place(shell, ty), name, span)?;
        let value = lower_copy(cx, &field, span)?;
        fields.push((*local, value, field.ty()));
    }
    cx.invalidate(MIRBindable::Place(shell), MIRInvalidationKind::Move, span);
    for (local, value, ty) in fields {
        let name = cx.frames[frame].body().local(local).name().cloned();
        let place = cx.place(ty, name, span)?;
        lower_store(cx, MIRTarget::Place(place), value, ty, None, span)?;
        cx.initialize(place, span);
        cx.bind(frame, local, Operand::place(place, ty));
    }
    Ok(Operand::unit(cx.program.types_mut()))
}

// A pointer used as an aggregate is read through
pub(super) fn lower_deref_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Some(inner) = cx.program.types().pointer_inner(operand.ty()) else {
        return Ok(operand);
    };
    let origin = operand.pointee_origin();
    let pointer = lower_nonnull_pointer(cx, operand, span)?;
    let ty = cx.program.types_mut().reference_to(inner);
    let out = cx.register(ty, span)?;
    let target_ty = cx.mir(ty, span)?;
    cx.intrinsic(
        MIRInternalIntrinsic::Bitcast {
            out: MIRTarget::Register(out),
            value: pointer,
            target_ty,
        },
        span,
    );
    Ok(Operand::reference(out, inner, origin))
}

pub(super) fn lower_member(
    cx: &mut FunctionLowering<'_, '_>,
    base: Operand,
    name: &CXIdent,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let base = if cx.program.types().is_array(base.ty()) {
        lower_decay(cx, base, span)?
    } else {
        base
    };
    let base = lower_deref_pointer(cx, base, span)?;
    let base = lower_spill(cx, base, span)?;
    let Some((index, field)) = cx.program.types().field(base.ty(), name.as_str()) else {
        return cx.error(
            span,
            format!(
                "'{}' has no member '{name}'",
                cx.program.types().display(base.ty())
            ),
        );
    };
    let field_ty = field.ty();
    let struct_ty = cx.mir(base.ty(), span)?;
    let bitfield = match calculate_field_layout(cx.program.types().mir_types(), struct_ty, index) {
        Some(MIRFieldLayout::Bitfield {
            bit_offset,
            bit_width,
            ..
        }) => Some(MIRBitfieldAccess {
            bit_offset,
            bit_width,
            signed: cx.program.types().is_signed(field_ty),
        }),
        _ => None,
    };
    let reference = cx.program.types_mut().reference_to(field_ty);
    let out = cx.register(reference, span)?;
    let address = base.address().expect("spilled operand is addressable");
    cx.intrinsic(
        MIRAggregateIntrinsic::StructField {
            out: MIRTarget::Register(out),
            base: address,
            field: index,
            struct_ty,
        },
        span,
    );
    Ok(Operand::new(
        OperandKind::Ref {
            reg: out,
            bitfield,
            origin: base.origin(),
        },
        field_ty,
    ))
}

fn lower_index(
    cx: &mut FunctionLowering<'_, '_>,
    base: Operand,
    index: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let types = cx.program.types();
    let (pointer, index, element) = match (types.kind(base.ty()), types.kind(index.ty())) {
        (TypeKind::Pointer(element), TypeKind::Int { .. }) => (base, index, *element),
        (TypeKind::Int { .. }, TypeKind::Pointer(element)) => (index, base, *element),
        _ => {
            return cx.error(span, "indexing requires a pointer and an integer");
        }
    };
    let ty = pointer.ty();
    let address = lower_pointer_offset(cx, pointer, index, element, false, ty, span)?;
    lower_deref_pointer(cx, address, span)
}

fn lower_initialize(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    ty: HMIRExprID,
    fields: &[(Option<CXIdent>, HMIRExprID)],
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let ty = lower_initializer_type(cx, frame, ty, expect, span)?;
    let mut values = Vec::with_capacity(fields.len());
    let ty = match cx.program.types().kind(ty).clone() {
        TypeKind::Array { element, length } => {
            if let Some(length) = length
                && fields.len() as u64 > length
            {
                return cx.error(span, "too many initializers for the array");
            }
            for (index, (_, value)) in fields.iter().enumerate() {
                let value = lower_field_value(cx, frame, *value, element, span)?;
                values.push((index, value));
            }
            cx.program.types_mut().intern(TypeKind::Array {
                element,
                length: Some(length.unwrap_or(fields.len() as u64)),
            })
        }
        TypeKind::Nominal(_) => {
            let tagged = cx
                .program
                .types()
                .nominal_of(ty)
                .is_some_and(|nominal| nominal.kind() == HMIRAggregateKind::TaggedUnion);
            let mut next = 0;
            for (name, value) in fields {
                let (index, field_ty) = match name {
                    Some(name) => match cx.program.types().field(ty, name.as_str()) {
                        Some((index, field)) => (index, field.ty()),
                        None => {
                            return cx.error(
                                span,
                                format!(
                                    "'{}' has no member '{name}'",
                                    cx.program.types().display(ty)
                                ),
                            );
                        }
                    },
                    None => (next, member_type(cx.program, ty, next, span)?),
                };
                next = index + 1;
                let value = if tagged && cx.program.types().is_void(field_ty) {
                    lower_expr(cx, frame, *value, Expect::Discard)?;
                    MIRValue::Constant(MIRConstant::Unit)
                } else {
                    lower_field_value(cx, frame, *value, field_ty, span)?
                };
                values.push((index, value));
            }
            ty
        }
        _ if let [(Some(name), _)] = fields => {
            return cx.error(
                span,
                format!(
                    "'{}' has no member '{name}'",
                    cx.program.types().display(ty)
                ),
            );
        }
        _ if fields.len() == 1 => {
            let operand = lower_expr(cx, frame, fields[0].1, Expect::Type(ty))?;
            return lower_convert(cx, operand, ty, span);
        }
        _ => {
            return cx.error(
                span,
                format!(
                    "'{}' cannot be initialized from a list",
                    cx.program.types().display(ty)
                ),
            );
        }
    };
    let out = cx.register(ty, span)?;
    let mir = cx.mir(ty, span)?;
    cx.intrinsic(
        MIRAggregateIntrinsic::AggregateInit {
            out: MIRTarget::Register(out),
            ty: mir,
            fields: values,
        },
        span,
    );
    Ok(Operand::register(out, ty))
}

fn lower_field_value(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    value: HMIRExprID,
    ty: TypeID,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let operand = lower_expr(cx, frame, value, Expect::Type(ty))?;
    let operand = lower_convert(cx, operand, ty, span)?;
    lower_value(cx, operand, span)
}

// The initialized type; a hole takes the expected type, and a bare generator is applied
// to the arguments of the expected type it generates
fn lower_initializer_type(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    ty: HMIRExprID,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<TypeID> {
    let expected = expect
        .ty()
        .map(|expected| match cx.program.types().kind(expected) {
            TypeKind::Reference(inner) => *inner,
            _ => expected,
        });
    if matches!(cx.kind(frame, ty), HMIRExprKind::Hole(_)) {
        return match expected {
            Some(expected) => Ok(expected),
            None => cx.error(span, "cannot infer the initialized type"),
        };
    }
    match lower_eval(cx, frame, ty, Expect::Any)? {
        StaticValue::Type(ty) => Ok(ty),
        StaticValue::Function { def, .. }
            if expected.is_some_and(|expected| {
                cx.program
                    .types()
                    .nominal_of(expected)
                    .is_some_and(|nominal| nominal.key().owner() == def)
            }) =>
        {
            Ok(expected.expect("checked above"))
        }
        other => cx.error(
            span,
            format!("expected a type to initialize, found {}", other.describe()),
        ),
    }
}

fn lower_is(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    value: HMIRExprID,
    pattern: HMIRPattern,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let subject = lower_expr(cx, frame, value, Expect::Any)?;
    let subject = lower_pattern_subject(cx, subject, span)?;
    let binds = match &pattern {
        HMIRPattern::Binding(_) => true,
        HMIRPattern::Variant { inner, .. } => inner.is_some(),
        _ => false,
    };
    let owned = binds && !subject.is_lvalue();
    let bool = cx.program.types_mut().bool();
    let out = cx.register(bool, span)?;
    let target = MIRTarget::Register(out);
    let subject = match &pattern {
        HMIRPattern::Binding(_) => {
            let subject = lower_spill(cx, subject, span)?;
            let truth = lower_int_constant(cx, 1, bool);
            lower_store(cx, target, truth, bool, None, span)?;
            subject
        }
        HMIRPattern::Integer(expected) => {
            let ty = subject.ty();
            let expected = lower_int_constant(cx, *expected as i128, ty);
            let value = lower_value(cx, subject.clone(), span)?;
            cx.intrinsic(
                MIRIntIntrinsic::Eq {
                    out: target,
                    lhs: value,
                    rhs: expected,
                },
                span,
            );
            subject
        }
        HMIRPattern::Float(expected) => {
            let TypeKind::Float { width } = cx.program.types().kind(subject.ty()).clone() else {
                return cx.error(span, "floating pattern on a non-floating value");
            };
            let value = lower_value(cx, subject.clone(), span)?;
            cx.intrinsic(
                MIRFloatIntrinsic::Eq {
                    out: target,
                    lhs: value,
                    rhs: MIRValue::Constant(MIRConstant::Float {
                        value: *expected,
                        ty: TypeTable::mir_float(width),
                    }),
                },
                span,
            );
            subject
        }
        HMIRPattern::Value(expected) => {
            let ty = subject.ty();
            let expected = lower_eval(cx, frame, *expected, Expect::Type(ty))?;
            let Some(expected) = expected.as_int() else {
                return cx.error(
                    span,
                    "value pattern is not an integer constant; bind the value with 'auto name'",
                );
            };
            let expected = lower_int_constant(cx, expected, ty);
            let value = lower_value(cx, subject.clone(), span)?;
            cx.intrinsic(
                MIRIntIntrinsic::Eq {
                    out: target,
                    lhs: value,
                    rhs: expected,
                },
                span,
            );
            subject
        }
        HMIRPattern::Variant { name, .. } => {
            let index = variant_index(cx.program, subject.ty(), name, span)?;
            let subject = lower_spill(cx, subject, span)?;
            let tag = lower_sum_index(cx, &subject, span)?;
            cx.intrinsic(
                MIRIntIntrinsic::Eq {
                    out: target,
                    lhs: tag,
                    rhs: MIRValue::Constant(MIRConstant::Integer {
                        ty: MIRIntType::I8,
                        value: index as i128,
                    }),
                },
                span,
            );
            subject
        }
    };
    if binds {
        cx.pattern_bindings.push(PatternBinding {
            frame,
            subject,
            owned,
            pattern,
        });
    }
    Ok(Operand::register(out, bool))
}

pub(super) fn lower_pattern_subject(
    cx: &mut FunctionLowering<'_, '_>,
    subject: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let subject = lower_auto_deref(cx, subject, span)?;
    if cx.program.types().is_pointer(subject.ty()) {
        return cx.error(
            span,
            "pattern subject is a pointer; dereference it explicitly",
        );
    }
    Ok(subject)
}

pub(super) fn lower_sum_index(
    cx: &mut FunctionLowering<'_, '_>,
    subject: &Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let tag = cx.program.types_mut().int(HMIRIntWidth::I8, false);
    let out = cx.register(tag, span)?;
    let sum_ty = cx.mir(subject.ty(), span)?;
    let value = subject.address().expect("matched subjects are addressable");
    cx.intrinsic(
        MIRAggregateIntrinsic::SumIndex {
            out: MIRTarget::Register(out),
            value,
            sum_ty,
        },
        span,
    );
    Ok(MIRValue::Register(out))
}

// Binds a matched pattern's locals; payloads of owned subjects are lifted into new places
pub(super) fn lower_bind_pattern(
    cx: &mut FunctionLowering<'_, '_>,
    binding: PatternBinding,
    span: &TokenRange,
) -> LowerResult<()> {
    let PatternBinding {
        frame,
        subject,
        owned,
        pattern,
    } = binding;
    let span = span.clone();
    match pattern {
        HMIRPattern::Binding(local) => {
            let bound = if owned || binds_by_reference(cx, frame, local) {
                subject
            } else {
                lower_copy_binding(cx, &subject, frame, local, &span)?
            };
            cx.bind(frame, local, bound);
        }
        HMIRPattern::Variant {
            name,
            inner: Some(local),
            ..
        } => {
            let index = variant_index(cx.program, subject.ty(), &name, &span)?;
            let payload = member_type(cx.program, subject.ty(), index, &span)?;
            if cx.program.types().is_void(payload) {
                lower_consume_subject(cx, &subject, owned, &span);
                let unit = Operand::unit(cx.program.types_mut());
                cx.bind(frame, local, unit);
                return Ok(());
            }
            let reference_ty = cx.program.types_mut().reference_to(payload);
            let reference = cx.register(reference_ty, &span)?;
            let sum_ty = cx.mir(subject.ty(), &span)?;
            cx.intrinsic(
                MIRAggregateIntrinsic::SumVariant {
                    out: MIRTarget::Register(reference),
                    base: subject.address().expect("matched subjects are addressable"),
                    variant: index,
                    sum_ty,
                },
                &span,
            );
            let borrowed = Operand::reference(reference, payload, None);
            let bound = match subject.origin().filter(|_| owned) {
                Some(origin) => {
                    lower_lift_payload(cx, reference, origin, payload, frame, local, &span)?
                }
                None if owned || binds_by_reference(cx, frame, local) => borrowed,
                None => lower_copy_binding(cx, &borrowed, frame, local, &span)?,
            };
            cx.bind(frame, local, bound);
        }
        HMIRPattern::Variant {
            name, inner: None, ..
        } => {
            let index = variant_index(cx.program, subject.ty(), &name, &span)?;
            let payload = member_type(cx.program, subject.ty(), index, &span)?;
            if cx.program.types().is_void(payload) {
                lower_consume_subject(cx, &subject, owned, &span);
            }
        }
        _ => {}
    }
    Ok(())
}

fn binds_by_reference(cx: &FunctionLowering<'_, '_>, frame: usize, local: HMIRLocalID) -> bool {
    let ty = cx.frames[frame].body().local(local).ty();
    matches!(
        cx.kind(frame, ty),
        HMIRExprKind::Native(HMIRNativeOp::Type(HMIRTypeOp::Reference(_)))
    )
}

// 'auto x' on a subject that stays in use copies the matched value; one that cannot be copied
// has to be borrowed or matched on a moved subject
fn lower_copy_binding(
    cx: &mut FunctionLowering<'_, '_>,
    source: &Operand,
    frame: usize,
    local: HMIRLocalID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let ty = source.ty();
    let name = cx.frames[frame].body().local(local).name().cloned();
    if !cx.program.types().is_pod(ty) {
        let binding = name.map_or_else(|| "auto".to_string(), |name| format!("auto {name}"));
        return cx.error(
            span,
            format!(
                "'{binding}' would move '{}' out of a value that is still in use; \
                 borrow it with 'auto&' or match on a moved value",
                cx.program.types().display(ty)
            ),
        );
    }
    let value = lower_copy(cx, source, span)?;
    let place = cx.place(ty, name, span)?;
    lower_store(cx, MIRTarget::Place(place), value, ty, None, span)?;
    cx.initialize(place, span);
    Ok(Operand::place(place, ty))
}

// An owned subject matched on a payload-free variant has nothing left to give away
fn lower_consume_subject(
    cx: &mut FunctionLowering<'_, '_>,
    subject: &Operand,
    owned: bool,
    span: &TokenRange,
) {
    if let Some(origin) = subject.origin().filter(|_| owned) {
        cx.invalidate(MIRBindable::Place(origin), MIRInvalidationKind::Move, span);
    }
}

fn lower_lift_payload(
    cx: &mut FunctionLowering<'_, '_>,
    reference: MIRRegisterID,
    origin: cx_mir::MIRPlaceID,
    payload: TypeID,
    frame: usize,
    local: HMIRLocalID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let out = cx.register(payload, span)?;
    cx.emit(
        MIRInstructionKind::Lift {
            out,
            source: MIRBindable::Register(reference),
            origin: MIRBindable::Place(origin),
        },
        span,
    );
    cx.invalidate(MIRBindable::Place(origin), MIRInvalidationKind::Move, span);
    let name = cx.frames[frame].body().local(local).name().cloned();
    let place = cx.place(payload, name, span)?;
    lower_store(
        cx,
        MIRTarget::Place(place),
        MIRValue::Register(out),
        payload,
        None,
        span,
    )?;
    cx.invalidate(MIRBindable::Register(out), MIRInvalidationKind::Move, span);
    cx.initialize(place, span);
    Ok(Operand::place(place, payload))
}

pub(super) fn lower_address_of(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    inner: HMIRExprID,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let operand = lower_expr(cx, frame, inner, Expect::Any)?;
    let decays = match cx.program.types().kind(operand.ty()).clone() {
        TypeKind::Function(_) | TypeKind::Str => true,
        TypeKind::Array { element, .. } => expect
            .ty()
            .and_then(|expected| cx.program.types().pointer_inner(expected))
            .is_some_and(|pointee| pointee == element),
        _ => false,
    };
    if decays {
        return lower_decay(cx, operand, span);
    }
    lower_address_of_operand(cx, operand, span)
}

pub(super) fn lower_address_of_operand(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if operand.bitfield().is_some() {
        return cx.error(span, "cannot take the address of a bitfield");
    }
    let operand = lower_spill(cx, operand, span)?;
    let ty = cx.program.types_mut().pointer_to(operand.ty());
    let out = cx.register(ty, span)?;
    let target = MIRTarget::Register(out);
    let intrinsic = match operand.kind() {
        OperandKind::Place(place) | OperandKind::AdoptedPlace(place) => {
            MIRInternalIntrinsic::PlaceAddress {
                out: target,
                place: *place,
            }
        }
        OperandKind::Global(global) => MIRInternalIntrinsic::GlobalAddress {
            out: target,
            global: *global,
        },
        OperandKind::Ref { reg, .. } => MIRInternalIntrinsic::ReferenceAddress {
            out: target,
            reference: MIRValue::Register(*reg),
        },
        _ => unreachable!("spilled operands are addressable"),
    };
    cx.intrinsic(intrinsic, span);
    Ok(Operand::register(out, ty))
}
