use cx_hmir::{HMIRAggregateKind, HMIRAggregateOp, HMIRExprID, HMIRExprKind, HMIRPattern};
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
    function::{Expect, FunctionLowering, Lower, Operand, OperandKind, PatternBinding},
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

impl FunctionLowering<'_, '_> {
    pub(crate) fn aggregate(
        &mut self,
        frame: usize,
        op: HMIRAggregateOp,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIRAggregateOp::Member { base, name } => {
                let base = self.expr(frame, base, Expect::Any)?;
                self.member(base, &name, span)
            }
            HMIRAggregateOp::Index { base, index } => {
                let base = self.expr(frame, base, Expect::Any)?;
                let index = self.expr(frame, index, Expect::Any)?;
                self.index(base, index, span)
            }
            HMIRAggregateOp::Initialize { ty, fields } => {
                self.initialize(frame, ty, &fields, expect, span)
            }
            HMIRAggregateOp::Is { value, pattern } => self.is(frame, value, pattern, span),
        }
    }

    // A pointer used as an aggregate is read through
    pub(crate) fn deref_pointer(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let TypeKind::Pointer(inner) = self.program.types().kind(operand.ty()).clone() else {
            return Ok(operand);
        };
        let pointer = self.value(operand, span)?;
        let ty = self.program.types_mut().reference(inner);
        let out = self.register(ty, span)?;
        let target_ty = self.mir(ty, span)?;
        self.intrinsic(
            MIRInternalIntrinsic::Bitcast {
                out: MIRTarget::Register(out),
                value: pointer,
                target_ty,
            },
            span,
        );
        Ok(Operand::reference(out, inner, None))
    }

    pub(crate) fn member(
        &mut self,
        base: Operand,
        name: &CXIdent,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let base = match self.program.types().kind(base.ty()) {
            TypeKind::Array { .. } => self.decay(base, span)?,
            _ => base,
        };
        let base = self.deref_pointer(base, span)?;
        let base = self.spill(base, span)?;
        let Some((index, field)) = self.program.types().field(base.ty(), name.as_str()) else {
            return self.error(
                span,
                format!(
                    "'{}' has no member '{name}'",
                    self.program.types().display(base.ty())
                ),
            );
        };
        let field_ty = field.ty();
        let struct_ty = self.mir(base.ty(), span)?;
        let bitfield =
            match calculate_field_layout(self.program.types().mir_types(), struct_ty, index) {
                Some(MIRFieldLayout::Bitfield {
                    bit_offset,
                    bit_width,
                    ..
                }) => Some(MIRBitfieldAccess {
                    bit_offset,
                    bit_width,
                    signed: self.program.types().is_signed(field_ty),
                }),
                _ => None,
            };
        let reference = self.program.types_mut().reference(field_ty);
        let out = self.register(reference, span)?;
        let address = base.address().expect("spilled operand is addressable");
        self.intrinsic(
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

    fn index(&mut self, base: Operand, index: Operand, span: &TokenRange) -> Lower<Operand> {
        let origin = base.origin();
        let (pointer, element) = match self.program.types().kind(base.ty()).clone() {
            TypeKind::Array { element, .. } => (self.decay(base, span)?, element),
            TypeKind::Pointer(element) => (base, element),
            TypeKind::Str => {
                let pointer = self.decay(base, span)?;
                let element = self
                    .program
                    .types()
                    .pointee(pointer.ty())
                    .expect("decayed string");
                (pointer, element)
            }
            _ => {
                return self.error(
                    span,
                    format!("cannot index '{}'", self.program.types().display(base.ty())),
                );
            }
        };
        let reference = self.program.types_mut().reference(element);
        let address = self.pointer_offset(pointer, index, element, false, reference, span)?;
        Ok(Operand::reference(register_of(&address), element, origin))
    }

    fn initialize(
        &mut self,
        frame: usize,
        ty: HMIRExprID,
        fields: &[(Option<CXIdent>, HMIRExprID)],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let ty = self.initializer_type(frame, ty, expect, span)?;
        let mut values = Vec::with_capacity(fields.len());
        let ty = match self.program.types().kind(ty).clone() {
            TypeKind::Array { element, length } => {
                if let Some(length) = length
                    && fields.len() as u64 > length
                {
                    return self.error(span, "too many initializers for the array");
                }
                for (index, (_, value)) in fields.iter().enumerate() {
                    let value = self.field_value(frame, *value, element, span)?;
                    values.push((index, value));
                }
                self.program.types_mut().intern(TypeKind::Array {
                    element,
                    length: Some(length.unwrap_or(fields.len() as u64)),
                })
            }
            TypeKind::Nominal(_) => {
                let tagged = self
                    .program
                    .types()
                    .nominal_of(ty)
                    .is_some_and(|nominal| nominal.kind() == HMIRAggregateKind::TaggedUnion);
                let mut next = 0;
                for (name, value) in fields {
                    let (index, field_ty) = match name {
                        Some(name) => match self.program.types().field(ty, name.as_str()) {
                            Some((index, field)) => (index, field.ty()),
                            None => {
                                return self.error(
                                    span,
                                    format!(
                                        "'{}' has no member '{name}'",
                                        self.program.types().display(ty)
                                    ),
                                );
                            }
                        },
                        None => (next, self.program.member_type(ty, next, span)?),
                    };
                    next = index + 1;
                    let value = if tagged && self.program.types().is_void(field_ty) {
                        self.expr(frame, *value, Expect::Discard)?;
                        MIRValue::Constant(MIRConstant::Unit)
                    } else {
                        self.field_value(frame, *value, field_ty, span)?
                    };
                    values.push((index, value));
                }
                ty
            }
            _ if fields.len() == 1 => {
                let operand = self.expr(frame, fields[0].1, Expect::Type(ty))?;
                return self.convert(operand, ty, span);
            }
            _ => {
                return self.error(
                    span,
                    format!(
                        "'{}' cannot be initialized from a list",
                        self.program.types().display(ty)
                    ),
                );
            }
        };
        let out = self.register(ty, span)?;
        let mir = self.mir(ty, span)?;
        self.intrinsic(
            MIRAggregateIntrinsic::AggregateInit {
                out: MIRTarget::Register(out),
                ty: mir,
                fields: values,
            },
            span,
        );
        Ok(Operand::register(out, ty))
    }

    fn field_value(
        &mut self,
        frame: usize,
        value: HMIRExprID,
        ty: TypeID,
        span: &TokenRange,
    ) -> Lower<MIRValue> {
        let operand = self.expr(frame, value, Expect::Type(ty))?;
        let operand = self.convert(operand, ty, span)?;
        self.value(operand, span)
    }

    // The initialized type; a hole takes the expected type, and a bare generator is applied
    // to the arguments of the expected type it generates
    fn initializer_type(
        &mut self,
        frame: usize,
        ty: HMIRExprID,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<TypeID> {
        let expected = expect
            .ty()
            .map(|expected| match self.program.types().kind(expected) {
                TypeKind::Reference(inner) => *inner,
                _ => expected,
            });
        if matches!(self.kind(frame, ty), HMIRExprKind::Hole(_)) {
            return match expected {
                Some(expected) => Ok(expected),
                None => self.error(span, "cannot infer the initialized type"),
            };
        }
        match self.eval(frame, ty, Expect::Any)? {
            StaticValue::Type(ty) => Ok(ty),
            StaticValue::Function { def, .. }
                if expected.is_some_and(|expected| {
                    self.program
                        .types()
                        .nominal_of(expected)
                        .is_some_and(|nominal| nominal.key().owner() == def)
                }) =>
            {
                Ok(expected.expect("checked above"))
            }
            other => self.error(
                span,
                format!("expected a type to initialize, found {other:?}"),
            ),
        }
    }

    fn is(
        &mut self,
        frame: usize,
        value: HMIRExprID,
        pattern: HMIRPattern,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let subject = self.expr(frame, value, Expect::Any)?;
        let binds = match &pattern {
            HMIRPattern::Binding(_) => true,
            HMIRPattern::Variant { inner, .. } => inner.is_some(),
            _ => false,
        };
        let owned = binds && !subject.is_lvalue();
        let bool = self.program.types_mut().bool();
        let out = self.register(bool, span)?;
        let target = MIRTarget::Register(out);
        let subject = match &pattern {
            HMIRPattern::Binding(_) => {
                let subject = self.spill(subject, span)?;
                let truth = self.int_constant(1, bool);
                self.store(target, truth, bool, None, span)?;
                subject
            }
            HMIRPattern::Integer(expected) => {
                let ty = subject.ty();
                let expected = self.int_constant(*expected as i128, ty);
                let value = self.value(subject.clone(), span)?;
                self.intrinsic(
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
                let TypeKind::Float { width } = self.program.types().kind(subject.ty()).clone()
                else {
                    return self.error(span, "floating pattern on a non-floating value");
                };
                let value = self.value(subject.clone(), span)?;
                self.intrinsic(
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
            HMIRPattern::Variant { index, .. } => {
                let subject = self.spill(subject, span)?;
                let tag = self.sum_index(&subject, span)?;
                self.intrinsic(
                    MIRIntIntrinsic::Eq {
                        out: target,
                        lhs: tag,
                        rhs: MIRValue::Constant(MIRConstant::Integer {
                            ty: MIRIntType::I8,
                            value: *index as i128,
                        }),
                    },
                    span,
                );
                subject
            }
        };
        if binds {
            self.pattern_bindings.push(PatternBinding {
                frame,
                subject,
                owned,
                pattern,
            });
        }
        Ok(Operand::register(out, bool))
    }

    pub(crate) fn sum_index(&mut self, subject: &Operand, span: &TokenRange) -> Lower<MIRValue> {
        let tag = self
            .program
            .types_mut()
            .int(cx_hmir::HMIRIntWidth::I8, false);
        let out = self.register(tag, span)?;
        let sum_ty = self.mir(subject.ty(), span)?;
        let value = subject.address().expect("matched subjects are addressable");
        self.intrinsic(
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
    pub(crate) fn bind_pattern(&mut self, binding: PatternBinding, span: &TokenRange) -> Lower<()> {
        let PatternBinding {
            frame,
            subject,
            owned,
            pattern,
        } = binding;
        let span = span.clone();
        match pattern {
            HMIRPattern::Binding(local) => self.bind(frame, local, subject),
            HMIRPattern::Variant {
                index,
                inner: Some(local),
                ..
            } => {
                let payload = self.program.member_type(subject.ty(), index, &span)?;
                if self.program.types().is_void(payload) {
                    let unit = Operand::unit(self.program.types_mut());
                    self.bind(frame, local, unit);
                    return Ok(());
                }
                let reference_ty = self.program.types_mut().reference(payload);
                let reference = self.register(reference_ty, &span)?;
                let sum_ty = self.mir(subject.ty(), &span)?;
                self.intrinsic(
                    MIRAggregateIntrinsic::SumVariant {
                        out: MIRTarget::Register(reference),
                        base: subject.address().expect("matched subjects are addressable"),
                        variant: index,
                        sum_ty,
                    },
                    &span,
                );
                let bound = match subject.origin().filter(|_| owned) {
                    None => Operand::reference(reference, payload, None),
                    Some(origin) => {
                        self.lift_payload(reference, origin, payload, frame, local, &span)?
                    }
                };
                self.bind(frame, local, bound);
            }
            _ => {}
        }
        Ok(())
    }

    fn lift_payload(
        &mut self,
        reference: MIRRegisterID,
        origin: cx_mir::MIRPlaceID,
        payload: TypeID,
        frame: usize,
        local: cx_hmir::HMIRLocalID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let out = self.register(payload, span)?;
        self.emit(
            MIRInstructionKind::Lift {
                out,
                source: MIRBindable::Register(reference),
                origin: MIRBindable::Place(origin),
            },
            span,
        );
        self.emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(origin),
                kind: MIRInvalidationKind::Move,
            },
            span,
        );
        let name = self.frames[frame].body().local(local).name().cloned();
        let place = self.place(payload, name, span)?;
        self.store(
            MIRTarget::Place(place),
            MIRValue::Register(out),
            payload,
            None,
            span,
        )?;
        self.emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Register(out),
                kind: MIRInvalidationKind::Move,
            },
            span,
        );
        self.emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
        Ok(Operand::place(place, payload))
    }

    pub(crate) fn address_of(
        &mut self,
        frame: usize,
        inner: HMIRExprID,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let operand = self.expr(frame, inner, Expect::Any)?;
        let decays = match self.program.types().kind(operand.ty()).clone() {
            TypeKind::Function(_) | TypeKind::Str => true,
            TypeKind::Array { element, .. } => expect
                .ty()
                .and_then(|expected| self.program.types().pointee(expected))
                .is_some_and(|pointee| pointee == element),
            _ => false,
        };
        if decays {
            return self.decay(operand, span);
        }
        self.address_of_operand(operand, span)
    }

    pub(crate) fn address_of_operand(
        &mut self,
        operand: Operand,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if operand.bitfield().is_some() {
            return self.error(span, "cannot take the address of a bitfield");
        }
        let operand = self.spill(operand, span)?;
        let ty = self.program.types_mut().pointer(operand.ty());
        let out = self.register(ty, span)?;
        let target = MIRTarget::Register(out);
        let intrinsic = match operand.kind() {
            OperandKind::Place(place) => MIRInternalIntrinsic::PlaceAddress {
                out: target,
                place: *place,
            },
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
        self.intrinsic(intrinsic, span);
        Ok(Operand::register(out, ty))
    }
}

pub(crate) fn register_of(operand: &Operand) -> MIRRegisterID {
    match operand.kind() {
        OperandKind::Value(MIRValue::Register(register)) => *register,
        _ => unreachable!("operand was produced into a register"),
    }
}
