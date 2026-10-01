use cx_hmir::{HMIRIntWidth, HMIRMoveSemantics};
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRConstant, MIRFloatIntrinsic,
    MIRGlobalRef, MIRInstructionKind, MIRIntIntrinsic, MIRInternalIntrinsic, MIRPlaceID,
    MIRPtrIntrinsic, MIRRegisterID, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    function::{FunctionLowering, Lower},
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

#[derive(Debug, Clone)]
pub(crate) struct Operand {
    kind: OperandKind,
    ty: TypeID,
}

#[derive(Debug, Clone)]
pub(crate) enum OperandKind {
    Place(MIRPlaceID),
    // A reference register; 'origin' is the place whose storage it views, when known
    Ref {
        reg: MIRRegisterID,
        bitfield: Option<MIRBitfieldAccess>,
        origin: Option<MIRPlaceID>,
    },
    Global(MIRGlobalRef),
    Value(MIRValue),
    Static(StaticValue),
}

impl Operand {
    pub(crate) fn new(kind: OperandKind, ty: TypeID) -> Self {
        Self { kind, ty }
    }

    pub(crate) fn place(place: MIRPlaceID, ty: TypeID) -> Self {
        Self::new(OperandKind::Place(place), ty)
    }

    pub(crate) fn value(value: MIRValue, ty: TypeID) -> Self {
        Self::new(OperandKind::Value(value), ty)
    }

    pub(crate) fn register(register: MIRRegisterID, ty: TypeID) -> Self {
        Self::value(MIRValue::Register(register), ty)
    }

    pub(crate) fn reference(reg: MIRRegisterID, ty: TypeID, origin: Option<MIRPlaceID>) -> Self {
        Self::new(
            OperandKind::Ref {
                reg,
                bitfield: None,
                origin,
            },
            ty,
        )
    }

    pub(crate) fn unit(types: &mut TypeTable) -> Self {
        Self::value(MIRValue::Constant(MIRConstant::Unit), types.void())
    }

    pub(crate) fn kind(&self) -> &OperandKind {
        &self.kind
    }

    pub(crate) fn ty(&self) -> TypeID {
        self.ty
    }

    pub(crate) fn with_type(mut self, ty: TypeID) -> Self {
        self.ty = ty;
        self
    }

    pub(crate) fn is_lvalue(&self) -> bool {
        matches!(
            self.kind,
            OperandKind::Place(_) | OperandKind::Ref { .. } | OperandKind::Global(_)
        )
    }

    pub(crate) fn as_static(&self) -> Option<&StaticValue> {
        match &self.kind {
            OperandKind::Static(value) => Some(value),
            _ => None,
        }
    }

    // The value naming this lvalue's storage
    pub(crate) fn address(&self) -> Option<MIRValue> {
        Some(match &self.kind {
            OperandKind::Place(place) => MIRValue::PlaceRef(*place),
            OperandKind::Ref { reg, .. } => MIRValue::Register(*reg),
            OperandKind::Global(global) => MIRValue::Constant(MIRConstant::GlobalRef(*global)),
            _ => return None,
        })
    }

    pub(crate) fn target(&self) -> Option<MIRTarget> {
        Some(match &self.kind {
            OperandKind::Place(place) => MIRTarget::Place(*place),
            OperandKind::Ref { reg, .. } => MIRTarget::Indirect(*reg),
            OperandKind::Global(global) => MIRTarget::Global(*global),
            _ => return None,
        })
    }

    pub(crate) fn bitfield(&self) -> Option<MIRBitfieldAccess> {
        match &self.kind {
            OperandKind::Ref { bitfield, .. } => *bitfield,
            _ => None,
        }
    }

    // The place owning this lvalue's storage
    pub(crate) fn origin(&self) -> Option<MIRPlaceID> {
        match &self.kind {
            OperandKind::Place(place) => Some(*place),
            OperandKind::Ref { origin, .. } => *origin,
            _ => None,
        }
    }
}

impl FunctionLowering<'_, '_> {
    pub(crate) fn is_pod(&self, ty: TypeID) -> bool {
        self.program
            .types()
            .nominal_of(ty)
            .is_none_or(|nominal| nominal.semantics() == HMIRMoveSemantics::POD)
    }

    pub(crate) fn store(
        &mut self,
        target: MIRTarget,
        value: MIRValue,
        ty: TypeID,
        bitfield: Option<MIRStoreBitfield>,
        span: &TokenRange,
    ) -> Lower<()> {
        let ty = self.mir(ty, span)?;
        self.emit(
            MIRInstructionKind::Store {
                target,
                value,
                ty,
                bitfield,
            },
            span,
        );
        Ok(())
    }

    // Copies an lvalue's current value into a register
    pub(crate) fn copy(&mut self, operand: &Operand, span: &TokenRange) -> Lower<MIRValue> {
        let out = self.register(operand.ty, span)?;
        let source = operand.address().expect("copied operand is an lvalue");
        let bitfield = operand.bitfield().map(MIRStoreBitfield::Source);
        self.store(MIRTarget::Register(out), source, operand.ty, bitfield, span)?;
        Ok(MIRValue::Register(out))
    }

    // Moves an lvalue's value out, consuming the liveness of the place that owns it
    pub(crate) fn lift(&mut self, operand: &Operand, span: &TokenRange) -> Lower<MIRValue> {
        let Some(origin) = operand.origin() else {
            return self.copy(operand, span);
        };
        let source = match operand.kind() {
            OperandKind::Ref { reg, .. } => MIRBindable::Register(*reg),
            _ => MIRBindable::Place(origin),
        };
        let out = self.register(operand.ty, span)?;
        self.emit(
            MIRInstructionKind::Lift {
                out,
                source,
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
        Ok(MIRValue::Register(out))
    }

    // The operand's value, moving out of storage whose type cannot be copied
    pub(crate) fn value(&mut self, operand: Operand, span: &TokenRange) -> Lower<MIRValue> {
        match operand.kind {
            OperandKind::Value(value) => Ok(value),
            OperandKind::Static(value) => self.static_value(value, operand.ty, span),
            _ if self.is_pod(operand.ty) => self.copy(&operand, span),
            _ => self.lift(&operand, span),
        }
    }

    pub(crate) fn read(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let ty = operand.ty;
        Ok(Operand::value(self.value(operand, span)?, ty))
    }

    pub(crate) fn static_value(
        &mut self,
        value: StaticValue,
        ty: TypeID,
        span: &TokenRange,
    ) -> Lower<MIRValue> {
        match value {
            StaticValue::Str(string) => {
                let ty = match self.program.types().kind(ty) {
                    TypeKind::Pointer(_) => ty,
                    _ => self.program.types_mut().char_pointer(),
                };
                let out = self.register(ty, span)?;
                self.intrinsic(
                    MIRInternalIntrinsic::StringAddress {
                        out: MIRTarget::Register(out),
                        string,
                    },
                    span,
                );
                Ok(MIRValue::Register(out))
            }
            StaticValue::Function { def, args } => {
                let id = self.program.declare_function(&(def, args), span)?;
                self.program.module_mut().use_function(id);
                Ok(MIRValue::Constant(MIRConstant::Function(id)))
            }
            StaticValue::Global(def) => {
                let global = self.program.global_ref(def, span)?;
                let ty = self.program.global_type(def, span)?;
                self.value(Operand::new(OperandKind::Global(global), ty), span)
            }
            value => Ok(MIRValue::Constant(
                self.program.to_constant(&value, ty, span)?,
            )),
        }
    }

    // Makes the operand addressable, spilling values into a temporary place
    pub(crate) fn spill(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        if operand.is_lvalue() {
            return Ok(operand);
        }
        if let OperandKind::Static(StaticValue::Global(def)) = operand.kind {
            let global = self.program.global_ref(def, span)?;
            let ty = self.program.global_type(def, span)?;
            return Ok(Operand::new(OperandKind::Global(global), ty));
        }
        let ty = operand.ty;
        let value = self.value(operand, span)?;
        let place = self.place(ty, None, span)?;
        self.store(MIRTarget::Place(place), value.clone(), ty, None, span)?;
        if let MIRValue::Register(register) = value
            && !self.is_pod(ty)
        {
            self.emit(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Register(register),
                    kind: MIRInvalidationKind::Move,
                },
                span,
            );
        }
        self.emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
        Ok(Operand::place(place, ty))
    }

    // Reading a reference-typed lvalue yields the referenced storage
    pub(crate) fn auto_deref(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let TypeKind::Reference(inner) = self.program.types().kind(operand.ty).clone() else {
            return Ok(operand);
        };
        let origin = operand.origin();
        let reference = match operand.kind {
            OperandKind::Value(MIRValue::Register(register)) => register,
            _ => match self.value(operand, span)? {
                MIRValue::Register(register) => register,
                _ => return self.error(span, "reference is not a register"),
            },
        };
        Ok(Operand::reference(
            reference,
            inner,
            origin.filter(|_| false),
        ))
    }

    pub(crate) fn int_constant(&mut self, value: i128, ty: TypeID) -> MIRValue {
        let width = self
            .program
            .types()
            .int_info(ty)
            .map(|(width, _)| width)
            .unwrap_or(HMIRIntWidth::I64);
        MIRValue::Constant(MIRConstant::Integer {
            ty: TypeTable::mir_int(width),
            value,
        })
    }

    pub(crate) fn truthy(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let bool = self.program.types_mut().bool();
        if operand.ty == bool {
            return self.read(operand, span);
        }
        if let Some(value) = operand.as_static().and_then(StaticValue::is_truthy) {
            return Ok(Operand::new(
                OperandKind::Static(StaticValue::bool(value, self.program.types_mut())),
                bool,
            ));
        }
        let operand = self.decay(operand, span)?;
        let kind = self.program.types().kind(operand.ty).clone();
        let source_ty = operand.ty;
        let value = self.value(operand, span)?;
        let out = self.register(bool, span)?;
        let target = MIRTarget::Register(out);
        match kind {
            TypeKind::Int { .. } => {
                let zero = self.int_constant(0, source_ty);
                self.intrinsic(
                    MIRIntIntrinsic::Neq {
                        out: target,
                        lhs: value,
                        rhs: zero,
                    },
                    span,
                );
            }
            TypeKind::Float { width } => {
                let zero = MIRValue::Constant(MIRConstant::Float {
                    value: 0.0f64.into(),
                    ty: TypeTable::mir_float(width),
                });
                self.intrinsic(
                    MIRFloatIntrinsic::Neq {
                        out: target,
                        lhs: value,
                        rhs: zero,
                    },
                    span,
                );
            }
            TypeKind::Pointer(_) | TypeKind::Str | TypeKind::Function(_) => {
                let null = MIRValue::Constant(MIRConstant::Nullptr {
                    ty: self.mir(source_ty, span)?,
                });
                self.intrinsic(
                    MIRPtrIntrinsic::Neq {
                        out: target,
                        lhs: value,
                        rhs: null,
                    },
                    span,
                );
            }
            _ => {
                return self.error(
                    span,
                    format!(
                        "'{}' has no truth value",
                        self.program.types().display(source_ty)
                    ),
                );
            }
        }
        Ok(Operand::register(out, bool))
    }

    // Converts between value types; covers C's implicit conversions and explicit casts
    pub(crate) fn convert(
        &mut self,
        operand: Operand,
        target: TypeID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let source = operand.ty;
        if source == target {
            return Ok(operand);
        }
        let types = self.program.types();
        let source_kind = types.kind(source).clone();
        let target_kind = types.kind(target).clone();

        if let OperandKind::Static(value) = &operand.kind
            && !matches!(target_kind, TypeKind::Reference(_))
            && !(matches!(value, StaticValue::Str(_))
                && matches!(target_kind, TypeKind::Array { .. }))
            && let Ok(value) = self.program.coerce_static(value.clone(), target, span)
        {
            return Ok(Operand::new(OperandKind::Static(value), target));
        }

        match (&source_kind, &target_kind) {
            (_, TypeKind::Void) => return Ok(Operand::unit(self.program.types_mut())),
            (TypeKind::Reference(inner), _) if *inner == target => {
                let operand = self.auto_deref(operand, span)?;
                return Ok(operand);
            }
            (_, TypeKind::Reference(inner)) if *inner == source => {
                let operand = self.spill(operand, span)?;
                let address = operand.address().expect("spilled operand is addressable");
                return Ok(Operand::value(address, target));
            }
            (_, TypeKind::Reference(inner)) => {
                let operand = self.decay(operand, span)?;
                if self.program.types().pointee(operand.ty()) == Some(*inner) {
                    let operand = self.deref_pointer(operand, span)?;
                    let address = operand
                        .address()
                        .expect("dereferenced pointer is addressable");
                    return Ok(Operand::value(address, target));
                }
                return self.error(
                    span,
                    format!(
                        "cannot convert '{}' to '{}'",
                        self.program.types().display(source),
                        self.program.types().display(target)
                    ),
                );
            }
            (
                TypeKind::Int {
                    width: from,
                    signed,
                },
                TypeKind::Int { width: to, .. },
            ) => {
                let (from, signed, to) = (*from, *signed, *to);
                let value = self.value(operand, span)?;
                if from == to {
                    return Ok(Operand::value(value, target));
                }
                let out = self.register(target, span)?;
                if to == HMIRIntWidth::I1 {
                    let zero = self.int_constant(0, source);
                    self.intrinsic(
                        MIRIntIntrinsic::Neq {
                            out: MIRTarget::Register(out),
                            lhs: value,
                            rhs: zero,
                        },
                        span,
                    );
                } else {
                    self.intrinsic(
                        MIRIntIntrinsic::IntCast {
                            out: MIRTarget::Register(out),
                            value,
                            target: TypeTable::mir_int(to),
                            sign_extend: signed && from != HMIRIntWidth::I1,
                        },
                        span,
                    );
                }
                return Ok(Operand::register(out, target));
            }
            (TypeKind::Int { signed, .. }, TypeKind::Float { width }) => {
                let (signed, width) = (*signed, *width);
                let value = self.value(operand, span)?;
                let out = self.register(target, span)?;
                self.intrinsic(
                    MIRIntIntrinsic::ToFloat {
                        out: MIRTarget::Register(out),
                        value,
                        target: TypeTable::mir_float(width),
                        signed,
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (TypeKind::Float { .. }, TypeKind::Int { width, signed }) => {
                if *width == HMIRIntWidth::I1 {
                    return self.truthy(operand, span);
                }
                let signed = *signed;
                let value = self.value(operand, span)?;
                let out = self.register(target, span)?;
                let target_ty = self.mir(target, span)?;
                self.intrinsic(
                    MIRFloatIntrinsic::ToInt {
                        out: MIRTarget::Register(out),
                        value,
                        target_ty,
                        signed,
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (TypeKind::Float { width: from }, TypeKind::Float { width: to }) => {
                let value = self.value(operand, span)?;
                if from == to {
                    return Ok(Operand::value(value, target));
                }
                let width = *to;
                let out = self.register(target, span)?;
                self.intrinsic(
                    MIRFloatIntrinsic::FloatCast {
                        out: MIRTarget::Register(out),
                        value,
                        float_ty: TypeTable::mir_float(width),
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (
                TypeKind::Pointer(_) | TypeKind::Str | TypeKind::Function(_),
                TypeKind::Int { width, .. },
            ) => {
                if *width == HMIRIntWidth::I1 {
                    return self.truthy(operand, span);
                }
                let operand = self.decay(operand, span)?;
                let value = self.value(operand, span)?;
                let out = self.register(target, span)?;
                let target_ty = self.mir(target, span)?;
                self.intrinsic(
                    MIRPtrIntrinsic::ToInt {
                        out: MIRTarget::Register(out),
                        ptr: value,
                        target_ty,
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (TypeKind::Int { signed, .. }, TypeKind::Pointer(_)) => {
                let signed = *signed;
                let value = self.value(operand, span)?;
                let out = self.register(target, span)?;
                self.intrinsic(
                    MIRIntIntrinsic::ToPtr {
                        out: MIRTarget::Register(out),
                        value,
                        sign_extend: signed,
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (TypeKind::Str, TypeKind::Array { length, .. }) => {
                let length = *length;
                return self.string_array(operand, target, length, span);
            }
            (
                TypeKind::Array { .. } | TypeKind::Str | TypeKind::Function(_),
                TypeKind::Pointer(_),
            ) => {
                let operand = self.decay(operand, span)?;
                return self.convert(operand, target, span);
            }
            (TypeKind::Pointer(_), TypeKind::Pointer(_)) => {
                let source_mir = self.mir(source, span)?;
                let target_mir = self.mir(target, span)?;
                let value = self.value(operand, span)?;
                if source_mir == target_mir {
                    return Ok(Operand::value(value, target));
                }
                let out = self.register(target, span)?;
                self.intrinsic(
                    MIRInternalIntrinsic::Bitcast {
                        out: MIRTarget::Register(out),
                        value,
                        target_ty: target_mir,
                    },
                    span,
                );
                return Ok(Operand::register(out, target));
            }
            (
                TypeKind::Array { element: from, .. },
                TypeKind::Array {
                    element: to,
                    length: None,
                },
            ) if from == to => {
                return Ok(operand.with_type(source));
            }
            (TypeKind::Unreachable, _) => return Ok(operand.with_type(target)),
            _ => {}
        }

        let source_mir = self.mir(source, span)?;
        let target_mir = self.mir(target, span)?;
        if source_mir == target_mir {
            return Ok(operand.with_type(target));
        }
        self.error(
            span,
            format!(
                "cannot convert '{}' to '{}'",
                self.program.types().display(source),
                self.program.types().display(target)
            ),
        )
    }

    // Arrays, strings and functions used as values become pointers to their first element
    pub(crate) fn decay(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let kind = self.program.types().kind(operand.ty).clone();
        match kind {
            TypeKind::Array { element, .. } => {
                let operand = self.spill(operand, span)?;
                let ty = self.program.types_mut().pointer(element);
                let out = self.register(ty, span)?;
                let array = operand.address().expect("spilled operand is addressable");
                self.intrinsic(
                    MIRInternalIntrinsic::ArrayAddress {
                        out: MIRTarget::Register(out),
                        array,
                    },
                    span,
                );
                Ok(Operand::register(out, ty))
            }
            TypeKind::Str => {
                let ty = self.program.types_mut().char_pointer();
                let value = self.value(operand, span)?;
                Ok(Operand::value(value, ty))
            }
            TypeKind::Function(_) => {
                let ty = self.program.types_mut().pointer(operand.ty);
                match operand.kind {
                    OperandKind::Static(StaticValue::Function { def, args }) => {
                        let id = self.program.declare_function(&(def, args), span)?;
                        self.program.module_mut().use_function(id);
                        let out = self.register(ty, span)?;
                        self.intrinsic(
                            MIRInternalIntrinsic::GetFnPtr {
                                out: MIRTarget::Register(out),
                                fn_id: id,
                            },
                            span,
                        );
                        Ok(Operand::register(out, ty))
                    }
                    _ => {
                        let value = self.value(operand, span)?;
                        Ok(Operand::value(value, ty))
                    }
                }
            }
            _ => Ok(operand),
        }
    }

    fn string_array(
        &mut self,
        operand: Operand,
        target: TypeID,
        length: Option<u64>,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let Some(StaticValue::Str(string)) = operand.as_static().cloned() else {
            return self.error(span, "array initialized from a non-constant string");
        };
        let length = length.unwrap_or(string.len() as u64 + 1) as usize;
        if string.len() > length {
            return self.error(span, "string is longer than its array");
        }
        let mut fields = string
            .bytes()
            .enumerate()
            .map(|(index, byte)| {
                (
                    index,
                    MIRValue::Constant(MIRConstant::Integer {
                        ty: cx_mir::MIRIntType::I8,
                        value: byte as i128,
                    }),
                )
            })
            .collect::<Vec<_>>();
        if string.len() < length {
            fields.push((
                string.len(),
                MIRValue::Constant(MIRConstant::Integer {
                    ty: cx_mir::MIRIntType::I8,
                    value: 0,
                }),
            ));
        }
        let target = match self.program.types().kind(target).clone() {
            TypeKind::Array {
                element,
                length: None,
            } => self.program.types_mut().intern(TypeKind::Array {
                element,
                length: Some(length as u64),
            }),
            _ => target,
        };
        let out = self.register(target, span)?;
        let ty = self.mir(target, span)?;
        self.intrinsic(
            MIRAggregateIntrinsic::AggregateInit {
                out: MIRTarget::Register(out),
                ty,
                fields,
            },
            span,
        );
        Ok(Operand::register(out, target))
    }
}
