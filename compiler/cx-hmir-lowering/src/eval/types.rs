use cx_hmir::{
    HMIRAggregateKind, HMIRExprID, HMIRExprKind, HMIRFieldDef, HMIRMoveSemantics, HMIRNativeOp,
    HMIRTypeOp,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    program::{Program, def_body, untagged_name},
    staging_error,
    ty::{Field, FunctionType, NominalKey, TypeID, TypeKind},
    value::StaticValue,
};

pub(super) fn exec_type_op(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    op: &HMIRTypeOp,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let types = |program: &mut Program<'_>, frame: &mut EvalFrame, exprs: &[HMIRExprID]| {
        exprs
            .iter()
            .map(|expr| program.eval_type(frame, *expr))
            .collect::<CXResult<Vec<_>>>()
    };
    Ok(match op {
        HMIRTypeOp::Pointer(inner) => {
            let inner = program.eval_type(frame, *inner)?;
            StaticValue::Type(program.types_mut().pointer(inner))
        }
        HMIRTypeOp::Reference(inner) => {
            let inner = program.eval_type(frame, *inner)?;
            StaticValue::Type(program.types_mut().reference(inner))
        }
        HMIRTypeOp::Array { element, length } => {
            let element = program.eval_type(frame, *element)?;
            let length = match length {
                Some(length) => {
                    let value = program.eval(frame, *length)?;
                    let length = value.as_int().ok_or_else(|| {
                        staging_error(span, "array length is not a compile-time integer".into())
                    })?;
                    Some(length.max(0) as u64)
                }
                None => None,
            };
            StaticValue::Type(
                program
                    .types_mut()
                    .intern(TypeKind::Array { element, length }),
            )
        }
        HMIRTypeOp::Function {
            params,
            ret,
            variadic,
        } => {
            let params = types(program, frame, params)?
                .into_iter()
                .filter(|param| !program.types().is_void(*param))
                .collect();
            let ret = program.eval_type(frame, *ret)?;
            StaticValue::Type(
                program
                    .types_mut()
                    .intern(TypeKind::Function(FunctionType::new(
                        params, ret, *variadic,
                    ))),
            )
        }
        HMIRTypeOp::Expr { params, result } => {
            let params = types(program, frame, params)?;
            let result = program.eval_type(frame, *result)?;
            StaticValue::Type(
                program
                    .types_mut()
                    .intern(TypeKind::Expr { params, result }),
            )
        }
        HMIRTypeOp::Aggregate { .. } => unreachable!("aggregate types are evaluated with their id"),
        HMIRTypeOp::SizeOf(operand) | HMIRTypeOp::AlignOf(operand) => {
            let ty = program.static_operand_type(frame, *operand, span)?;
            let size = if matches!(op, HMIRTypeOp::SizeOf(_)) {
                program.types_mut().size_of(ty, span)?
            } else {
                program.types_mut().align_of(ty, span)?
            };
            let size_type = program.types_mut().size_type();
            StaticValue::int(size as i128, size_type)
        }
        HMIRTypeOp::IsInt(operand)
        | HMIRTypeOp::IsFloat(operand)
        | HMIRTypeOp::IsPointer(operand)
        | HMIRTypeOp::IsSigned(operand) => {
            let ty = program.eval_type(frame, *operand)?;
            let kind = program.types().kind(ty);
            let result = match op {
                HMIRTypeOp::IsInt(_) => matches!(kind, TypeKind::Int { .. }),
                HMIRTypeOp::IsFloat(_) => matches!(kind, TypeKind::Float { .. }),
                HMIRTypeOp::IsPointer(_) => matches!(kind, TypeKind::Pointer(_)),
                _ => program.types().is_signed(ty),
            };
            StaticValue::bool(result, program.types_mut())
        }
        HMIRTypeOp::Equal(left, right) => {
            let left = program.eval_type(frame, *left)?;
            let right = program.eval_type(frame, *right)?;
            StaticValue::bool(left == right, program.types_mut())
        }
    })
}

impl Program<'_> {
    // The type an operand of sizeof/alignof designates: a type, or the type of a value
    fn static_operand_type(
        &mut self,
        frame: &mut EvalFrame,
        operand: HMIRExprID,
        span: &TokenRange,
    ) -> CXResult<TypeID> {
        let unit = frame.unit().clone();
        let body = def_body(unit.def(frame.def().def())).expect("evaluated def has a body");
        if let HMIRExprKind::Local(local) = body.expr(operand).kind()
            && frame.local(*local).is_none()
            && let Some(ty) = frame.runtime_type(*local)
        {
            return Ok(ty);
        }
        if let HMIRExprKind::Native(HMIRNativeOp::Coerce { value, target, .. }) =
            body.expr(operand).kind()
            && let HMIRExprKind::Native(HMIRNativeOp::Type(HMIRTypeOp::Reference(inner))) =
                body.expr(*target).kind()
            && matches!(body.expr(*inner).kind(), HMIRExprKind::Hole(_))
        {
            let pointer = self.static_operand_type(frame, *value, span)?;
            return match self.types().kind(pointer) {
                TypeKind::Pointer(inner) | TypeKind::Array { element: inner, .. } => Ok(*inner),
                _ => Err(staging_error(
                    span,
                    format!("cannot dereference '{}'", self.types().display(pointer)),
                )),
            };
        }
        match self.eval(frame, operand)? {
            StaticValue::Type(ty) => Ok(ty),
            value => self.static_type(&value, span),
        }
    }

    pub(super) fn eval_aggregate_type(
        &mut self,
        frame: &mut EvalFrame,
        id: HMIRExprID,
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
        fields: &[HMIRFieldDef],
        span: &TokenRange,
    ) -> CXResult<StaticValue> {
        let owner = frame.owner().clone();
        let key = NominalKey::new(owner.0, owner.1.clone(), id);
        let name = self.nominal_name(&owner);
        let (ty, pending) = self.types_mut().intern_nominal(key, name, kind, semantics);
        if !pending {
            return Ok(StaticValue::Type(ty));
        }
        if self.active_mut().contains(&*owner) && !self.generated_mut().contains_key(&*owner) {
            self.generated_mut()
                .insert((*owner).clone(), StaticValue::Type(ty));
        }
        let mut defined = Vec::with_capacity(fields.len());
        for field in fields {
            let field_ty = self.eval_type(frame, field.ty())?;
            if self.types().is_void(field_ty) && kind != HMIRAggregateKind::TaggedUnion {
                return Err(staging_error(span, "aggregate field of type void".into()));
            }
            defined.push(Field::new(
                field.name().cloned(),
                field_ty,
                field.bit_width(),
            ));
        }
        self.types_mut().define_nominal(ty, defined);
        Ok(StaticValue::Type(ty))
    }

    fn nominal_name(&mut self, owner: &crate::program::Instance) -> String {
        let name = self.def_name(owner.0);
        let base = untagged_name(&name.name).to_string();
        if owner.1.is_empty() {
            return base;
        }
        let args = owner
            .1
            .iter()
            .map(|arg| match arg {
                StaticValue::Type(ty) => self.types().display(*ty),
                StaticValue::Int { value, .. } => value.to_string(),
                _ => "_".to_string(),
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{base}<{args}>")
    }
}
