use cx_hmir::{
    HMIRBinaryOp, HMIRCoerceMode, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRNativeOp,
    HMIRTypeOp, HMIRUnaryOp,
};
use cx_mir::{
    MIRBindable, MIRBlockTarget, MIRConstant, MIRFloatIntrinsic, MIRInstructionKind,
    MIRIntIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    function::{Expect, FunctionLowering, Lower, Operand},
    ty::{TypeID, TypeKind, TypeTable},
    value::{arithmetic_type, is_comparison, is_logical},
};

impl FunctionLowering<'_, '_> {
    pub(crate) fn binary(
        &mut self,
        frame: usize,
        op: HMIRBinaryOp,
        lhs: HMIRExprID,
        rhs: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if is_logical(op) {
            return self.short_circuit(frame, op, lhs, rhs, span);
        }
        let lhs = self.expr(frame, lhs, Expect::Any)?;
        let rhs = self.expr(frame, rhs, Expect::Any)?;
        self.binary_operands(op, lhs, rhs, span)
    }

    pub(crate) fn binary_operands(
        &mut self,
        op: HMIRBinaryOp,
        lhs: Operand,
        rhs: Operand,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if let (Some(left), Some(right)) = (lhs.as_static(), rhs.as_static())
            && let Ok(value) = self
                .program
                .fold_binary(op, left.clone(), right.clone(), span)
        {
            return self.static_operand(value, span);
        }
        let lhs = self.decay(lhs, span)?;
        let rhs = self.decay(rhs, span)?;
        let left = self.program.types().kind(lhs.ty()).clone();
        let right = self.program.types().kind(rhs.ty()).clone();
        match (&left, &right) {
            (TypeKind::Pointer(element), TypeKind::Int { .. })
                if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
            {
                let ty = lhs.ty();
                self.pointer_offset(lhs, rhs, *element, op == HMIRBinaryOp::Sub, ty, span)
            }
            (TypeKind::Int { .. }, TypeKind::Pointer(element)) if op == HMIRBinaryOp::Add => {
                let ty = rhs.ty();
                self.pointer_offset(rhs, lhs, *element, false, ty, span)
            }
            (TypeKind::Pointer(element), TypeKind::Pointer(_)) if op == HMIRBinaryOp::Sub => {
                let element_ty = self.mir(*element, span)?;
                let ty = self.program.types_mut().int(HMIRIntWidth::I64, true);
                let lhs = self.value(lhs, span)?;
                let rhs = self.value(rhs, span)?;
                let out = self.register(ty, span)?;
                self.intrinsic(
                    MIRPtrIntrinsic::Diff {
                        out: MIRTarget::Register(out),
                        lhs,
                        rhs,
                        element_ty,
                    },
                    span,
                );
                Ok(Operand::register(out, ty))
            }
            (TypeKind::Pointer(_), _) | (_, TypeKind::Pointer(_)) if is_comparison(op) => {
                let (lhs, rhs) = if matches!(left, TypeKind::Pointer(_)) {
                    let ty = lhs.ty();
                    let rhs = self.convert(rhs, ty, span)?;
                    (lhs, rhs)
                } else {
                    let ty = rhs.ty();
                    (self.convert(lhs, ty, span)?, rhs)
                };
                self.pointer_comparison(op, lhs, rhs, span)
            }
            _ => self.arithmetic(op, lhs, rhs, span),
        }
    }

    fn arithmetic(
        &mut self,
        op: HMIRBinaryOp,
        lhs: Operand,
        rhs: Operand,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let types = self.program.types_mut();
        let common = match op {
            HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => {
                arithmetic_type(types, lhs.ty(), lhs.ty())
            }
            _ => arithmetic_type(types, lhs.ty(), rhs.ty()),
        };
        let Some(common) = common else {
            return self.error(
                span,
                format!(
                    "no operator '{}' for '{}' and '{}'",
                    op.path(),
                    self.program.types().display(lhs.ty()),
                    self.program.types().display(rhs.ty())
                ),
            );
        };
        let lhs = self.convert(lhs, common, span)?;
        let rhs = self.convert(rhs, common, span)?;
        let lhs = self.value(lhs, span)?;
        let rhs = self.value(rhs, span)?;
        let result = if is_comparison(op) {
            self.program.types_mut().bool()
        } else {
            common
        };
        let out = self.register(result, span)?;
        let out_target = MIRTarget::Register(out);
        let intrinsic: MIRIntrinsic = match self.program.types().kind(common).clone() {
            TypeKind::Float { .. } => {
                let (out, lhs, rhs) = (out_target, lhs, rhs);
                match op {
                    HMIRBinaryOp::Add => MIRFloatIntrinsic::Add { out, lhs, rhs },
                    HMIRBinaryOp::Sub => MIRFloatIntrinsic::Sub { out, lhs, rhs },
                    HMIRBinaryOp::Mul => MIRFloatIntrinsic::Mul { out, lhs, rhs },
                    HMIRBinaryOp::Div => MIRFloatIntrinsic::Div { out, lhs, rhs },
                    HMIRBinaryOp::Eq => MIRFloatIntrinsic::Eq { out, lhs, rhs },
                    HMIRBinaryOp::Neq => MIRFloatIntrinsic::Neq { out, lhs, rhs },
                    HMIRBinaryOp::Lt => MIRFloatIntrinsic::Lt { out, lhs, rhs },
                    HMIRBinaryOp::Le => MIRFloatIntrinsic::Le { out, lhs, rhs },
                    HMIRBinaryOp::Gt => MIRFloatIntrinsic::Gt { out, lhs, rhs },
                    HMIRBinaryOp::Ge => MIRFloatIntrinsic::Geq { out, lhs, rhs },
                    _ => {
                        return self.error(span, format!("'{}' on floating values", op.path()));
                    }
                }
                .into()
            }
            TypeKind::Int { signed, .. } => {
                let (out, lhs, rhs) = (out_target, lhs, rhs);
                match op {
                    HMIRBinaryOp::Add => MIRIntIntrinsic::Add { out, lhs, rhs },
                    HMIRBinaryOp::Sub => MIRIntIntrinsic::Sub { out, lhs, rhs },
                    HMIRBinaryOp::Mul if signed => MIRIntIntrinsic::SMul { out, lhs, rhs },
                    HMIRBinaryOp::Mul => MIRIntIntrinsic::UMul { out, lhs, rhs },
                    HMIRBinaryOp::Div if signed => MIRIntIntrinsic::SDiv { out, lhs, rhs },
                    HMIRBinaryOp::Div => MIRIntIntrinsic::UDiv { out, lhs, rhs },
                    HMIRBinaryOp::Mod if signed => MIRIntIntrinsic::SMod { out, lhs, rhs },
                    HMIRBinaryOp::Mod => MIRIntIntrinsic::UMod { out, lhs, rhs },
                    HMIRBinaryOp::Eq => MIRIntIntrinsic::Eq { out, lhs, rhs },
                    HMIRBinaryOp::Neq => MIRIntIntrinsic::Neq { out, lhs, rhs },
                    HMIRBinaryOp::Lt if signed => MIRIntIntrinsic::SLt { out, lhs, rhs },
                    HMIRBinaryOp::Lt => MIRIntIntrinsic::ULt { out, lhs, rhs },
                    HMIRBinaryOp::Le if signed => MIRIntIntrinsic::SLe { out, lhs, rhs },
                    HMIRBinaryOp::Le => MIRIntIntrinsic::ULe { out, lhs, rhs },
                    HMIRBinaryOp::Gt if signed => MIRIntIntrinsic::SGt { out, lhs, rhs },
                    HMIRBinaryOp::Gt => MIRIntIntrinsic::UGt { out, lhs, rhs },
                    HMIRBinaryOp::Ge if signed => MIRIntIntrinsic::SGe { out, lhs, rhs },
                    HMIRBinaryOp::Ge => MIRIntIntrinsic::UGe { out, lhs, rhs },
                    HMIRBinaryOp::BAnd => MIRIntIntrinsic::BAnd { out, lhs, rhs },
                    HMIRBinaryOp::BOr => MIRIntIntrinsic::BOr { out, lhs, rhs },
                    HMIRBinaryOp::BXor => MIRIntIntrinsic::BXor { out, lhs, rhs },
                    HMIRBinaryOp::LShift => MIRIntIntrinsic::LShift { out, lhs, rhs },
                    HMIRBinaryOp::RShift if signed => MIRIntIntrinsic::ARShift { out, lhs, rhs },
                    HMIRBinaryOp::RShift => MIRIntIntrinsic::LRShift { out, lhs, rhs },
                    HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                        unreachable!("logical operators short-circuit")
                    }
                }
                .into()
            }
            _ => unreachable!("arithmetic types are integral or floating"),
        };
        self.intrinsic(intrinsic, span);
        Ok(Operand::register(out, result))
    }

    fn pointer_comparison(
        &mut self,
        op: HMIRBinaryOp,
        lhs: Operand,
        rhs: Operand,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let lhs = self.value(lhs, span)?;
        let rhs = self.value(rhs, span)?;
        let bool = self.program.types_mut().bool();
        let out = MIRTarget::Register(self.register(bool, span)?);
        let intrinsic = match op {
            HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
            HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
            HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
            HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs, rhs },
            HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
            HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs, rhs },
            _ => unreachable!("pointer comparisons are comparisons"),
        };
        let MIRTarget::Register(register) = out else {
            unreachable!()
        };
        self.intrinsic(intrinsic, span);
        Ok(Operand::register(register, bool))
    }

    // 'pointer' displaced by 'index' elements, typed 'result'
    pub(crate) fn pointer_offset(
        &mut self,
        pointer: Operand,
        index: Operand,
        element: TypeID,
        subtract: bool,
        result: TypeID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let size = if self.program.types().is_void(element) {
            1
        } else {
            self.program.types_mut().size_of(element, span)?
        };
        let signed = self.program.types().is_signed(index.ty());
        let offset_ty = self.program.types_mut().int(HMIRIntWidth::I64, signed);
        let index = self.convert(index, offset_ty, span)?;
        let index = self.value(index, span)?;
        let pointer = self.value(pointer, span)?;
        let scaled = self.register(offset_ty, span)?;
        let size = self.int_constant(size as i128, offset_ty);
        self.intrinsic(
            MIRIntIntrinsic::SMul {
                out: MIRTarget::Register(scaled),
                lhs: index,
                rhs: size,
            },
            span,
        );
        let out = self.register(result, span)?;
        let (out_target, offset) = (MIRTarget::Register(out), MIRValue::Register(scaled));
        if subtract {
            self.intrinsic(
                MIRPtrIntrinsic::Sub {
                    out: out_target,
                    ptr: pointer,
                    offset,
                },
                span,
            );
        } else {
            self.intrinsic(
                MIRPtrIntrinsic::Add {
                    out: out_target,
                    ptr: pointer,
                    offset,
                },
                span,
            );
        }
        Ok(Operand::register(out, result))
    }

    fn short_circuit(
        &mut self,
        frame: usize,
        op: HMIRBinaryOp,
        lhs: HMIRExprID,
        rhs: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let bool = self.program.types_mut().bool();
        let left = self.expr(frame, lhs, Expect::Any)?;
        let left = self.truthy(left, span)?;
        let left = self.value(left, span)?;

        let rhs_block = self.new_block("logical.rhs");
        let merge = self.new_block("logical.merge");
        let mir_bool = self.mir(bool, span)?;
        let result = self.body.add_block_param(merge, mir_bool, None);
        let rhs_target = MIRBlockTarget::new(rhs_block);
        let merge_target = MIRBlockTarget::with_args(merge, vec![left.clone()]);
        let (true_target, false_target) = match op {
            HMIRBinaryOp::LAnd => (rhs_target, merge_target),
            _ => (merge_target, rhs_target),
        };
        self.emit(
            MIRInstructionKind::Branch {
                cond: left,
                true_target,
                false_target,
            },
            span,
        );

        self.set_block(rhs_block);
        let right = self
            .expr(frame, rhs, Expect::Any)
            .and_then(|right| self.truthy(right, span))
            .and_then(|right| self.value(right, span));
        match right {
            Ok(right) => self.jump(merge, vec![right], span),
            Err(super::Stop::Diverged) => {}
            Err(error) => return Err(error),
        }
        self.set_block(merge);
        Ok(Operand::register(result, bool))
    }

    pub(crate) fn unary(
        &mut self,
        frame: usize,
        id: HMIRExprID,
        op: HMIRUnaryOp,
        operand: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIRUnaryOp::PreIncrement => return self.increment(frame, operand, 1, true, span),
            HMIRUnaryOp::PreDecrement => return self.increment(frame, operand, -1, true, span),
            HMIRUnaryOp::PostIncrement => return self.increment(frame, operand, 1, false, span),
            HMIRUnaryOp::PostDecrement => return self.increment(frame, operand, -1, false, span),
            _ => {}
        }
        let value = self.expr(frame, operand, Expect::Any)?;
        if value.as_static().is_some() {
            let folded = self.eval(frame, id, Expect::Any)?;
            return self.static_operand(folded, span);
        }
        if op == HMIRUnaryOp::LNot {
            let value = self.truthy(value, span)?;
            let bool = value.ty();
            let value = self.value(value, span)?;
            let out = self.register(bool, span)?;
            self.intrinsic(
                MIRIntIntrinsic::LNot {
                    out: MIRTarget::Register(out),
                    value,
                },
                span,
            );
            return Ok(Operand::register(out, bool));
        }

        let types = self.program.types_mut();
        let Some(ty) = arithmetic_type(types, value.ty(), value.ty()) else {
            return self.error(
                span,
                format!(
                    "no operator '{}' for '{}'",
                    op.path(),
                    self.program.types().display(value.ty())
                ),
            );
        };
        let value = self.convert(value, ty, span)?;
        let value = self.value(value, span)?;
        let out = self.register(ty, span)?;
        let target = MIRTarget::Register(out);
        let float = matches!(self.program.types().kind(ty), TypeKind::Float { .. });
        let intrinsic: MIRIntrinsic = match op {
            HMIRUnaryOp::Neg if float => MIRFloatIntrinsic::Neg { out: target, value }.into(),
            HMIRUnaryOp::Neg => MIRIntIntrinsic::Neg { out: target, value }.into(),
            HMIRUnaryOp::BNot if !float => MIRIntIntrinsic::BNot { out: target, value }.into(),
            _ => return self.error(span, format!("'{}' on floating values", op.path())),
        };
        self.intrinsic(intrinsic, span);
        Ok(Operand::register(out, ty))
    }

    fn increment(
        &mut self,
        frame: usize,
        operand: HMIRExprID,
        amount: i128,
        prefix: bool,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let target = self.expr(frame, operand, Expect::Any)?;
        let Some(destination) = target.target() else {
            return self.error(span, "incremented a value that is not addressable");
        };
        let ty = target.ty();
        let current = self.copy(&target, span)?;
        let out = self.register(ty, span)?;
        let out_target = MIRTarget::Register(out);
        match self.program.types().kind(ty).clone() {
            TypeKind::Int { .. } => {
                let step = self.int_constant(amount.abs(), ty);
                let (lhs, rhs) = (current.clone(), step);
                if amount >= 0 {
                    self.intrinsic(
                        MIRIntIntrinsic::Add {
                            out: out_target,
                            lhs,
                            rhs,
                        },
                        span,
                    );
                } else {
                    self.intrinsic(
                        MIRIntIntrinsic::Sub {
                            out: out_target,
                            lhs,
                            rhs,
                        },
                        span,
                    );
                }
            }
            TypeKind::Float { width } => {
                let step = MIRValue::Constant(MIRConstant::Float {
                    value: (amount as f64).into(),
                    ty: TypeTable::mir_float(width),
                });
                self.intrinsic(
                    MIRFloatIntrinsic::Add {
                        out: out_target,
                        lhs: current.clone(),
                        rhs: step,
                    },
                    span,
                );
            }
            TypeKind::Pointer(element) => {
                let size = self.program.types_mut().size_of(element, span)?;
                let size_type = self.program.types_mut().size_type();
                let offset = self.int_constant(amount * size as i128, size_type);
                self.intrinsic(
                    MIRPtrIntrinsic::Add {
                        out: out_target,
                        ptr: current.clone(),
                        offset,
                    },
                    span,
                );
            }
            _ => {
                return self.error(
                    span,
                    format!("cannot increment '{}'", self.program.types().display(ty)),
                );
            }
        }
        let bitfield = target.bitfield().map(MIRStoreBitfield::Target);
        self.store(destination, MIRValue::Register(out), ty, bitfield, span)?;
        if prefix {
            Ok(target)
        } else {
            Ok(Operand::value(current, ty))
        }
    }

    pub(crate) fn assign(
        &mut self,
        frame: usize,
        target: HMIRExprID,
        op: Option<HMIRBinaryOp>,
        value: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let lhs = self.expr(frame, target, Expect::Any)?;
        let Some(destination) = lhs.target() else {
            return self.error(span, "assigned to a value that is not addressable");
        };
        let ty = lhs.ty();
        let value = match op {
            Some(op) => {
                let current = Operand::value(self.copy(&lhs, span)?, ty);
                let rhs = self.expr(frame, value, Expect::Any)?;
                self.binary_operands(op, current, rhs, span)?
            }
            None => self.expr(frame, value, Expect::Type(ty))?,
        };
        let value = self.convert(value, ty, span)?;
        let value = self.value(value, span)?;
        if let MIRTarget::Place(place) = destination {
            self.emit(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Place(place),
                    kind: MIRInvalidationKind::Drop,
                },
                span,
            );
            self.emit(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(place),
                },
                span,
            );
        }
        let bitfield = lhs.bitfield().map(MIRStoreBitfield::Target);
        self.store(destination, value, ty, bitfield, span)?;
        Ok(lhs)
    }

    pub(crate) fn coerce(
        &mut self,
        frame: usize,
        mode: HMIRCoerceMode,
        value: HMIRExprID,
        target: HMIRExprID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if mode == HMIRCoerceMode::Truthy {
            let value = self.expr(frame, value, Expect::Any)?;
            return self.truthy(value, span);
        }
        if self.is_dereference(frame, target) {
            let value = self.expr(frame, value, Expect::Any)?;
            return self.dereference(value, span);
        }
        let ty = self.eval_type(frame, target)?;
        let value = self.expr(frame, value, Expect::Type(ty))?;
        self.convert(value, ty, span)
    }

    // A coercion to '?&' reads through a pointer
    fn is_dereference(&self, frame: usize, target: HMIRExprID) -> bool {
        let HMIRExprKind::Native(HMIRNativeOp::Type(HMIRTypeOp::Reference(inner))) =
            self.kind(frame, target)
        else {
            return false;
        };
        matches!(self.kind(frame, inner), HMIRExprKind::Hole(_))
    }

    fn dereference(&mut self, operand: Operand, span: &TokenRange) -> Lower<Operand> {
        let operand = self.decay(operand, span)?;
        match self.program.types().kind(operand.ty()).clone() {
            TypeKind::Pointer(inner)
                if matches!(self.program.types().kind(inner), TypeKind::Function(_)) =>
            {
                Ok(operand)
            }
            TypeKind::Pointer(_) => self.deref_pointer(operand, span),
            _ => self.error(
                span,
                format!(
                    "cannot dereference '{}'",
                    self.program.types().display(operand.ty())
                ),
            ),
        }
    }
}
