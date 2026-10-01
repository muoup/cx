use std::rc::Rc;

use cx_hmir::{
    HMIRExprID, HMIRExprKind, HMIRIntrinsic, HMIRLocalID, HMIRNativeOp, HMIROwnershipOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_mir::{
    MIRBindable, MIRInstructionKind, MIRInternalIntrinsic, MIRTarget, MIRVAIntrinsic, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    function::{
        Expect, Frame, FunctionLowering, Lower, Operand, OperandKind, coerce::coerce, inspect,
    },
    lower::{Context, Output, lower},
    program::DefKey,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

impl FunctionLowering<'_, '_> {
    pub(crate) fn expr(&mut self, frame: usize, id: HMIRExprID, expect: Expect) -> Lower<Operand> {
        let Output::Runtime(value) = lower(Context::Runtime(self, frame), id, expect)? else {
            unreachable!()
        };
        Ok(value)
    }

    pub(crate) fn static_operand(
        &mut self,
        value: StaticValue,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if let StaticValue::Global(def) = value {
            return self.global_operand(def, span);
        }
        let ty = self.program.static_type(&value, span)?;
        Ok(Operand::new(OperandKind::Static(value), ty))
    }

    fn global_operand(&mut self, def: DefKey, span: &TokenRange) -> Lower<Operand> {
        let ty = self.program.global_type(def, span)?;
        if self.unevaluated {
            return Ok(inspect::binding(self, ty, span)?);
        }
        let global = self.program.global_ref(def, span)?;
        Ok(Operand::new(OperandKind::Global(global), ty))
    }

    pub(crate) fn local(
        &mut self,
        frame: usize,
        local: HMIRLocalID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if let Some(value) = self.static_binding(frame, local) {
            if self.unevaluated
                && !matches!(
                    value,
                    StaticValue::Type(_) | StaticValue::Function { .. } | StaticValue::Quote(_)
                )
            {
                let ty = self.program.static_type(&value, span)?;
                let operand = inspect::binding(self, ty, span)?;
                return self.auto_deref(operand, span);
            }
            return self.static_operand(value, span);
        }
        if self.unevaluated && self.binding(frame, local).is_none() {
            let ty = self.frames[frame].body().local(local).ty();
            let ty = self.eval_type(frame, ty)?;
            let operand = inspect::binding(self, ty, span)?;
            self.bind(frame, local, operand);
        }
        let Some(operand) = self.binding(frame, local) else {
            let name = self.frames[frame]
                .body()
                .local(local)
                .name()
                .map(ToString::to_string)
                .unwrap_or_else(|| local.to_string());
            return self.error(span, format!("'{name}' is not bound at runtime"));
        };
        self.auto_deref(operand, span)
    }

    pub(crate) fn lower_let(
        &mut self,
        frame: usize,
        local: HMIRLocalID,
        initializer: Option<HMIRExprID>,
        span: &TokenRange,
    ) -> Lower<()> {
        let decl = self.frames[frame].body().local(local).clone();
        if decl.is_comptime() {
            let value = match initializer {
                Some(initializer) => {
                    let declared = self.eval_type_hint(frame, decl.ty())?;
                    let value = self.eval(frame, initializer, Expect::of(declared))?;
                    match declared {
                        Some(ty) if !matches!(self.program.types().kind(ty), TypeKind::Type) => {
                            self.program.coerce_static(value, ty, span)?
                        }
                        _ => value,
                    }
                }
                None => StaticValue::Unit,
            };
            self.frames[frame].statics.insert(local, value);
            return Ok(());
        }

        let declared = self.eval_type_hint(frame, decl.ty())?;
        let name = decl.name().cloned();
        if let Some(initializer) = initializer
            && let HMIRExprKind::Native(HMIRNativeOp::OwnershipOp(
                op @ (HMIROwnershipOp::Adopt(_) | HMIROwnershipOp::Allocate(_)),
            )) = self.kind(frame, initializer)
        {
            let place = self.place_op(frame, &op, declared, name, span)?;
            self.bind(frame, local, place);
            return Ok(());
        }

        let init = initializer
            .map(|initializer| self.expr(frame, initializer, Expect::of(declared)))
            .transpose()?;
        let ty = match (declared, &init) {
            (Some(ty), Some(init)) => match self.program.types().kind(ty) {
                TypeKind::Array { length: None, .. } => {
                    let init = self.convert(init.clone(), ty, span)?;
                    init.ty()
                }
                _ => ty,
            },
            (Some(ty), None) => ty,
            (None, Some(init)) => self.inferred_type(init.ty()),
            (None, None) => return self.error(span, "local without a type or initializer"),
        };
        if self.program.types().is_void(ty) {
            let unit = Operand::unit(self.program.types_mut());
            self.bind(frame, local, unit);
            return Ok(());
        }

        let place = self.place(ty, name, span)?;
        if let Some(init) = init {
            let init = self.convert(init, ty, span)?;
            let value = self.value(init, span)?;
            self.store(MIRTarget::Place(place), value, ty, None, span)?;
        }
        self.emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
        self.bind(frame, local, Operand::place(place, ty));
        Ok(())
    }

    // The type a local takes from its initializer when it declares none
    pub(crate) fn inferred_type(&mut self, ty: TypeID) -> TypeID {
        let types = self.program.types_mut();
        match types.kind(ty).clone() {
            TypeKind::Str => types.char_pointer(),
            TypeKind::Function(_) => types.pointer(ty),
            TypeKind::Reference(inner) => inner,
            _ => ty,
        }
    }

    // 'allocate' and 'adopt' create places rather than values
    fn place_op(
        &mut self,
        frame: usize,
        op: &HMIROwnershipOp,
        declared: Option<TypeID>,
        name: Option<cx_util::identifier::CXIdent>,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIROwnershipOp::Allocate(ty) => {
                let ty = match declared {
                    Some(ty) => ty,
                    None => self.eval_type(frame, *ty)?,
                };
                let place = self.place(ty, name, span)?;
                Ok(Operand::place(place, ty))
            }
            HMIROwnershipOp::Adopt(pointer) => {
                let pointer = self.expr(frame, *pointer, Expect::Any)?;
                let pointer = self.decay(pointer, span)?;
                let ty = match declared.or_else(|| self.program.types().pointee(pointer.ty())) {
                    Some(ty) => ty,
                    None => return self.error(span, "adopted a non-pointer"),
                };
                let pointer_ty = self.program.types_mut().pointer(ty);
                let pointer = self.convert(pointer, pointer_ty, span)?;
                let address = self.value(pointer, span)?;
                let place = self.place(ty, name, span)?;
                self.body.mark_adopted(place);
                self.intrinsic(MIRInternalIntrinsic::AdoptPlace { place, address }, span);
                self.emit(
                    MIRInstructionKind::Initialize {
                        place: MIRBindable::Place(place),
                    },
                    span,
                );
                Ok(Operand::place(place, ty))
            }
            _ => unreachable!("only allocate and adopt create places"),
        }
    }

    pub(crate) fn native(
        &mut self,
        frame: usize,
        id: HMIRExprID,
        op: HMIRNativeOp,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIRNativeOp::BinOp { op, lhs, rhs } => self.binary(frame, op, lhs, rhs, span),
            HMIRNativeOp::UnOp { op, operand } => self.unary(frame, op, operand, span),
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            } => coerce(self, frame, mode, value, target, span),
            HMIRNativeOp::Assign { target, op, value } => {
                self.assign(frame, target, op, value, span)
            }
            HMIRNativeOp::AddressOf(inner) => self.address_of(frame, inner, expect, span),
            HMIRNativeOp::Dereference(inner) => {
                let operand = self.expr(frame, inner, Expect::Any)?;
                let operand = self.decay(operand, span)?;
                let Some(inner) = self.program.types().pointee(operand.ty()) else {
                    return self.error(span, "dereferenced a non-pointer");
                };
                if matches!(self.program.types().kind(inner), TypeKind::Function(_)) {
                    return Ok(operand);
                }
                self.deref_pointer(operand, span)
            }
            HMIRNativeOp::Type(_) => {
                let value = self.eval(frame, id, expect)?;
                self.static_operand(value, span)
            }
            HMIRNativeOp::Control(op) => self.control(frame, op, expect, span),
            HMIRNativeOp::OwnershipOp(op) => self.ownership(frame, op, expect, span),
            HMIRNativeOp::AggregateOp(op) => self.aggregate(frame, op, expect, span),
        }
    }

    fn ownership(
        &mut self,
        frame: usize,
        op: HMIROwnershipOp,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIROwnershipOp::Move(inner) => {
                let operand = self.expr(frame, inner, expect)?;
                if !operand.is_lvalue() {
                    return Ok(operand);
                }
                let ty = operand.ty();
                let value = self.lift(&operand, span)?;
                Ok(Operand::value(value, ty))
            }
            HMIROwnershipOp::Leak(inner) => {
                let operand = self.expr(frame, inner, expect)?;
                if let OperandKind::Place(place) = operand.kind()
                    && self.program.types().is_nodrop(operand.ty())
                {
                    self.emit(
                        MIRInstructionKind::Invalidate {
                            place: MIRBindable::Place(*place),
                            kind: MIRInvalidationKind::Leak,
                        },
                        span,
                    );
                }
                Ok(operand)
            }
            op => self.place_op(frame, &op, None, None, span),
        }
    }

    pub(crate) fn splice(
        &mut self,
        frame: usize,
        quote: HMIRExprID,
        args: &[HMIRExprID],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let quote = if self.unevaluated {
            let operand = self.expr(frame, quote, Expect::Any)?;
            match operand.as_static() {
                Some(StaticValue::Quote(quote)) => quote.clone(),
                _ => {
                    let TypeKind::Expr { params, result } =
                        self.program.types().kind(operand.ty()).clone()
                    else {
                        return self.error(span, "spliced a value that is not a quote");
                    };
                    if params.len() != args.len() {
                        return self.error(
                            span,
                            "expression argument count does not match its parameters",
                        );
                    }
                    for (arg, param) in args.iter().zip(params) {
                        let arg = self.expr(frame, *arg, Expect::Type(param))?;
                        self.convert(arg, param, span)?;
                    }
                    return Ok(inspect::binding(self, result, span)?);
                }
            }
        } else {
            let StaticValue::Quote(quote) = self.eval(frame, quote, Expect::Any)? else {
                return self.error(span, "spliced a value that is not a quote");
            };
            quote
        };
        let quote = quote.get();
        if quote.params().len() != args.len() {
            return self.error(
                span,
                format!(
                    "quote expects {} arguments, found {}",
                    quote.params().len(),
                    args.len()
                ),
            );
        }
        let mut operands = Vec::with_capacity(args.len());
        for arg in args {
            operands.push(self.expr(frame, *arg, Expect::Any)?);
        }

        let unit = self.program.unit(quote.unit());
        let def = DefKey::new(quote.unit(), quote.def());
        let mut spliced = Frame::new(unit, def, Rc::new(quote.owner().clone()));
        spliced.statics = quote.env().clone();
        spliced.origin = quote
            .origin()
            .filter(|origin| origin.lowering() == self.serial)
            .map(|origin| origin.frame());
        self.frames.push(spliced);
        let index = self.frames.len() - 1;
        if self.unevaluated {
            self.frames[index].origin = None;
            for (local, ty) in quote.runtime_types() {
                let operand = inspect::binding(self, *ty, span)?;
                self.bind(index, *local, operand);
            }
        }
        for (param, operand) in quote.params().iter().zip(operands) {
            let operand = match operand.kind() {
                OperandKind::Static(value) => {
                    self.frames[index].statics.insert(*param, value.clone());
                    continue;
                }
                _ => operand,
            };
            self.bind(index, *param, operand);
        }
        self.expr(index, quote.body(), expect)
    }

    pub(crate) fn intrinsic_expr(
        &mut self,
        frame: usize,
        intrinsic: HMIRIntrinsic,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let Intrinsic::VA(intrinsic) = intrinsic else {
            return self.error(span, "only variadic intrinsics are lowered from HMIR");
        };
        match intrinsic {
            VAIntrinsic::Start { list, last } => {
                let list = self.storage(frame, list)?;
                let last = self.storage(frame, last)?;
                self.intrinsic(MIRVAIntrinsic::VaStart { list, last }, span);
                Ok(Operand::unit(self.program.types_mut()))
            }
            VAIntrinsic::End { list } => {
                let list = self.storage(frame, list)?;
                self.intrinsic(MIRVAIntrinsic::VaEnd { list }, span);
                Ok(Operand::unit(self.program.types_mut()))
            }
            VAIntrinsic::Arg { list, ty } => {
                let list = self.storage(frame, list)?;
                let ty = self.eval_type(frame, ty)?;
                let out = self.register(ty, span)?;
                let mir = self.mir(ty, span)?;
                self.intrinsic(
                    MIRVAIntrinsic::VaArg {
                        out: MIRTarget::Register(out),
                        list,
                        ty: mir,
                    },
                    span,
                );
                Ok(Operand::register(out, ty))
            }
        }
    }

    // The storage an lvalue names, or the value of anything else
    fn storage(&mut self, frame: usize, id: HMIRExprID) -> Lower<MIRValue> {
        let operand = self.expr(frame, id, Expect::Any)?;
        match operand.address() {
            Some(address) => Ok(address),
            None => self.value(operand, &self.span(frame, id)),
        }
    }
}
