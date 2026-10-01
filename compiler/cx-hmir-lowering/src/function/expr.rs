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
    function::{Expect, Frame, FunctionLowering, Lower, Operand, OperandKind},
    program::DefKey,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

impl FunctionLowering<'_, '_> {
    pub(crate) fn expr(&mut self, frame: usize, id: HMIRExprID, expect: Expect) -> Lower<Operand> {
        let span = self.span(frame, id);
        match self.kind(frame, id) {
            HMIRExprKind::Constant(constant) => {
                let unit = self.frames[frame].def.unit();
                let value = self.program.import_constant(unit, &constant, &span)?;
                self.static_operand(value, &span)
            }
            HMIRExprKind::Local(local) => self.local(frame, local, &span),
            HMIRExprKind::Def(def) => {
                let unit = self.frames[frame].def.unit();
                let key = self.program.resolve(unit, &def, &span)?;
                let value = self.program.def_value(key, &span)?;
                self.static_operand(value, &span)
            }
            HMIRExprKind::Hole(_) => self.error(&span, "unresolved hole in runtime code"),
            HMIRExprKind::Error => self.error(&span, "erroneous expression"),
            HMIRExprKind::Comptime(_) | HMIRExprKind::Quote { .. } => {
                let value = self.eval(frame, id, expect)?;
                self.static_operand(value, &span)
            }
            HMIRExprKind::Splice { quote, args } => self.splice(frame, quote, &args, expect, &span),
            HMIRExprKind::Intrinsic(intrinsic) => self.intrinsic_expr(frame, intrinsic, &span),
            HMIRExprKind::Native(op) => self.native(frame, id, op, expect, &span),
            HMIRExprKind::Let { local, initializer } => {
                self.lower_let(frame, local, initializer, &span)?;
                Ok(Operand::unit(self.program.types_mut()))
            }
            HMIRExprKind::Call { callee, args } => self.call(frame, callee, &args, expect, &span),
            HMIRExprKind::Block {
                kind,
                statements,
                tail,
            } => self.block(frame, kind, &statements, tail, expect, &span),
            HMIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.lower_if(frame, condition, then_branch, else_branch, expect, &span),
            HMIRExprKind::While {
                condition,
                body,
                pre_eval,
            } => self.lower_while(frame, condition, body, pre_eval, &span),
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => self.lower_for(frame, init, condition, increment, body, &span),
            HMIRExprKind::Switch {
                condition,
                cases,
                default,
            } => self.lower_switch(frame, condition, &cases, default, &span),
            HMIRExprKind::Match {
                scrutinee,
                subject,
                arms,
            } => self.lower_match(frame, scrutinee, subject, &arms, expect, &span),
            HMIRExprKind::Label { name, body } => {
                self.lower_label(frame, name, body, expect, &span)
            }
        }
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
        let global = self.program.global_ref(def, span)?;
        let ty = self.program.global_type(def, span)?;
        Ok(Operand::new(OperandKind::Global(global), ty))
    }

    fn local(&mut self, frame: usize, local: HMIRLocalID, span: &TokenRange) -> Lower<Operand> {
        if let Some(value) = self.static_binding(frame, local) {
            return self.static_operand(value, span);
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

    fn lower_let(
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

    fn native(
        &mut self,
        frame: usize,
        id: HMIRExprID,
        op: HMIRNativeOp,
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        match op {
            HMIRNativeOp::BinOp { op, lhs, rhs } => self.binary(frame, op, lhs, rhs, span),
            HMIRNativeOp::UnOp { op, operand } => self.unary(frame, id, op, operand, span),
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            } => super::coerce::coerce(self, frame, mode, value, target, span),
            HMIRNativeOp::Assign { target, op, value } => {
                self.assign(frame, target, op, value, span)
            }
            HMIRNativeOp::AddressOf(inner) => self.address_of(frame, inner, expect, span),
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

    fn splice(
        &mut self,
        frame: usize,
        quote: HMIRExprID,
        args: &[HMIRExprID],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let StaticValue::Quote(quote) = self.eval(frame, quote, Expect::Any)? else {
            return self.error(span, "spliced a value that is not a quote");
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

    fn intrinsic_expr(
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
