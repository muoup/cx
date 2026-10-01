use std::rc::Rc;

use cx_hmir::{
    HMIRAggregateOp, HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth,
    HMIRNativeOp,
};
use cx_mir::{MIRConstant, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    eval::EvalFrame,
    function::{Expect, FunctionLowering, Lower, Operand, Stop},
    program::DefKey,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

enum CallArg {
    Static(StaticValue),
    Runtime(Operand),
}

impl FunctionLowering<'_, '_> {
    pub(crate) fn call(
        &mut self,
        frame: usize,
        callee: HMIRExprID,
        args: &[HMIRExprID],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        if let HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Member {
            base,
            name,
        })) = self.kind(frame, callee)
        {
            let receiver = self.expr(frame, base, Expect::Any)?;
            return self.method_call(frame, receiver, &name, args, expect, span);
        }
        let callee = match self.static_callee(frame, callee)? {
            Some(StaticValue::Function { def, args: bound }) => {
                return self.static_call(frame, def, bound, None, args, expect, span);
            }
            Some(value) => self.static_operand(value, span)?,
            None => self.expr(frame, callee, Expect::Any)?,
        };
        match callee.as_static() {
            Some(StaticValue::Function { def, args: bound }) => {
                let (def, bound) = (*def, bound.clone());
                self.static_call(frame, def, bound, None, args, expect, span)
            }
            _ => self.indirect_call(frame, callee, args, span),
        }
    }

    fn static_callee(&mut self, frame: usize, callee: HMIRExprID) -> Lower<Option<StaticValue>> {
        let span = self.span(frame, callee);
        Ok(match self.kind(frame, callee) {
            HMIRExprKind::Def(def) => {
                let unit = self.frames[frame].def.unit();
                let key = self.program.resolve(unit, &def, &span)?;
                Some(self.program.def_value(key, &span)?)
            }
            HMIRExprKind::Local(local) => self.static_binding(frame, local),
            HMIRExprKind::Comptime(_) => Some(self.eval(frame, callee, Expect::Any)?),
            _ => None,
        })
    }

    // A member that is not a field names an associated function taking the receiver first
    fn method_call(
        &mut self,
        frame: usize,
        receiver: Operand,
        name: &CXIdent,
        args: &[HMIRExprID],
        expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let object = match self.program.types().kind(receiver.ty()) {
            TypeKind::Pointer(inner) => *inner,
            _ => receiver.ty(),
        };
        if self.program.types().field(object, name.as_str()).is_some() {
            let member = self.member(receiver, name, span)?;
            return self.indirect_call(frame, member, args, span);
        }
        let Some(owner) = self
            .program
            .types()
            .nominal_of(object)
            .map(|nominal| nominal.key().owner())
        else {
            return self.error(
                span,
                format!(
                    "'{}' has no method '{name}'",
                    self.program.types().display(object)
                ),
            );
        };
        let Some(def) = self.program.associated(owner, name) else {
            return self.error(
                span,
                format!(
                    "'{}' has no method '{name}'",
                    self.program.types().display(object)
                ),
            );
        };
        let receiver = self.deref_pointer(receiver, span)?;
        self.static_call(frame, def, Vec::new(), Some(receiver), args, expect, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn static_call(
        &mut self,
        frame: usize,
        def: DefKey,
        bound: Vec<StaticValue>,
        receiver: Option<Operand>,
        args: &[HMIRExprID],
        _expect: Expect,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let unit = self.program.unit(def.unit());
        let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
            return self.error(
                span,
                format!("'{}' is not a function", unit.def(def.def()).name()),
            );
        };
        let params = function.signature().params().to_vec();
        let variadic = function.signature().is_variadic();
        let body = function.body();
        let template = self.program.template_params(def);
        let rest_len = params.len() - template.len();
        let given = bound.len() + receiver.is_some() as usize + args.len();
        let explicit_len = given.saturating_sub(rest_len).min(template.len());
        if given < rest_len || (!variadic && given > params.len()) {
            return self.error(
                span,
                format!(
                    "'{}' expects {} arguments, found {given}",
                    unit.def(def.def()).name(),
                    rest_len
                ),
            );
        }

        let mut bound = bound.into_iter();
        let mut args = args.iter().copied();
        let mut explicit = Vec::with_capacity(template.len());
        for _ in 0..explicit_len {
            if let Some(value) = bound.next() {
                explicit.push(Some(value));
                continue;
            }
            let arg = args.next().expect("explicit arguments were counted");
            if matches!(self.kind(frame, arg), HMIRExprKind::Hole(_)) {
                explicit.push(None);
            } else {
                explicit.push(Some(self.eval(frame, arg, Expect::Any)?));
            }
        }

        let mut hints = EvalFrame::new(unit.clone(), def, Rc::new((def, Vec::new())));
        for (param, value) in template.iter().zip(&explicit) {
            if let Some(value) = value {
                hints.bind(*param, value.clone());
            }
        }
        let mut rest = bound.map(CallArg::Static).collect::<Vec<_>>();
        let receiver_index = receiver.as_ref().map(|_| rest.len());
        rest.extend(receiver.map(CallArg::Runtime));
        for arg in args {
            let param = params.get(template.len() + rest.len()).copied();
            let hint = param.and_then(|param| {
                let ty = body.local(param).ty();
                self.program.eval_type(&mut hints, ty).ok()
            });
            let comptime = param.is_some_and(|param| body.local(param).is_comptime());
            rest.push(if comptime {
                CallArg::Static(self.eval(frame, arg, Expect::Any)?)
            } else {
                CallArg::Runtime(self.expr(frame, arg, Expect::of(hint))?)
            });
        }

        let mut actual = Vec::with_capacity(rest_len);
        for arg in rest.iter().take(rest_len) {
            actual.push(match arg {
                CallArg::Static(value) => self.program.static_type(value, span).ok(),
                CallArg::Runtime(operand) => Some(operand.ty()),
            });
        }
        let mut instance_args = if template.is_empty() {
            Vec::new()
        } else {
            self.program.deduce_template(def, explicit, &actual, span)?
        };
        for (param, arg) in params[template.len()..].iter().zip(&rest) {
            if !body.local(*param).is_comptime() {
                continue;
            }
            match arg {
                CallArg::Static(value) => instance_args.push(value.clone()),
                CallArg::Runtime(operand) => match operand.as_static() {
                    Some(value) => instance_args.push(value.clone()),
                    None => {
                        return self.error(span, "comptime argument is not known at compile time");
                    }
                },
            }
        }

        let instance = (def, instance_args);
        let id = self.program.declare_function(&instance, span)?;
        self.program.module_mut().use_function(id);
        let signature = self.program.signature(&instance, span)?;
        let mut values = Vec::with_capacity(rest.len());
        for (index, arg) in rest.into_iter().enumerate() {
            let operand = match arg {
                CallArg::Static(value) => self.static_operand(value, span),
                CallArg::Runtime(operand) => Ok(operand),
            };
            match params.get(template.len() + index) {
                Some(param) if body.local(*param).is_comptime() => continue,
                Some(param) => {
                    let Some(position) =
                        signature.runtime().iter().position(|local| local == param)
                    else {
                        continue;
                    };
                    let ty = signature.params()[position].1;
                    let mut operand = operand?;
                    if receiver_index == Some(index) {
                        operand = self.receiver(operand, ty, span)?;
                    }
                    let operand = self.convert(operand, ty, span)?;
                    values.push(self.value(operand, span)?);
                }
                None => {
                    let value = self.variadic_value(operand?, span)?;
                    values.push(value);
                }
            }
        }
        self.emit_call(
            MIRValue::Constant(MIRConstant::Function(id)),
            values,
            signature.ret(),
            span,
        )
    }

    // A receiver lvalue is passed by address to a method taking a pointer
    fn receiver(&mut self, receiver: Operand, param: TypeID, span: &TokenRange) -> Lower<Operand> {
        match self.program.types().kind(param) {
            TypeKind::Pointer(inner) if *inner == receiver.ty() => {
                self.address_of_operand(receiver, span)
            }
            _ => Ok(receiver),
        }
    }

    fn indirect_call(
        &mut self,
        frame: usize,
        callee: Operand,
        args: &[HMIRExprID],
        span: &TokenRange,
    ) -> Lower<Operand> {
        let function = match self.program.types().kind(callee.ty()) {
            TypeKind::Pointer(inner) => self.program.types().kind(*inner).clone(),
            kind => kind.clone(),
        };
        let TypeKind::Function(function) = function else {
            return self.error(
                span,
                format!(
                    "'{}' is not callable",
                    self.program.types().display(callee.ty())
                ),
            );
        };
        let params = function.params().len();
        if args.len() < params || (!function.is_variadic() && args.len() > params) {
            return self.error(
                span,
                format!("expected {params} arguments, found {}", args.len()),
            );
        }
        let callee = self.decay(callee, span)?;
        let callee = self.value(callee, span)?;
        let mut values = Vec::with_capacity(args.len());
        for (index, arg) in args.iter().enumerate() {
            let param = function.params().get(index).copied();
            let operand = self.expr(frame, *arg, Expect::of(param))?;
            values.push(match param {
                Some(param) => {
                    let operand = self.convert(operand, param, span)?;
                    self.value(operand, span)?
                }
                None => self.variadic_value(operand, span)?,
            });
        }
        self.emit_call(callee, values, function.ret(), span)
    }

    // C's default argument promotions
    fn variadic_value(&mut self, operand: Operand, span: &TokenRange) -> Lower<MIRValue> {
        let operand = self.decay(operand, span)?;
        let promoted = match self.program.types().kind(operand.ty()).clone() {
            TypeKind::Int { width, signed } if width < HMIRIntWidth::I32 => Some(
                self.program
                    .types_mut()
                    .int(HMIRIntWidth::I32, signed || width == HMIRIntWidth::I1),
            ),
            TypeKind::Float {
                width: HMIRFloatWidth::F32,
            } => Some(self.program.types_mut().intern(TypeKind::Float {
                width: HMIRFloatWidth::F64,
            })),
            _ => None,
        };
        let operand = match promoted {
            Some(ty) => self.convert(operand, ty, span)?,
            None => operand,
        };
        self.value(operand, span)
    }

    fn emit_call(
        &mut self,
        callee: MIRValue,
        args: Vec<MIRValue>,
        ret: TypeID,
        span: &TokenRange,
    ) -> Lower<Operand> {
        let types = self.program.types();
        let unreachable = types.is_unreachable(ret);
        let out = if types.is_void(ret) || unreachable {
            None
        } else {
            Some(self.register(ret, span)?)
        };
        self.emit(MIRInstructionKind::Call { out, callee, args }, span);
        if unreachable {
            self.emit(MIRInstructionKind::Unreachable, span);
            return Err(Stop::Diverged);
        }
        Ok(match out {
            Some(register) => Operand::register(register, ret),
            None => Operand::unit(self.program.types_mut()),
        })
    }
}
