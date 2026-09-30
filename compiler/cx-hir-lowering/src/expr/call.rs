use cx_hir::ast::{
    expression::{HIRBinOp, HIRExprKind, HIRExpression},
    function::HIRComptimeFnPrototype,
    template::HIRTemplateInput,
    types::HIRType,
};
use cx_hmir::{HMIRAggregateOp, HMIRConstant, HMIRDefRef, HMIRExprID, HMIRExprKind};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    resolve::GlobalSymbol,
};

fn comma_separated(expr: &HIRExpression) -> Vec<&HIRExpression> {
    match &expr.kind {
        HIRExprKind::Void => Vec::new(),
        HIRExprKind::BinOp {
            op: HIRBinOp::Comma,
            lhs,
            rhs,
        } => {
            let mut exprs = comma_separated(lhs);
            exprs.push(rhs);
            exprs
        }
        _ => vec![expr],
    }
}

impl BodyLowering<'_> {
    pub(super) fn lower_call<'h>(
        &mut self,
        call: &'h HIRExpression,
        prepend: Vec<&'h HIRExpression>,
        append: Vec<&'h HIRExpression>,
    ) -> HMIRExprID {
        let HIRExprKind::BinOp { op, lhs, rhs } = &call.kind else {
            return self.error(&call.range);
        };
        match op {
            HIRBinOp::MethodCall => {
                let args = prepend
                    .into_iter()
                    .chain(comma_separated(rhs))
                    .chain(append)
                    .collect();
                self.lower_callee_call(lhs, args, &call.range)
            }
            HIRBinOp::Pipe => {
                let prepend = std::iter::once(lhs.as_ref()).chain(prepend).collect();
                self.lower_call(rhs, prepend, append)
            }
            HIRBinOp::BackwardPipe => {
                let append = std::iter::once(rhs.as_ref()).chain(append).collect();
                self.lower_call(lhs, prepend, append)
            }
            _ => self.error(&call.range),
        }
    }

    fn lower_callee_call(
        &mut self,
        callee: &HIRExpression,
        args: Vec<&HIRExpression>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let HIRExprKind::Identifier {
            name,
            template_input,
        } = &callee.kind
        else {
            let callee = self.lower_expr(callee);
            return self.call(callee, Vec::new(), &args, span);
        };

        if let Some(root) = name.root_name_ref()
            && let Some(intrinsic) = self.builtin_call(root.as_str(), &args, span)
        {
            return intrinsic;
        }

        match self.lookup(name, None) {
            Symbol::Local(binding) if binding.is_quoted() => {
                let quote = self.push(HMIRExprKind::Local(binding.local()), &callee.range);
                let args = args.iter().map(|arg| self.lower_expr(arg)).collect();
                self.push(HMIRExprKind::Splice { quote, args }, span)
            }
            Symbol::Global(GlobalSymbol::ComptimeFunction(def, prototype)) => {
                self.comptime_call(def, &prototype, template_input.as_ref(), &args, span)
            }
            Symbol::Global(GlobalSymbol::Constructor(data, variant)) => {
                let value = match args.as_slice() {
                    [] => self.push(HMIRExprKind::Constant(HMIRConstant::Unit), span),
                    [value] => self.lower_expr(value),
                    _ => self.error(span),
                };
                self.construct(&data.union_type, template_input.as_ref(), variant, value, span)
            }
            _ => {
                let callee_id = self.identifier(name, None, &callee.range);
                let leading = self.template_args(template_input.as_ref());
                self.call(callee_id, leading, &args, span)
            }
        }
    }

    fn call(
        &mut self,
        callee: HMIRExprID,
        mut leading: Vec<HMIRExprID>,
        args: &[&HIRExpression],
        span: &TokenRange,
    ) -> HMIRExprID {
        leading.extend(args.iter().map(|arg| self.lower_expr(arg)));
        self.push(
            HMIRExprKind::Call {
                callee,
                args: leading,
            },
            span,
        )
    }

    fn comptime_call(
        &mut self,
        def: HMIRDefRef,
        prototype: &HIRComptimeFnPrototype,
        template_input: Option<&HIRTemplateInput>,
        args: &[&HIRExpression],
        span: &TokenRange,
    ) -> HMIRExprID {
        let callee = self.push(HMIRExprKind::Def(def), span);
        let mut lowered = self.template_args(template_input);
        for (index, arg) in args.iter().enumerate() {
            let quoted = prototype
                .params
                .get(index)
                .is_some_and(|param| param.value_type.expr);
            lowered.push(if quoted && !self.is_comptime() {
                self.quote_argument(arg)
            } else {
                self.lower_expr(arg)
            });
        }
        let call = self.push(
            HMIRExprKind::Call {
                callee,
                args: lowered,
            },
            span,
        );
        if self.is_comptime() {
            return call;
        }

        let staged = self.push(HMIRExprKind::Comptime(call), span);
        if !prototype.return_type.expr {
            return staged;
        }
        self.push(
            HMIRExprKind::Splice {
                quote: staged,
                args: Vec::new(),
            },
            span,
        )
    }

    fn quote_argument(&mut self, arg: &HIRExpression) -> HMIRExprID {
        match &arg.kind {
            HIRExprKind::ParamStagedExpression { .. } | HIRExprKind::Emit { .. } => {
                self.lower_expr(arg)
            }
            _ => self.lower_quote(&[], arg, &arg.range),
        }
    }

    pub(super) fn construct(
        &mut self,
        union_type: &HIRType,
        template_input: Option<&HIRTemplateInput>,
        variant: CXIdent,
        value: HMIRExprID,
        span: &TokenRange,
    ) -> HMIRExprID {
        let ty = self.constructor_sum(union_type, template_input, span);
        self.aggregate_op(
            HMIRAggregateOp::Initialize {
                ty,
                fields: vec![(Some(variant), value)],
            },
            span,
        )
    }

    fn builtin_call(
        &mut self,
        name: &str,
        args: &[&HIRExpression],
        span: &TokenRange,
    ) -> Option<HMIRExprID> {
        let intrinsic = match (name, args) {
            ("va_start" | "__builtin_va_start", [list, last]) => VAIntrinsic::Start {
                list: self.lower_expr(list),
                last: self.lower_expr(last),
            },
            ("va_end" | "__builtin_va_end", [list]) => VAIntrinsic::End {
                list: self.lower_expr(list),
            },
            _ => return None,
        };
        Some(self.push(HMIRExprKind::Intrinsic(Intrinsic::VA(intrinsic)), span))
    }
}
