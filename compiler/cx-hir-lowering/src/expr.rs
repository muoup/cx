mod call;
mod control;
mod literal;
mod op;
mod pattern;

use cx_hir::ast::expression::{
    HIRBinOp, HIRBlockKind, HIRExprKind, HIRExpression, HIRInitIndex, HIRUnpackBinding,
};
use cx_hir::ast::{template::HIRTemplateInput, types::HIRType};
use cx_hmir::{
    HMIRAggregateOp, HMIRBlockKind, HMIRConstant, HMIRControlOp, HMIRExprID, HMIRExprKind,
    HMIRNativeOp, HMIROwnershipOp, HMIRTypeOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    resolve::GlobalSymbol,
};

impl BodyLowering<'_> {
    pub(crate) fn lower_expr(&mut self, expr: &HIRExpression) -> HMIRExprID {
        let span = &expr.range;
        match &expr.kind {
            HIRExprKind::Taken | HIRExprKind::Then => self.error(span),
            HIRExprKind::Void => self.push(HMIRExprKind::Constant(HMIRConstant::Unit), span),

            HIRExprKind::Identifier {
                name,
                template_input,
            } => self.identifier(name, template_input.as_ref(), span),
            HIRExprKind::IntLiteral {
                magnitude, suffix, ..
            } => self.int_literal(*magnitude, *suffix, span),
            HIRExprKind::BoolLiteral(value) => {
                self.push(HMIRExprKind::Constant(HMIRConstant::Bool(*value)), span)
            }
            HIRExprKind::FloatLiteral { val, suffix } => self.float_literal(*val, *suffix, span),
            HIRExprKind::StringLiteral { val } => {
                self.push(HMIRExprKind::Constant(HMIRConstant::Str(val.clone())), span)
            }

            HIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => self.lower_if(condition, then_branch, else_branch.as_deref(), span),
            HIRExprKind::Ternary {
                condition,
                then_branch,
                else_branch,
            } => self.lower_if(condition, then_branch, Some(else_branch), span),
            HIRExprKind::While {
                condition,
                body,
                pre_eval,
            } => self.lower_while(condition, body, *pre_eval, span),
            HIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => self.lower_for(init, condition, increment, body, span),
            HIRExprKind::Match { condition, arms } => self.lower_match(condition, arms, span),
            HIRExprKind::Switch {
                condition,
                block,
                cases,
                default_case,
            } => self.lower_switch(condition, block, cases, *default_case, span),

            HIRExprKind::SizeOfExpr { expr: operand } => {
                let operand = self.lower_expr(operand);
                self.native(HMIRNativeOp::Type(HMIRTypeOp::SizeOf(operand)), span)
            }
            HIRExprKind::SizeOfType { ty } => {
                let ty = self.lower_type(ty);
                self.native(HMIRNativeOp::Type(HMIRTypeOp::SizeOf(ty)), span)
            }
            HIRExprKind::AlignOfExpr { expr: operand } => {
                let operand = self.lower_expr(operand);
                self.native(HMIRNativeOp::Type(HMIRTypeOp::AlignOf(operand)), span)
            }
            HIRExprKind::AlignOfType { ty } => {
                let ty = self.lower_type(ty);
                self.native(HMIRNativeOp::Type(HMIRTypeOp::AlignOf(ty)), span)
            }

            HIRExprKind::VarDeclaration {
                ty,
                name,
                initial_value,
                ..
            } => self.lower_declaration(ty, name, initial_value.as_deref(), span),
            HIRExprKind::BinOp {
                op: HIRBinOp::MethodCall | HIRBinOp::Pipe | HIRBinOp::BackwardPipe,
                ..
            } => self.lower_call(expr, Vec::new(), Vec::new()),
            HIRExprKind::BinOp { lhs, rhs, op } => self.lower_binop(op, lhs, rhs, span),
            HIRExprKind::UnOp { operand, operator } => self.lower_unop(operator, operand, span),

            HIRExprKind::Block { exprs, kind } => self.lower_block(exprs, *kind, span),

            HIRExprKind::Defer { expr: inner } => {
                let inner = self.lower_expr(inner);
                self.control(HMIRControlOp::Defer(inner), span)
            }
            HIRExprKind::Unsafe { expr: inner } => {
                let inner = self.lower_expr(inner);
                self.control(HMIRControlOp::Unsafe(inner), span)
            }
            HIRExprKind::ParamStagedExpression { params, body } => {
                self.lower_quote(params, body, span)
            }
            HIRExprKind::Emit { expr: inner } => self.lower_quote(&[], inner, span),

            HIRExprKind::Break => self.control(HMIRControlOp::Break, span),
            HIRExprKind::Continue => self.control(HMIRControlOp::Continue, span),
            HIRExprKind::Goto { name } => self.control(HMIRControlOp::Goto(name.clone()), span),
            HIRExprKind::Label { name, statement } => {
                let body = self.lower_expr(statement);
                self.push(
                    HMIRExprKind::Label {
                        name: name.clone(),
                        body,
                    },
                    span,
                )
            }
            HIRExprKind::Return { value } => {
                let value = value.as_deref().map(|value| self.lower_expr(value));
                self.control(HMIRControlOp::Return(value), span)
            }
            HIRExprKind::Yield { value } => {
                let value = value.as_deref().map(|value| self.lower_expr(value));
                self.control(HMIRControlOp::Yield(value), span)
            }

            HIRExprKind::Leak { expr: inner } => {
                let inner = self.lower_expr(inner);
                self.ownership(HMIROwnershipOp::Leak(inner), span)
            }
            HIRExprKind::Adopt { expr: inner } => {
                let inner = self.lower_expr(inner);
                self.ownership(HMIROwnershipOp::Adopt(inner), span)
            }
            HIRExprKind::Unpack {
                expr: inner,
                bindings,
            } => self.lower_unpack(inner, bindings, span),

            HIRExprKind::InitializerList { indices } => self.lower_initializer(None, indices, span),

            HIRExprKind::VaArg { list, ty } => {
                let list = self.lower_expr(list);
                let ty = self.lower_type(ty);
                self.push(
                    HMIRExprKind::Intrinsic(Intrinsic::VA(VAIntrinsic::Arg { list, ty })),
                    span,
                )
            }
        }
    }

    pub(crate) fn lower_block(
        &mut self,
        exprs: &[HIRExpression],
        kind: HIRBlockKind,
        span: &TokenRange,
    ) -> HMIRExprID {
        let lower = |this: &mut Self| -> Vec<HMIRExprID> {
            exprs.iter().map(|expr| this.lower_expr(expr)).collect()
        };
        let (kind, statements) = match kind {
            HIRBlockKind::Sequence => (HMIRBlockKind::Sequence, lower(self)),
            HIRBlockKind::Statement => (HMIRBlockKind::Scope, self.scoped(lower)),
            HIRBlockKind::Expression => (HMIRBlockKind::Yield, self.scoped(lower)),
        };
        self.block(kind, statements, span)
    }

    pub(crate) fn control(&mut self, op: HMIRControlOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::Control(op), span)
    }

    pub(crate) fn ownership(&mut self, op: HMIROwnershipOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::OwnershipOp(op), span)
    }

    pub(crate) fn aggregate_op(&mut self, op: HMIRAggregateOp, span: &TokenRange) -> HMIRExprID {
        self.native(HMIRNativeOp::AggregateOp(op), span)
    }

    fn identifier(
        &mut self,
        name: &QualifiedName,
        template_input: Option<&HIRTemplateInput>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let value = match self.lookup(name, None) {
            Symbol::Local(binding) => {
                let local = self.push(HMIRExprKind::Local(binding.local()), span);
                if !binding.is_quoted() || self.is_comptime() {
                    local
                } else {
                    self.push(
                        HMIRExprKind::Splice {
                            quote: local,
                            args: Vec::new(),
                        },
                        span,
                    )
                }
            }
            Symbol::Global(GlobalSymbol::Primitive(desc)) => self.type_constant(desc, span),
            Symbol::Global(GlobalSymbol::Def(def) | GlobalSymbol::ComptimeFunction(def, _)) => {
                self.push(HMIRExprKind::Def(def), span)
            }
            Symbol::Global(GlobalSymbol::Constructor(data, variant)) => {
                let unit = self.push(HMIRExprKind::Constant(HMIRConstant::Unit), span);
                return self.construct(&data.union_type, template_input, variant, unit, span);
            }
        };
        self.instantiate(value, template_input, span)
    }

    fn lower_declaration(
        &mut self,
        ty: &HIRType,
        name: &CXIdent,
        initial_value: Option<&HIRExpression>,
        span: &TokenRange,
    ) -> HMIRExprID {
        let local_ty = self.lower_type(ty);
        let initializer = initial_value.map(|value| match &value.kind {
            HIRExprKind::InitializerList { indices } => {
                self.lower_initializer(Some(ty), indices, &value.range)
            }
            _ => self.lower_expr(value),
        });
        let local = self.declare_local(Some(name), local_ty, span);
        self.push(HMIRExprKind::Let { local, initializer }, span)
    }

    fn lower_initializer(
        &mut self,
        ty: Option<&HIRType>,
        indices: &[HIRInitIndex],
        span: &TokenRange,
    ) -> HMIRExprID {
        let ty = match ty {
            Some(ty) => self.lower_type(ty),
            None => self.hole(span),
        };
        let fields = indices
            .iter()
            .map(|index| {
                let value = match &index.value.kind {
                    HIRExprKind::InitializerList { indices } => {
                        self.lower_initializer(None, indices, &index.value.range)
                    }
                    _ => self.lower_expr(&index.value),
                };
                (index.name.as_deref().map(CXIdent::from), value)
            })
            .collect();
        self.aggregate_op(HMIRAggregateOp::Initialize { ty, fields }, span)
    }

    fn lower_unpack(
        &mut self,
        inner: &HIRExpression,
        bindings: &[HIRUnpackBinding],
        span: &TokenRange,
    ) -> HMIRExprID {
        let value = self.lower_expr(inner);
        let source_ty = self.hole(span);
        let source = self.declare_local(None, source_ty, span);
        let mut statements = vec![self.push(
            HMIRExprKind::Let {
                local: source,
                initializer: Some(value),
            },
            span,
        )];
        for binding in bindings {
            let base = self.push(HMIRExprKind::Local(source), span);
            let field = self.aggregate_op(
                HMIRAggregateOp::Member {
                    base,
                    name: binding.field.clone(),
                },
                span,
            );
            let ty = self.hole(span);
            let local = self.declare_local(Some(&binding.binding), ty, span);
            statements.push(self.push(
                HMIRExprKind::Let {
                    local,
                    initializer: Some(field),
                },
                span,
            ));
        }
        self.block(HMIRBlockKind::Sequence, statements, span)
    }

    fn lower_quote(
        &mut self,
        params: &[CXIdent],
        body: &HIRExpression,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.with_stage(false, |this| {
            this.scoped(|this| {
                let params = params
                    .iter()
                    .map(|param| {
                        let ty = this.hole(span);
                        this.declare_local(Some(param), ty, span)
                    })
                    .collect();
                let body = this.lower_expr(body);
                this.push(HMIRExprKind::Quote { params, body }, span)
            })
        })
    }
}
