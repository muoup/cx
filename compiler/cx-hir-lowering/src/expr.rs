pub(crate) mod call;
pub(crate) mod control;
pub(crate) mod literal;
pub(crate) mod op;
pub(crate) mod pattern;

use cx_hir::ast::expression::{
    HIRBinOp, HIRBlockKind, HIRClosureParam, HIRExprKind, HIRExpression, HIRInitIndex,
    HIRUnpackBinding,
};
use cx_hir::ast::types::HIRType;
use cx_hmir::{
    HMIRAggregateOp, HMIRBlockKind, HMIRConstant, HMIRControlOp, HMIRExprID, HMIRExprKind,
    HMIRNativeOp, HMIROwnershipOp, HMIRTypeOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    body::{BodyLowering, Symbol, lower_reify, lower_static, lower_variant_constructor},
    def::is_void,
    expr::call::{lower_call, lower_construct, lower_scope_base},
    expr::control::{lower_for, lower_if, lower_match, lower_switch, lower_while},
    expr::literal::{lower_float_literal, lower_int_literal},
    expr::op::{lower_binop, lower_unop},
    resolve::GlobalSymbol,
    ty::{lower_constructor_sum, lower_type, staged_signature},
};

pub(crate) fn lower_expr(cx: &mut BodyLowering<'_>, expr: &HIRExpression) -> HMIRExprID {
    let span = &expr.range;
    match &expr.kind {
        HIRExprKind::Taken | HIRExprKind::Then => cx.error(span),
        HIRExprKind::Void => cx.push(HMIRExprKind::Constant(HMIRConstant::Unit), span),

        HIRExprKind::Identifier { name } => lower_identifier(cx, name, span),
        HIRExprKind::Type(ty) => lower_type(cx, ty),
        HIRExprKind::Hole => cx.hole(span),
        HIRExprKind::ScopeAccess { base, member } => {
            let sum = lower_scope_base(cx, base);
            lower_variant_constructor(cx, sum, member, span)
        }
        HIRExprKind::IntLiteral {
            magnitude,
            base,
            suffix,
        } => lower_int_literal(cx, *magnitude, *base, *suffix, span),
        HIRExprKind::BoolLiteral(value) => {
            cx.push(HMIRExprKind::Constant(HMIRConstant::Bool(*value)), span)
        }
        HIRExprKind::FloatLiteral { val, suffix } => lower_float_literal(cx, *val, *suffix, span),
        HIRExprKind::StringLiteral { val } => {
            cx.push(HMIRExprKind::Constant(HMIRConstant::Str(val.clone())), span)
        }

        HIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(cx, condition, then_branch, else_branch.as_deref(), span),
        HIRExprKind::Ternary {
            condition,
            then_branch,
            else_branch,
        } => lower_if(cx, condition, then_branch, Some(else_branch), span),
        HIRExprKind::While {
            condition,
            body,
            pre_eval,
        } => lower_while(cx, condition, body, *pre_eval, span),
        HIRExprKind::For {
            init,
            condition,
            increment,
            body,
        } => lower_for(cx, init, condition, increment, body, span),
        HIRExprKind::Match { condition, arms } => lower_match(cx, condition, arms, span),
        HIRExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => lower_switch(cx, condition, block, cases, *default_case, span),

        HIRExprKind::SizeOfExpr { expr: operand } => {
            let operand = lower_expr(cx, operand);
            cx.native(HMIRNativeOp::Type(HMIRTypeOp::SizeOf(operand)), span)
        }
        HIRExprKind::SizeOfType { ty } => {
            let ty = lower_type(cx, ty);
            cx.native(HMIRNativeOp::Type(HMIRTypeOp::SizeOf(ty)), span)
        }
        HIRExprKind::AlignOfExpr { expr: operand } => {
            let operand = lower_expr(cx, operand);
            cx.native(HMIRNativeOp::Type(HMIRTypeOp::AlignOf(operand)), span)
        }
        HIRExprKind::AlignOfType { ty } => {
            let ty = lower_type(cx, ty);
            cx.native(HMIRNativeOp::Type(HMIRTypeOp::AlignOf(ty)), span)
        }

        HIRExprKind::VarDeclaration {
            ty,
            name,
            initial_value,
            linkage: LinkageMode::Static,
        } if !cx.is_comptime() => {
            lower_static(cx, name, ty, initial_value.as_deref(), span);
            cx.push(HMIRExprKind::Constant(HMIRConstant::Unit), span)
        }
        HIRExprKind::VarDeclaration {
            ty,
            name,
            initial_value,
            ..
        } => lower_declaration(cx, ty, name, initial_value.as_deref(), span),
        HIRExprKind::BinOp {
            op: HIRBinOp::MethodCall | HIRBinOp::Pipe(_) | HIRBinOp::BackwardPipe,
            ..
        } => lower_call(cx, expr, Vec::new(), Vec::new()),
        HIRExprKind::BinOp { lhs, rhs, op } => lower_binop(cx, op, lhs, rhs, span),
        HIRExprKind::UnOp { operand, operator } => lower_unop(cx, operator, operand, span),

        HIRExprKind::Block { exprs, kind } => lower_block(cx, exprs, *kind, span),

        HIRExprKind::Defer { expr: inner } => {
            let inner = lower_expr(cx, inner);
            cx.control(HMIRControlOp::Defer(inner), span)
        }
        HIRExprKind::Unsafe { expr: inner } => {
            let inner = lower_expr(cx, inner);
            cx.control(HMIRControlOp::Unsafe(inner), span)
        }
        HIRExprKind::Closure { params, body } => lower_closure(cx, params, body, span),
        HIRExprKind::Emit { expr: inner } => lower_quote(cx, &[], inner, span),

        HIRExprKind::Break => cx.control(HMIRControlOp::Break, span),
        HIRExprKind::Continue => cx.control(HMIRControlOp::Continue, span),
        HIRExprKind::Goto { name } => cx.control(HMIRControlOp::Goto(name.clone()), span),
        HIRExprKind::Label { name, statement } => {
            let body = lower_expr(cx, statement);
            cx.push(
                HMIRExprKind::Label {
                    name: name.clone(),
                    body,
                },
                span,
            )
        }
        HIRExprKind::Return { value } => {
            let value = value.as_deref().map(|value| lower_expr(cx, value));
            cx.control(HMIRControlOp::Return(value), span)
        }
        HIRExprKind::Yield { value } => {
            let value = value.as_deref().map(|value| lower_expr(cx, value));
            cx.control(HMIRControlOp::Yield(value), span)
        }

        HIRExprKind::Leak { expr: inner } => {
            let inner = lower_expr(cx, inner);
            cx.ownership(HMIROwnershipOp::Leak(inner), span)
        }
        HIRExprKind::Reify { signature, value } => lower_reify(cx, signature, value, span),
        HIRExprKind::Adopt { expr: inner } => {
            let inner = lower_expr(cx, inner);
            cx.ownership(HMIROwnershipOp::Adopt(inner), span)
        }
        HIRExprKind::Unpack {
            expr: inner,
            bindings,
        } => lower_unpack(cx, inner, bindings, span),

        HIRExprKind::InitializerList { indices } => lower_initializer(cx, None, indices, span),

        HIRExprKind::VaArg { list, ty } => {
            let list = lower_expr(cx, list);
            let ty = lower_type(cx, ty);
            cx.push(
                HMIRExprKind::Intrinsic(Intrinsic::VA(VAIntrinsic::Arg { list, ty })),
                span,
            )
        }
    }
}

fn lower_block(
    cx: &mut BodyLowering<'_>,
    exprs: &[HIRExpression],
    kind: HIRBlockKind,
    span: &TokenRange,
) -> HMIRExprID {
    let lower = |this: &mut BodyLowering<'_>| -> Vec<HMIRExprID> {
        exprs.iter().map(|expr| lower_expr(this, expr)).collect()
    };
    let (kind, statements) = match kind {
        HIRBlockKind::Sequence => (HMIRBlockKind::Sequence, lower(cx)),
        HIRBlockKind::Statement => (HMIRBlockKind::Scope, cx.scoped(lower)),
        HIRBlockKind::Expression => (HMIRBlockKind::Yield, cx.scoped(lower)),
    };
    cx.block(kind, statements, span)
}

pub(crate) fn lower_identifier(
    cx: &mut BodyLowering<'_>,
    name: &QualifiedName,
    span: &TokenRange,
) -> HMIRExprID {
    match cx.lookup(name, None) {
        Symbol::Local(binding) => {
            let local = cx.push(HMIRExprKind::Local(binding.local()), span);
            if !binding.is_quoted() || cx.is_comptime() {
                local
            } else {
                cx.push(
                    HMIRExprKind::Splice {
                        quote: local,
                        args: Vec::new(),
                    },
                    span,
                )
            }
        }
        Symbol::Global(GlobalSymbol::Primitive(desc)) => cx.type_constant(desc, span),
        Symbol::Global(
            GlobalSymbol::Def(def)
            | GlobalSymbol::Function(def)
            | GlobalSymbol::ComptimeFunction(def, ..),
        ) => cx.push(HMIRExprKind::Def(def), span),
        Symbol::Global(GlobalSymbol::Constructor(data, variant, def)) => {
            let payload = cx.resolver().constructor_payload(&data);
            if payload.is_none_or(|payload| is_void(cx, &payload)) {
                let unit = cx.push(HMIRExprKind::Constant(HMIRConstant::Unit), span);
                let sum = lower_constructor_sum(cx, &data.union_type, span);
                return lower_construct(cx, sum, variant, unit, span);
            }
            cx.push(HMIRExprKind::Def(def), span)
        }
    }
}

fn lower_declaration(
    cx: &mut BodyLowering<'_>,
    ty: &HIRType,
    name: &CXIdent,
    initial_value: Option<&HIRExpression>,
    span: &TokenRange,
) -> HMIRExprID {
    let local_ty = lower_type(cx, ty);
    let initializer = initial_value.map(|value| lower_initial_value(cx, ty, value));
    let local = cx.declare_local(Some(name), local_ty, span);
    cx.push(HMIRExprKind::Let { local, initializer }, span)
}

pub(crate) fn lower_initial_value(
    cx: &mut BodyLowering<'_>,
    ty: &HIRType,
    value: &HIRExpression,
) -> HMIRExprID {
    match &value.kind {
        HIRExprKind::InitializerList { indices } => {
            lower_initializer(cx, Some(ty), indices, &value.range)
        }
        _ => lower_expr(cx, value),
    }
}

fn lower_initializer(
    cx: &mut BodyLowering<'_>,
    ty: Option<&HIRType>,
    indices: &[HIRInitIndex],
    span: &TokenRange,
) -> HMIRExprID {
    let ty = match ty {
        Some(ty) => lower_type(cx, ty),
        None => cx.hole(span),
    };
    let fields = indices
        .iter()
        .map(|index| {
            let value = match &index.value.kind {
                HIRExprKind::InitializerList { indices } => {
                    lower_initializer(cx, None, indices, &index.value.range)
                }
                _ => lower_expr(cx, &index.value),
            };
            (index.name.as_deref().map(CXIdent::from), value)
        })
        .collect();
    cx.aggregate_op(HMIRAggregateOp::Initialize { ty, fields }, span)
}

fn lower_unpack(
    cx: &mut BodyLowering<'_>,
    inner: &HIRExpression,
    bindings: &[HIRUnpackBinding],
    span: &TokenRange,
) -> HMIRExprID {
    let value = lower_expr(cx, inner);
    let bindings = bindings
        .iter()
        .map(|binding| {
            let ty = cx.hole(span);
            let local = cx.declare_local(Some(&binding.binding), ty, span);
            (binding.field.clone(), local)
        })
        .collect();
    cx.aggregate_op(HMIRAggregateOp::Unpack { value, bindings }, span)
}

// A closure is an anonymous comptime function; the code it produces is its 'emit'
fn lower_closure(
    cx: &mut BodyLowering<'_>,
    params: &[HIRClosureParam],
    body: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    match &body.kind {
        HIRExprKind::Emit { expr } => lower_quote(cx, params, expr, span),
        // 'then' has been replaced by the rest of the enclosing block, which it emits
        HIRExprKind::Block {
            kind: HIRBlockKind::Sequence,
            ..
        } => lower_quote(cx, params, body, span),
        _ => cx.error(span),
    }
}

pub(crate) fn lower_quote(
    cx: &mut BodyLowering<'_>,
    params: &[HIRClosureParam],
    body: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    // Parameter types are written in the stage the closure appears in
    let types = params
        .iter()
        .map(|param| match &param.ty {
            Some(ty) => match staged_signature(ty) {
                Some((_, carried)) => lower_type(cx, carried),
                None => lower_type(cx, ty),
            },
            None => cx.hole(span),
        })
        .collect::<Vec<_>>();
    cx.with_stage(false, |this| {
        this.scoped(|this| {
            let params = params
                .iter()
                .zip(types)
                .map(|(param, ty)| this.declare_local(Some(&param.name), ty, span))
                .collect();
            let body = lower_expr(this, body);
            this.push(HMIRExprKind::Quote { params, body }, span)
        })
    })
}
