pub(crate) mod call;
pub(crate) mod control;
pub(crate) mod literal;
pub(crate) mod op;
pub(crate) mod pattern;

use std::rc::Rc;

use cx_hir::ast::expression::{
    HIRBinOp, HIRBlockKind, HIRClosureParam, HIRExprKind, HIRExpression, HIRInitIndex,
    HIRMemberDesignator, HIRUnpackBinding,
};
use cx_hir::ast::types::HIRType;
use cx_hmir::{
    HMIRAggregateOp, HMIRControlOp, HMIROwnershipOp,
    expr::{
        HMIRExprID, HMIRExprKind, literal::HMIRLiteral,
        op::{HMIRMemberStep, HMIRTypeOp},
    },
    ty::intrinsic::{HMIRIntType, HMIRIntWidth},
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    body::{BodyLowering, ScopeKind, Symbol, lower_reify, lower_static},
    expr::call::lower_call,
    expr::control::{lower_case, lower_for, lower_if, lower_match, lower_switch, lower_while},
    expr::literal::{lower_float_literal, lower_int_literal},
    expr::op::{lower_binop, lower_unop},
    log::log_hir_error,
    ty::{lower_type, staged_signature},
};

pub(crate) fn lower_expr(
    context: &mut BodyLowering<'_>,
    expr: &HIRExpression,
) -> CXResult<HMIRExprID> {
    let span = &expr.range;
    Ok(match &expr.kind {
        HIRExprKind::Taken => {
            return log_hir_error(span, &mir::MALFORMED_HIR, "expression was taken".into());
        }
        HIRExprKind::Then if !context.in_block() => {
            return log_hir_error(
                span,
                &typecheck::INVALID_CONTEXT,
                (
                    "then expression".into(),
                    "a scope that is not a block".into(),
                ),
            );
        }
        HIRExprKind::Then => {
            let statements = context.lower_block_statements()?;
            context.block(statements, false, false, span)
        }
        HIRExprKind::Void => context.push(HMIRExprKind::Literal(HMIRLiteral::Unit), span),

        HIRExprKind::Identifier { name } => lower_identifier(context, name, span)?,
        HIRExprKind::Type(ty) => lower_type(context, ty)?,
        HIRExprKind::Hole => context.hole(span),
        HIRExprKind::ScopeAccess { base, member } => {
            let base = lower_scope_base(context, base)?;
            let member = member.clone();
            context.push(HMIRExprKind::ScopeAccess { base, member }, span)
        }
        HIRExprKind::IntLiteral {
            magnitude,
            base,
            suffix,
        } => lower_int_literal(context, *magnitude, *base, *suffix, span)?,
        HIRExprKind::BoolLiteral(value) => {
            let ty = HMIRIntType::new(HMIRIntWidth::I1, false);
            context.int_literal(ty, *value as i128, span)
        }
        HIRExprKind::FloatLiteral { val, suffix } => {
            lower_float_literal(context, *val, *suffix, span)?
        }
        HIRExprKind::StringLiteral { val } => {
            context.push(HMIRExprKind::Literal(HMIRLiteral::Str(val.clone())), span)
        }

        HIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => lower_if(
            context,
            condition,
            then_branch,
            else_branch.as_deref(),
            true,
            span,
        )?,
        HIRExprKind::Ternary {
            condition,
            then_branch,
            else_branch,
        } => lower_if(
            context,
            condition,
            then_branch,
            Some(else_branch),
            false,
            span,
        )?,
        HIRExprKind::While {
            condition,
            body,
            pre_eval,
        } => lower_while(context, condition, body, *pre_eval, span)?,
        HIRExprKind::For {
            init,
            condition,
            increment,
            body,
        } => lower_for(context, init, condition, increment, body, span)?,
        HIRExprKind::Match { condition, arms } => lower_match(context, condition, arms, span)?,
        HIRExprKind::Switch { condition, body } => lower_switch(context, condition, body, span)?,
        HIRExprKind::Case { value, statement } => {
            lower_case(context, value.as_deref(), statement, span)?
        }

        HIRExprKind::SizeOfExpr { expr: operand } => {
            let operand = lower_expr(context, operand)?;
            context.type_op(HMIRTypeOp::SizeOf(operand), span)
        }
        HIRExprKind::SizeOfType { ty } => {
            let ty = lower_type(context, ty)?;
            context.type_op(HMIRTypeOp::SizeOf(ty), span)
        }
        HIRExprKind::AlignOfExpr { expr: operand } => {
            let operand = lower_expr(context, operand)?;
            context.type_op(HMIRTypeOp::AlignOf(operand), span)
        }
        HIRExprKind::AlignOfType { ty } => {
            let ty = lower_type(context, ty)?;
            context.type_op(HMIRTypeOp::AlignOf(ty), span)
        }
        HIRExprKind::OffsetOf { ty, member } => {
            let ty = lower_type(context, ty)?;
            let member = member
                .iter()
                .map(|step| match step {
                    HIRMemberDesignator::Field(name) => Ok(HMIRMemberStep::Field(name.clone())),
                    HIRMemberDesignator::Index(index) => {
                        Ok(HMIRMemberStep::Index(lower_expr(context, index)?))
                    }
                })
                .collect::<CXResult<_>>()?;
            context.type_op(HMIRTypeOp::OffsetOf { ty, member }, span)
        }

        HIRExprKind::VarDeclaration {
            ty,
            name,
            initial_value,
            linkage: linkage @ (LinkageMode::Static | LinkageMode::Extern),
        } if !context.comptime() => {
            lower_static(context, name, ty, initial_value.as_deref(), *linkage, span)?;
            context.push(HMIRExprKind::Literal(HMIRLiteral::Unit), span)
        }
        HIRExprKind::VarDeclaration {
            ty,
            name,
            initial_value,
            ..
        } => lower_declaration(context, ty, name, initial_value.as_deref(), span)?,
        HIRExprKind::BinOp {
            op: HIRBinOp::MethodCall | HIRBinOp::Pipe(_) | HIRBinOp::BackwardPipe,
            ..
        } => context.scoped_as(ScopeKind::Arguments, |this| {
            lower_call(this, expr, Vec::new(), Vec::new())
        })?,
        HIRExprKind::BinOp { lhs, rhs, op } => lower_binop(context, op, lhs, rhs, span)?,
        HIRExprKind::UnOp { operand, operator } => lower_unop(context, operator, operand, span)?,

        HIRExprKind::Block { exprs, kind } => lower_block(context, exprs, *kind, span)?,

        HIRExprKind::Defer { expr: inner } => {
            let inner = lower_expr(context, inner)?;
            context.control(HMIRControlOp::Defer(inner), span)
        }
        HIRExprKind::Unsafe { expr: inner } => {
            let inner = lower_expr(context, inner)?;
            context.control(HMIRControlOp::Unsafe(inner), span)
        }
        HIRExprKind::Closure { params, body } => lower_closure(context, params, body, span)?,
        HIRExprKind::Emit { expr: inner } => lower_quote(context, &[], inner, span)?,

        HIRExprKind::Break => context.control(HMIRControlOp::Break, span),
        HIRExprKind::Continue => context.control(HMIRControlOp::Continue, span),
        HIRExprKind::Goto { name } => context.control(HMIRControlOp::Goto(name.clone()), span),
        HIRExprKind::IndirectGoto { target } => {
            let target = lower_expr(context, target)?;
            context.control(HMIRControlOp::IndirectGoto(target), span)
        }
        HIRExprKind::LabelAddress { name } => context.label_address(name, span),
        HIRExprKind::Label { name, statement } => {
            let name = name.clone();
            let body = lower_expr(context, statement)?;
            context.push(HMIRExprKind::Label { name, body }, span)
        }
        HIRExprKind::Return { value } => {
            let value = lower_optional(context, value.as_deref())?;
            context.control(HMIRControlOp::Return(value), span)
        }
        HIRExprKind::Yield { value } => {
            let value = lower_optional(context, value.as_deref())?;
            context.control(HMIRControlOp::Yield(value), span)
        }

        HIRExprKind::Leak { expr: inner } => {
            let inner = lower_expr(context, inner)?;
            context.ownership(HMIROwnershipOp::Leak(inner), span)
        }
        HIRExprKind::Reify { signature, value } => lower_reify(context, signature, value, span)?,
        HIRExprKind::NamedArgument { .. } => {
            return log_hir_error(
                span,
                &typecheck::INVALID_CONTEXT,
                ("a named argument".into(), "anything but a call".into()),
            );
        }
        HIRExprKind::Adopt { expr: inner } => {
            let inner = lower_expr(context, inner)?;
            context.ownership(HMIROwnershipOp::Adopt(inner), span)
        }
        HIRExprKind::Unpack {
            expr: inner,
            bindings,
        } => lower_unpack(context, inner, bindings, span)?,

        HIRExprKind::InitializerList { indices } => {
            lower_initializer(context, None, indices, span)?
        }

        HIRExprKind::VaArg { list, ty } => {
            let list = lower_expr(context, list)?;
            let ty = lower_type(context, ty)?;
            context.push(
                HMIRExprKind::Intrinsic(Intrinsic::VA(VAIntrinsic::Arg { list, ty })),
                span,
            )
        }
    })
}

fn lower_optional(
    context: &mut BodyLowering<'_>,
    expr: Option<&HIRExpression>,
) -> CXResult<Option<HMIRExprID>> {
    expr.map(|expr| lower_expr(context, expr)).transpose()
}

fn lower_block(
    context: &mut BodyLowering<'_>,
    exprs: &Rc<[HIRExpression]>,
    kind: HIRBlockKind,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let yields = match kind {
        HIRBlockKind::Sequence => {
            let statements = exprs
                .iter()
                .map(|expr| lower_expr(context, expr))
                .collect::<CXResult<_>>()?;
            return Ok(context.block(statements, false, false, span));
        }
        HIRBlockKind::Statement => false,
        HIRBlockKind::Expression => true,
    };
    let statements = lower_scope_statements(context, exprs)?;

    Ok(context.block(statements, true, yields, span))
}

pub(crate) fn lower_scope_statements(
    context: &mut BodyLowering<'_>,
    statements: &Rc<[HIRExpression]>,
) -> CXResult<Vec<HMIRExprID>> {
    let scope = ScopeKind::Block {
        statements: statements.clone(),
        next: 0,
    };
    context.scoped_as(scope, BodyLowering::lower_block_statements)
}

pub(crate) fn lower_identifier(
    context: &mut BodyLowering<'_>,
    name: &QualifiedName,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    Ok(match context.lookup(name, None) {
        Symbol::Local(binding) => {
            let local = context.push(HMIRExprKind::Local(binding.local()), span);
            if !binding.is_quoted() || context.comptime() {
                return Ok(local);
            }

            let args = Vec::new();
            context.push(HMIRExprKind::Splice { quote: local, args }, span)
        }
        Symbol::Def(def) => context.push(HMIRExprKind::Def(def), span),
        Symbol::Name => context.push(
            HMIRExprKind::Name {
                name: name.clone(),
                tag: None,
            },
            span,
        ),
    })
}

// The type a 'base::variant(...)' constructor builds. A base left to the compiler, '_' or a
// generator applied to holes, is completed from the type the value is expected to have.
fn lower_scope_base(
    context: &mut BodyLowering<'_>,
    base: &HIRExpression,
) -> CXResult<HMIRExprID> {
    match &base.kind {
        HIRExprKind::Hole => Ok(context.hole(&base.range)),
        HIRExprKind::Type(ty) => lower_type(context, ty),
        _ => lower_expr(context, base),
    }
}

fn lower_declaration(
    context: &mut BodyLowering<'_>,
    ty: &HIRType,
    name: &CXIdent,
    initial_value: Option<&HIRExpression>,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let local_ty = lower_type(context, ty)?;
    let local = context.declare_local(Some(name), local_ty, span);
    let initializer = initial_value
        .map(|value| lower_initial_value(context, local_ty, value))
        .transpose()?;

    Ok(context.push(HMIRExprKind::Let { local, initializer }, span))
}

// An initializer list shares the lowered type of its declaration: lowering an aggregate declared
// in place a second time would make a distinct type of it
pub(crate) fn lower_initial_value(
    context: &mut BodyLowering<'_>,
    ty: HMIRExprID,
    value: &HIRExpression,
) -> CXResult<HMIRExprID> {
    match &value.kind {
        HIRExprKind::InitializerList { indices } => {
            lower_initializer(context, Some(ty), indices, &value.range)
        }
        _ => lower_expr(context, value),
    }
}

fn lower_initializer(
    context: &mut BodyLowering<'_>,
    ty: Option<HMIRExprID>,
    indices: &[HIRInitIndex],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let ty = ty.unwrap_or_else(|| context.hole(span));
    let fields = indices
        .iter()
        .map(|index| {
            let value = match &index.value.kind {
                HIRExprKind::InitializerList { indices } => {
                    lower_initializer(context, None, indices, &index.value.range)?
                }
                _ => lower_expr(context, &index.value)?,
            };
            Ok((index.name.as_deref().map(CXIdent::from), value))
        })
        .collect::<CXResult<_>>()?;

    Ok(context.aggregate_op(HMIRAggregateOp::Initialize { ty, fields }, span))
}

fn lower_unpack(
    context: &mut BodyLowering<'_>,
    inner: &HIRExpression,
    bindings: &[HIRUnpackBinding],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let value = lower_expr(context, inner)?;
    let bindings = bindings
        .iter()
        .map(|binding| {
            let ty = context.hole(span);
            let local = context.declare_local(Some(&binding.binding), ty, span);
            (binding.field.clone(), local)
        })
        .collect();

    Ok(context.aggregate_op(HMIRAggregateOp::Unpack { value, bindings }, span))
}

// A closure is an anonymous comptime function; the code it produces is its 'emit'
fn lower_closure(
    context: &mut BodyLowering<'_>,
    params: &[HIRClosureParam],
    body: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    match &body.kind {
        HIRExprKind::Emit { expr } => lower_quote(context, params, expr, span),
        // 'then' emits the rest of the enclosing block
        HIRExprKind::Then => lower_quote(context, params, body, span),
        _ => log_hir_error(
            span,
            &typecheck::TYPE_REQUIREMENT,
            (
                "closure body".into(),
                "an emit expression or a block".into(),
                None,
            ),
        ),
    }
}

pub(crate) fn lower_quote(
    context: &mut BodyLowering<'_>,
    params: &[HIRClosureParam],
    body: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    // Parameter types are written in the stage the closure appears in
    let types = params
        .iter()
        .map(|param| match &param.ty {
            Some(ty) => match staged_signature(ty) {
                Some((_, carried)) => lower_type(context, carried),
                None => lower_type(context, ty),
            },
            None => Ok(context.hole(span)),
        })
        .collect::<CXResult<Vec<_>>>()?;

    context.with_stage(false, |this| {
        this.scoped_as(ScopeKind::Staged, |this| {
            let params = params
                .iter()
                .zip(types)
                .map(|(param, ty)| this.declare_local(Some(&param.name), ty, span))
                .collect();
            let body = lower_expr(this, body)?;

            Ok(this.push(HMIRExprKind::Quote { params, body }, span))
        })
    })
}
