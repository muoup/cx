use std::slice::Iter;

use cx_hir::ast::expression::{HIRExprKind, HIRExpression};
use cx_hmir::expr::{
    HMIRExprID, HMIRExprKind,
    literal::HMIRLiteral,
    op::{HMIROp, HMIRTypeOp},
};
use cx_intrinsics::{Intrinsic, IntrinsicSource};
use cx_log::{
    CXResult,
    catalogue::{parse, typecheck},
    error::CXError,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{body::BodyLowering, expr::lower_expr, log::log_hir_error};

pub(crate) fn lower_intrinsic<'a>(
    context: &mut BodyLowering<'a>,
    path: &CXIdent,
    args: &'a [HIRExpression],
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let subject = format!("intrinsic '@{path}'");
    let mut source = Arguments {
        context,
        args: args.iter(),
        subject: &subject,
        demanded: 0,
        span,
    };
    let kind = match path.as_str().starts_with("type.") {
        true => HMIRTypeOp::from_path(path.as_str(), &mut source)?
            .map(|op| HMIRExprKind::Op(HMIROp::Type(op))),
        false => Intrinsic::from_path(path.as_str(), &mut source)?.map(HMIRExprKind::Intrinsic),
    };
    let Some(kind) = kind else {
        return log_hir_error(span, &parse::UNKNOWN_NAME, ("intrinsic".into(), format!("@{path}")));
    };
    if source.demanded != args.len() {
        let count = (subject.clone(), source.demanded, args.len(), false);
        return log_hir_error(span, &typecheck::ARGUMENT_COUNT, count);
    }
    Ok(source.context.push(kind, span))
}

struct Arguments<'c, 'a> {
    context: &'c mut BodyLowering<'a>,
    args: Iter<'a, HIRExpression>,
    subject: &'c str,
    demanded: usize,
    span: &'c TokenRange,
}

impl<'a> Arguments<'_, 'a> {
    fn next(&mut self) -> Option<&'a HIRExpression> {
        self.demanded += 1;
        self.args.next()
    }

    fn lowered(&mut self) -> CXResult<HMIRExprID> {
        match self.next() {
            Some(arg) => lower_expr(self.context, arg),
            None => Ok(self.context.push(HMIRExprKind::Literal(HMIRLiteral::Unit), self.span)),
        }
    }

    fn not_literal<T>(&self, arg: &HIRExpression, literal: &str) -> CXResult<T> {
        let subject = format!("argument {} of {}", self.demanded, self.subject);
        log_hir_error(&arg.range, &typecheck::UNEXPECTED_KIND, (subject, literal.into()))
    }
}

impl IntrinsicSource<HMIRExprID, HMIRExprID> for Arguments<'_, '_> {
    type Error = CXError;

    fn value(&mut self) -> CXResult<HMIRExprID> {
        self.lowered()
    }

    fn ty(&mut self) -> CXResult<HMIRExprID> {
        self.lowered()
    }

    fn bool(&mut self) -> CXResult<bool> {
        match self.next() {
            Some(HIRExpression { kind: HIRExprKind::BoolLiteral(value), .. }) => Ok(*value),
            Some(arg) => self.not_literal(arg, "a bool literal"),
            None => Ok(false),
        }
    }

    fn string(&mut self) -> CXResult<Option<String>> {
        let Some(arg) = self.args.as_slice().first() else {
            return Ok(None);
        };
        self.next();
        match &arg.kind {
            HIRExprKind::StringLiteral { val } => Ok(Some(val.clone())),
            _ => self.not_literal(arg, "a string literal"),
        }
    }
}
