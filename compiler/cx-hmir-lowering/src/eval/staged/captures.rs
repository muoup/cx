use std::{cell::RefCell, collections::HashSet};

use cx_hmir::{
    HMIRAggregateOp, HMIRBody, HMIRControlOp, HMIROp, HMIROwnershipOp, HMIRPattern,
    body::HMIRLocalID,
    expr::{
        HMIRExprID, HMIRExprKind,
        op::{HMIRMemberStep, HMIRTypeOp},
    },
    ty::aggregate::HMIRDeclaredMove,
};
use cx_log::{CXResult, catalogue::mir};

use crate::{
    env::function::{FnContext, LocalLoweringContext},
    eval::{coerce, comptime, flow},
    log::hmir_error,
    value::Bindings,
};

pub(super) fn collect(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> CXResult<Bindings> {
    let mut walk = Locals {
        body,
        seen: HashSet::new(),
        references: HashSet::new(),
        bound: HashSet::new(),
    };
    for local in params {
        walk.bind(*local);
    }
    walk.visit(root);
    let mut captures = Bindings::new();
    for local in walk.references.difference(&walk.bound) {
        let value = context.get_local(local).cloned().ok_or_else(|| {
            hmir_error(
                body.local(*local).span(),
                &mir::COMPTIME_UNAVAILABLE,
                "this captured binding".into(),
            )
        })?;
        let value = match context.context() {
            FnContext::Runtime(_) => value,
            FnContext::Comptime(_) => {
                let span = body.local(*local).span();
                let value = coerce::std_rvalue(context, value, span)?;
                flow::top(comptime::export(context, value, span))?
            }
        };
        captures.insert(*local, value);
    }
    Ok(captures)
}

struct Locals<'a> {
    body: &'a HMIRBody,
    seen: HashSet<HMIRExprID>,
    references: HashSet<HMIRLocalID>,
    bound: HashSet<HMIRLocalID>,
}

impl Locals<'_> {
    fn bind(&mut self, local: HMIRLocalID) {
        self.bound.insert(local);
        self.visit(self.body.local(local).ty());
    }

    fn pattern(&mut self, pattern: &HMIRPattern) {
        match pattern {
            HMIRPattern::Binding(local) => self.bind(*local),
            HMIRPattern::Value(value) => self.visit(*value),
            HMIRPattern::Variant { sum, inner, .. } => {
                if let Some(sum) = sum {
                    self.visit(*sum);
                }
                if let Some(inner) = inner {
                    self.pattern(inner);
                }
            }
            _ => {}
        }
    }

    fn visit(&mut self, expr: HMIRExprID) {
        if !self.seen.insert(expr) {
            return;
        }
        let mut children = Vec::new();
        match self.body.expr(expr).kind() {
            HMIRExprKind::Local(local) => {
                self.references.insert(*local);
            }
            HMIRExprKind::Type(ty) => {
                let _: Result<_, ()> = ty.map(
                    &mut children,
                    |children, id| {
                        children.push(*id);
                        Ok(*id)
                    },
                    |children, id| {
                        children.push(*id);
                        Ok(*id)
                    },
                    |children, declared| {
                        if let HMIRDeclaredMove::CopyTraits(id) = declared {
                            children.push(*id);
                        }
                        Ok(*declared)
                    },
                );
            }
            HMIRExprKind::ScopeAccess { base, .. }
            | HMIRExprKind::Comptime(base)
            | HMIRExprKind::Piped(base) => children.push(*base),
            HMIRExprKind::Quote { params, body } => {
                for local in params {
                    self.bind(*local);
                }
                children.push(*body);
            }
            HMIRExprKind::Splice { quote, args } => {
                children.push(*quote);
                children.extend(args);
            }
            HMIRExprKind::Let { local, initializer } => {
                self.bind(*local);
                children.extend(initializer);
            }
            HMIRExprKind::Call { callee, args } => {
                children.push(*callee);
                children.extend(args);
            }
            HMIRExprKind::Block { statements, .. } => children.extend(statements),
            HMIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                children.extend([*condition, *then_branch]);
                children.extend(else_branch);
            }
            HMIRExprKind::While {
                condition, body, ..
            } => children.extend([*condition, *body]),
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => children.extend([*init, *condition, *increment, *body]),
            HMIRExprKind::Switch { condition, body } => children.extend([*condition, *body]),
            HMIRExprKind::Case { value, body } => {
                children.extend(value);
                children.push(*body);
            }
            HMIRExprKind::Label { body, .. } => children.push(*body),
            HMIRExprKind::Match {
                scrutinee,
                subject,
                arms,
            } => {
                children.push(*scrutinee);
                self.bind(*subject);
                for (pattern, arm) in arms {
                    self.pattern(pattern);
                    children.push(*arm);
                }
            }
            HMIRExprKind::Intrinsic(intrinsic) => {
                let ids = RefCell::new(Vec::new());
                intrinsic.map(
                    |id| {
                        ids.borrow_mut().push(*id);
                        *id
                    },
                    |id| {
                        ids.borrow_mut().push(*id);
                        *id
                    },
                );
                children.extend(ids.into_inner());
            }
            HMIRExprKind::Op(op) => match op {
                HMIROp::BinOp { lhs, rhs, .. } => children.extend([*lhs, *rhs]),
                HMIROp::UnOp { operand, .. }
                | HMIROp::AddressOf(operand)
                | HMIROp::Dereference(operand) => children.push(*operand),
                HMIROp::Coerce { value, target, .. } | HMIROp::Assign { value, target, .. } => {
                    children.extend([*value, *target])
                }
                HMIROp::Control(control) => match control {
                    HMIRControlOp::Return(value) | HMIRControlOp::Yield(value) => {
                        children.extend(value)
                    }
                    HMIRControlOp::Unsafe(value)
                    | HMIRControlOp::Defer(value)
                    | HMIRControlOp::IndirectGoto(value) => children.push(*value),
                    _ => {}
                },
                HMIROp::OwnershipOp(
                    HMIROwnershipOp::Adopt(value)
                    | HMIROwnershipOp::Leak(value)
                    | HMIROwnershipOp::Move(value),
                ) => children.push(*value),
                HMIROp::AggregateOp(op) => match op {
                    HMIRAggregateOp::Member { base, .. } => children.push(*base),
                    HMIRAggregateOp::Index { base, index } => children.extend([*base, *index]),
                    HMIRAggregateOp::Initialize { ty, fields } => {
                        children.push(*ty);
                        children.extend(fields.iter().map(|(_, value)| value));
                    }
                    HMIRAggregateOp::Is { value, pattern } => {
                        children.push(*value);
                        self.pattern(pattern);
                    }
                    HMIRAggregateOp::Unpack { value, bindings } => {
                        children.push(*value);
                        for (_, local) in bindings {
                            self.bind(*local);
                        }
                    }
                },
                HMIROp::Type(op) => match op {
                    HMIRTypeOp::Equal(lhs, rhs) => children.extend([*lhs, *rhs]),
                    HMIRTypeOp::Aggregate { ty, pairs } => {
                        children.push(*ty);
                        children.extend(pairs.iter().map(|(_, value)| value));
                    }
                    HMIRTypeOp::OffsetOf { ty, member } => {
                        children.push(*ty);
                        for step in member {
                            if let HMIRMemberStep::Index(index) = step {
                                children.push(*index);
                            }
                        }
                    }
                    HMIRTypeOp::PointerInner(value)
                    | HMIRTypeOp::ReferenceInner(value)
                    | HMIRTypeOp::Member { ty: value, .. }
                    | HMIRTypeOp::TypeOf(value)
                    | HMIRTypeOp::Decay(value)
                    | HMIRTypeOp::SizeOf(value)
                    | HMIRTypeOp::AlignOf(value)
                    | HMIRTypeOp::IsInt(value)
                    | HMIRTypeOp::IsFloat(value)
                    | HMIRTypeOp::IsPointer(value)
                    | HMIRTypeOp::IsSigned(value) => children.push(*value),
                },
            },
            HMIRExprKind::Literal(_)
            | HMIRExprKind::Def(_)
            | HMIRExprKind::Name { .. }
            | HMIRExprKind::Hole(_)
            | HMIRExprKind::Error(_) => {}
        }
        for child in children {
            self.visit(child);
        }
    }
}
