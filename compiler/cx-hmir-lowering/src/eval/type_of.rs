mod call;

use std::rc::Rc;

use cx_hmir::{
    HMIRAggregateOp, HMIRBlockKind, HMIRCoerceMode, HMIRControlOp, HMIRDefKind, HMIRExprID,
    HMIRExprKind, HMIRNativeOp, HMIROwnershipOp, HMIRTypeOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::CXResult;

use crate::{
    eval::{EvalFrame, RuntimeView, type_relations},
    program::{DefKey, Program, def_body},
    staging_error,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

pub(crate) fn type_of(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
) -> CXResult<TypeID> {
    let unit = frame.unit().clone();
    let body = def_body(unit.def(frame.def().def())).expect("type query has a body");
    let expr = body.expr(id);
    let span = expr.span();
    let ty = match expr.kind() {
        HMIRExprKind::Constant(constant) => {
            let value = program.import_constant(frame.def().unit(), constant, span)?;
            program.static_type(&value, span)?
        }
        HMIRExprKind::Local(local) => match frame.local(*local).cloned() {
            Some(StaticValue::Quote(quote)) => {
                let quote = quote.get();
                let mut captured = EvalFrame::new(
                    program.unit(quote.unit()),
                    DefKey::new(quote.unit(), quote.def()),
                    Rc::new(quote.owner().clone()),
                )
                .with_runtime(RuntimeView::new(
                    quote.origin(),
                    quote.runtime_types().clone(),
                ));
                for (local, value) in quote.env() {
                    captured.bind(*local, value.clone());
                }
                quote_type(program, &mut captured, quote.params(), quote.body())?
            }
            Some(value) => program.static_type(&value, span)?,
            None => match frame.runtime_type(*local) {
                Some(ty) => ty,
                None => program.eval_type(frame, body.local(*local).ty())?,
            },
        },
        HMIRExprKind::Def(def) => {
            let key = program.resolve(frame.def().unit(), def, span)?;
            let target = program.unit(key.unit());
            match target.def(key.def()).kind() {
                HMIRDefKind::Global(_) => program.global_type(key, span)?,
                HMIRDefKind::Type(_) => program.types_mut().type_of_types(),
                HMIRDefKind::Function(_) => program.function_type(&(key, Vec::new()), span)?,
                HMIRDefKind::ComptimeGlobal(global) => {
                    let mut global_frame = program.frame_for(&(key, Vec::new()));
                    program.eval_type(&mut global_frame, global.ty())?
                }
            }
        }
        HMIRExprKind::Comptime(inner) => type_of(program, frame, *inner)?,
        HMIRExprKind::Call { callee, args } => call::result(program, frame, *callee, args, span)?,
        HMIRExprKind::Quote { params, body } => quote_type(program, frame, params, *body)?,
        HMIRExprKind::Splice { quote, .. } => {
            let ty = type_of(program, frame, *quote)?;
            match program.types().kind(ty) {
                TypeKind::Expr { result, .. } => *result,
                _ => {
                    return Err(staging_error(
                        span,
                        "splice requires an expression type".into(),
                    ));
                }
            }
        }
        HMIRExprKind::Native(op) => match op {
            HMIRNativeOp::Coerce {
                mode: HMIRCoerceMode::Truthy,
                ..
            }
            | HMIRNativeOp::AggregateOp(HMIRAggregateOp::Is { .. }) => program.types_mut().bool(),
            HMIRNativeOp::Coerce { target, .. }
            | HMIRNativeOp::AggregateOp(HMIRAggregateOp::Initialize { ty: target, .. }) => {
                program.eval_type(frame, *target)?
            }
            HMIRNativeOp::Assign { target, .. } => type_of(program, frame, *target)?,
            HMIRNativeOp::AddressOf(value) => {
                let ty = type_of(program, frame, *value)?;
                program.types_mut().pointer(ty)
            }
            HMIRNativeOp::UnOp { op, operand } => {
                let ty = type_of(program, frame, *operand)?;
                type_relations::unary(program, *op, ty, span)?
            }
            HMIRNativeOp::BinOp { op, lhs, rhs } => {
                let lhs = type_of(program, frame, *lhs)?;
                let rhs = type_of(program, frame, *rhs)?;
                type_relations::binary(program, *op, lhs, rhs, span)?
            }
            HMIRNativeOp::AggregateOp(HMIRAggregateOp::Member { base, name }) => {
                let ty = type_of(program, frame, *base)?;
                let ty = program.types().pointee(ty).unwrap_or(ty);
                program
                    .types()
                    .field(ty, name.as_str())
                    .map(|(_, field)| field.ty())
                    .ok_or_else(|| {
                        staging_error(
                            span,
                            format!("'{}' has no member '{name}'", program.types().display(ty)),
                        )
                    })?
            }
            HMIRNativeOp::AggregateOp(HMIRAggregateOp::Index { base, .. }) => {
                let ty = type_of(program, frame, *base)?;
                let ty = type_relations::decay(program.types_mut(), ty);
                program.types().pointee(ty).ok_or_else(|| {
                    staging_error(
                        span,
                        format!("cannot index '{}'", program.types().display(ty)),
                    )
                })?
            }
            HMIRNativeOp::OwnershipOp(
                HMIROwnershipOp::Move(inner) | HMIROwnershipOp::Leak(inner),
            )
            | HMIRNativeOp::Control(HMIRControlOp::Unsafe(inner)) => {
                type_of(program, frame, *inner)?
            }
            HMIRNativeOp::Type(HMIRTypeOp::SizeOf(_) | HMIRTypeOp::AlignOf(_)) => {
                program.types_mut().size_type()
            }
            HMIRNativeOp::Type(
                HMIRTypeOp::IsInt(_)
                | HMIRTypeOp::IsFloat(_)
                | HMIRTypeOp::IsPointer(_)
                | HMIRTypeOp::IsSigned(_)
                | HMIRTypeOp::Equal(..),
            ) => program.types_mut().bool(),
            HMIRNativeOp::Type(_) => program.types_mut().type_of_types(),
            HMIRNativeOp::Control(_) => program.types_mut().intern(TypeKind::Unreachable),
            _ => {
                return Err(staging_error(
                    span,
                    "cannot determine this operation's type".into(),
                ));
            }
        },
        HMIRExprKind::Block {
            kind,
            statements,
            tail,
        } => match tail {
            Some(tail) => type_of(program, frame, *tail)?,
            None if *kind == HMIRBlockKind::Yield => {
                let mut types = Vec::new();
                for statement in statements {
                    yields(program, frame, *statement, &mut types)?;
                }
                let mut types = types.into_iter();
                let mut ty = types.next().unwrap_or_else(|| program.types_mut().void());
                for other in types {
                    ty = type_relations::common(program, ty, other, span)?;
                }
                ty
            }
            None => program.types_mut().void(),
        },
        HMIRExprKind::If {
            then_branch,
            else_branch: Some(other),
            ..
        } => {
            let lhs = type_of(program, frame, *then_branch)?;
            let rhs = type_of(program, frame, *other)?;
            type_relations::common(program, lhs, rhs, span)?
        }
        HMIRExprKind::Let { .. }
        | HMIRExprKind::If {
            else_branch: None, ..
        }
        | HMIRExprKind::While { .. }
        | HMIRExprKind::For { .. }
        | HMIRExprKind::Switch { .. } => program.types_mut().void(),
        HMIRExprKind::Label { body, .. } => type_of(program, frame, *body)?,
        HMIRExprKind::Intrinsic(Intrinsic::VA(VAIntrinsic::Arg { ty, .. })) => {
            program.eval_type(frame, *ty)?
        }
        _ => {
            return Err(staging_error(
                span,
                "cannot determine this expression's type".into(),
            ));
        }
    };
    Ok(type_relations::value_type(program.types(), ty))
}

fn quote_type(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    params: &[cx_hmir::HMIRLocalID],
    body: HMIRExprID,
) -> CXResult<TypeID> {
    let unit = frame.unit().clone();
    let source = def_body(unit.def(frame.def().def())).expect("quote has a body");
    let params = params
        .iter()
        .map(|param| program.eval_type(frame, source.local(*param).ty()))
        .collect::<CXResult<Vec<_>>>()?;
    let result = type_of(program, frame, body)?;
    Ok(program
        .types_mut()
        .intern(TypeKind::Expr { params, result }))
}

fn yields(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    types: &mut Vec<TypeID>,
) -> CXResult<()> {
    let unit = frame.unit().clone();
    let body = def_body(unit.def(frame.def().def())).expect("yield has a body");
    match body.expr(id).kind() {
        HMIRExprKind::Native(HMIRNativeOp::Control(HMIRControlOp::Yield(value))) => {
            types.push(match value {
                Some(value) => type_of(program, frame, *value)?,
                None => program.types_mut().void(),
            });
        }
        HMIRExprKind::Block {
            kind,
            statements,
            tail,
        } if *kind != HMIRBlockKind::Yield => {
            for statement in statements.iter().chain(tail) {
                yields(program, frame, *statement, types)?;
            }
        }
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            yields(program, frame, *then_branch, types)?;
            if let Some(branch) = else_branch {
                yields(program, frame, *branch, types)?;
            }
        }
        HMIRExprKind::While { body, .. }
        | HMIRExprKind::For { body, .. }
        | HMIRExprKind::Label { body, .. }
        | HMIRExprKind::Native(HMIRNativeOp::Control(HMIRControlOp::Unsafe(body))) => {
            yields(program, frame, *body, types)?
        }
        _ => {}
    }
    Ok(())
}
