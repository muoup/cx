use cx_hir::ast::{
    function::HIRFunctionPrototype,
    types::{HIRField, HIRMoveSemantics, HIRType, HIRTypeKind},
};
use cx_hmir::{
    HMIRAggregateKind, HMIRDefRef, HMIRExprID, HMIRExprKind, HMIRFieldDef, HMIRMoveSemantics,
    HMIRTypeOp,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::lower_expr,
    resolve::GlobalSymbol,
};

pub(crate) fn lower_type(cx: &mut BodyLowering<'_>, ty: &HIRType) -> HMIRExprID {
    let span = &ty.range;
    if let Some((params, result)) = staged_signature(ty) {
        return lower_staged_type(cx, params, result, span);
    }

    match &ty.kind {
        HIRTypeKind::Identifier { name, args, .. } => {
            let callee = match cx.lookup(name, ty.tag_kind()) {
                Symbol::Local(binding) => cx.push(HMIRExprKind::Local(binding.local()), span),
                Symbol::Global(GlobalSymbol::Primitive(desc)) => cx.type_constant(desc, span),
                Symbol::Global(
                    GlobalSymbol::Def(def)
                    | GlobalSymbol::Function(def)
                    | GlobalSymbol::ComptimeFunction(def, ..),
                ) => cx.push(HMIRExprKind::Def(def), span),
                Symbol::Global(GlobalSymbol::Constructor(..)) => cx.error(span),
            };
            let Some(args) = args else {
                return callee;
            };
            let args = args.iter().map(|arg| lower_expr(cx, arg)).collect();
            cx.push(HMIRExprKind::Call { callee, args }, span)
        }
        HIRTypeKind::Universe => cx.type_of_types(span),
        HIRTypeKind::Auto => cx.hole(span),
        // Handled as a staged signature above
        HIRTypeKind::Expr(result) => lower_staged_type(cx, Vec::new(), result, span),
        HIRTypeKind::ExplicitSizedArray(element, length) => {
            let element = lower_type(cx, element);
            let length = lower_expr(cx, length);
            cx.type_op(
                HMIRTypeOp::Array {
                    element,
                    length: Some(length),
                },
                span,
            )
        }
        HIRTypeKind::ImplicitSizedArray(element) => {
            let element = lower_type(cx, element);
            cx.type_op(
                HMIRTypeOp::Array {
                    element,
                    length: None,
                },
                span,
            )
        }
        HIRTypeKind::MemoryReference { inner_type, .. } => {
            let inner = lower_type(cx, inner_type);
            cx.type_op(HMIRTypeOp::Reference(inner), span)
        }
        HIRTypeKind::PointerTo { inner_type } => {
            let inner = lower_type(cx, inner_type);
            cx.type_op(HMIRTypeOp::Pointer(inner), span)
        }
        HIRTypeKind::Structured {
            attributes, fields, ..
        } => lower_aggregate_type(
            cx,
            HMIRAggregateKind::Struct,
            &attributes.semantics,
            fields,
            span,
        ),
        HIRTypeKind::Union { fields, .. } => lower_aggregate_type(
            cx,
            HMIRAggregateKind::Union,
            &HIRMoveSemantics::POD,
            fields,
            span,
        ),
        HIRTypeKind::TaggedUnion {
            attributes,
            variants,
            ..
        } => lower_aggregate_type(
            cx,
            HMIRAggregateKind::TaggedUnion,
            &attributes.semantics,
            variants,
            span,
        ),
        HIRTypeKind::FunctionPointer { prototype } => lower_function_type(cx, prototype),
    }
}

// The shape of a staged value: 'expr(T)', or a comptime function over code such as
// '@fn(expr(A)) -> expr(T)'. Code parameters are listed by the type of code they carry.
pub(crate) fn staged_signature(ty: &HIRType) -> Option<(Vec<&HIRType>, &HIRType)> {
    if let HIRTypeKind::Expr(result) = &ty.kind {
        return Some((Vec::new(), result));
    }

    let HIRTypeKind::PointerTo { inner_type } = &ty.kind else {
        return None;
    };
    let HIRTypeKind::FunctionPointer { prototype } = &inner_type.kind else {
        return None;
    };
    let result = match &prototype.return_type.kind {
        HIRTypeKind::Expr(result) => result.as_ref(),
        _ => return None,
    };
    let params = prototype
        .params
        .iter()
        .map(|param| match &param.ty.kind {
            HIRTypeKind::Expr(carried) => carried.as_ref(),
            _ => &param.ty,
        })
        .collect();
    Some((params, result))
}

fn lower_staged_type(
    cx: &mut BodyLowering<'_>,
    params: Vec<&HIRType>,
    result: &HIRType,
    span: &TokenRange,
) -> HMIRExprID {
    let params = params.into_iter().map(|param| lower_type(cx, param)).collect();
    let result = lower_type(cx, result);
    cx.type_op(HMIRTypeOp::Expr { params, result }, span)
}

// The sum a constructor builds, as the def of its type
pub(crate) fn lower_constructor_sum(
    cx: &mut BodyLowering<'_>,
    union_type: &HIRType,
    span: &TokenRange,
) -> HMIRExprID {
    let HIRTypeKind::Identifier { name, .. } = &union_type.kind else {
        return lower_type(cx, union_type);
    };
    let def = match cx.lookup(name, union_type.tag_kind()) {
        Symbol::Global(GlobalSymbol::Def(def)) => def,
        _ => HMIRDefRef::External(name.clone()),
    };
    cx.push(HMIRExprKind::Def(def), span)
}

fn lower_function_type(cx: &mut BodyLowering<'_>, prototype: &HIRFunctionPrototype) -> HMIRExprID {
    let params = prototype
        .params
        .iter()
        .map(|param| lower_type(cx, &param.ty))
        .collect();
    let ret = lower_type(cx, &prototype.return_type);
    cx.type_op(
        HMIRTypeOp::Function {
            params,
            ret,
            variadic: prototype.var_args,
        },
        &prototype.range,
    )
}

fn lower_aggregate_type(
    cx: &mut BodyLowering<'_>,
    kind: HMIRAggregateKind,
    semantics: &HIRMoveSemantics,
    fields: &[HIRField],
    span: &TokenRange,
) -> HMIRExprID {
    let fields = fields
        .iter()
        .map(|field| match field {
            HIRField::Standard { name, ty } => {
                HMIRFieldDef::new(Some(CXIdent::from(name.as_str())), lower_type(cx, ty), None)
            }
            HIRField::Bitfield {
                name,
                integer_type,
                width,
            } => HMIRFieldDef::new(
                name.as_deref().map(CXIdent::from),
                lower_type(cx, integer_type),
                Some(*width),
            ),
        })
        .collect();
    let semantics = match semantics {
        HIRMoveSemantics::POD => HMIRMoveSemantics::POD,
        HIRMoveSemantics::Nocopy => HMIRMoveSemantics::Nocopy,
        HIRMoveSemantics::Nodrop => HMIRMoveSemantics::Nodrop,
    };
    cx.type_op(
        HMIRTypeOp::Aggregate {
            kind,
            semantics,
            fields,
        },
        span,
    )
}
