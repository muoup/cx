use cx_hir::ast::{
    function::{HIRComptimeValueType, HIRFunctionPrototype},
    template::HIRTemplateInput,
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
    match &ty.kind {
        HIRTypeKind::Identifier {
            name,
            template_input,
            ..
        } => {
            let callee = match cx.lookup(name, ty.tag_kind()) {
                Symbol::Local(binding) => cx.push(HMIRExprKind::Local(binding.local()), span),
                Symbol::Global(GlobalSymbol::Primitive(desc)) => cx.type_constant(desc, span),
                Symbol::Global(
                    GlobalSymbol::Def(def)
                    | GlobalSymbol::Function(def, _)
                    | GlobalSymbol::ComptimeFunction(def, ..),
                ) => cx.push(HMIRExprKind::Def(def), span),
                Symbol::Global(GlobalSymbol::Constructor(..)) => cx.error(span),
            };
            lower_instantiate(cx, callee, template_input.as_ref(), span)
        }
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

pub(crate) fn lower_comptime_value_type(
    cx: &mut BodyLowering<'_>,
    value_type: &HIRComptimeValueType,
) -> HMIRExprID {
    let result = lower_type(cx, &value_type.ty);
    if !value_type.expr {
        return result;
    }
    let params = value_type
        .params
        .iter()
        .map(|param| lower_type(cx, param))
        .collect();
    cx.type_op(HMIRTypeOp::Expr { params, result }, &value_type.ty.range)
}

pub(crate) fn lower_instantiate(
    cx: &mut BodyLowering<'_>,
    callee: HMIRExprID,
    template_input: Option<&HIRTemplateInput>,
    span: &TokenRange,
) -> HMIRExprID {
    let Some(input) = template_input else {
        return callee;
    };
    let args = lower_template_args(cx, Some(input));
    cx.push(HMIRExprKind::Call { callee, args }, span)
}

pub(crate) fn lower_template_args(
    cx: &mut BodyLowering<'_>,
    input: Option<&HIRTemplateInput>,
) -> Vec<HMIRExprID> {
    input
        .map(|input| input.params.iter().map(|ty| lower_type(cx, ty)).collect())
        .unwrap_or_default()
}

pub(crate) fn lower_constructor_sum(
    cx: &mut BodyLowering<'_>,
    union_type: &HIRType,
    template_input: Option<&HIRTemplateInput>,
    span: &TokenRange,
) -> HMIRExprID {
    let HIRTypeKind::Identifier { name, .. } = &union_type.kind else {
        return lower_type(cx, union_type);
    };
    let def = match cx.lookup(name, union_type.tag_kind()) {
        Symbol::Global(GlobalSymbol::Def(def)) => def,
        _ => HMIRDefRef::External(name.clone()),
    };
    let callee = cx.push(HMIRExprKind::Def(def), span);
    lower_instantiate(cx, callee, template_input, span)
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
