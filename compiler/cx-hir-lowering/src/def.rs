use cx_hir::ast::{
    function::{HIRFunctionBody, HIRFunctionContract, HIRFunctionPrototype},
    global_var::HIREnumVariant,
    modifiers::HIRSymbolNameScheme,
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBlockKind, HMIRComptimeGlobal, HMIRContract, HMIRDefKind,
    HMIRExprID, HMIRExprKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRIntWidth,
    HMIRNativeOp, HMIROwnershipOp, HMIRSignature, HMIRTypeDesc,
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_initial_value, lower_scope_statements},
    plan::{DefSource, PlannedDef},
    resolve::GlobalSymbol,
    ty::{lower_type, staged_signature},
};

pub(crate) fn lower_def(mut cx: BodyLowering<'_>, plan: &PlannedDef) -> HMIRDefKind {
    let span = plan.span();
    match plan.source() {
        DefSource::OpaqueType => HMIRDefKind::Type(cx.intern(HMIRTypeDesc::Opaque {
            size: 0,
            alignment: 1,
        })),
        DefSource::Type { ty } => {
            let ty_of = cx.type_of_types(span);
            let initializer = lower_type(&mut cx, ty);
            HMIRDefKind::ComptimeGlobal(Box::new(HMIRComptimeGlobal::new(
                cx.finish(),
                ty_of,
                initializer,
            )))
        }
        DefSource::Function { prototype, body } => {
            lower_function(cx, plan.name(), prototype, *body)
        }
        DefSource::Global {
            ty,
            mutable,
            initializer,
            linkage,
            naming,
        } => {
            let link_name = cx.resolver().link_name(plan.name(), *naming);
            let global_ty = lower_type(&mut cx, ty);
            let initializer =
                initializer.map(|initializer| lower_initial_value(&mut cx, global_ty, initializer));
            HMIRDefKind::Global(Box::new(HMIRGlobal::new(
                cx.finish(),
                global_ty,
                initializer,
                *mutable,
                *linkage,
                link_name,
            )))
        }
        DefSource::EnumVariant { variants, index } => {
            lower_enum_variant(cx, plan.name(), variants, *index)
        }
        DefSource::Constructor {
            union_type,
            payload,
        } => lower_constructor(cx, plan.name(), union_type, payload, span),
    }
}

fn lower_function(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    prototype: &HIRFunctionPrototype,
    body: Option<&HIRFunctionBody>,
) -> HMIRDefKind {
    let (params, return_type, contract, root) = cx.with_stage(prototype.comptime, |this| {
        let declared = match prototype.params.as_slice() {
            [param] if param.name.is_none() && is_void(this, &param.ty) => &[],
            params => params,
        };
        let params = declared
            .iter()
            .map(|param| {
                let ty = lower_type(this, &param.ty);
                this.declare(
                    param.name.as_ref(),
                    ty,
                    param.comptime,
                    staged_signature(&param.ty).is_some(),
                    &param.ty.range,
                )
            })
            .collect();
        let return_type = lower_type(this, &prototype.return_type);
        let contract = lower_contract(this, &prototype.contract, &prototype.range);
        let root = body.map(|body| lower_function_body(this, body));
        (params, return_type, contract, root)
    });
    let signature = HMIRSignature::new(
        params,
        return_type,
        prototype.var_args,
        prototype.linkage,
        cx.resolver().link_name(name, prototype.symbol_naming),
        contract,
    );
    let stage = if prototype.comptime {
        HMIRFunctionStage::Comptime
    } else {
        HMIRFunctionStage::Runtime
    };
    let labels = cx.take_address_labels();
    let function = HMIRFunction::new(stage, cx.finish(), signature, root);
    HMIRDefKind::Function(Box::new(function.with_address_labels(labels)))
}

pub(crate) fn is_void(cx: &BodyLowering<'_>, ty: &HIRType) -> bool {
    let HIRTypeKind::Identifier {
        name,
        lookup: HIRTypeLookup::Standard,
        args: None,
    } = &ty.kind
    else {
        return false;
    };
    matches!(
        cx.lookup(name, None),
        Symbol::Global(GlobalSymbol::Primitive(HMIRTypeDesc::Void))
    )
}

// A sum variant used as a value is a function building the sum from the variant's payload
fn lower_constructor(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    union_type: &HIRType,
    payload: &HIRType,
    span: &TokenRange,
) -> HMIRDefKind {
    let payload = lower_type(&mut cx, payload);
    let value = cx.declare(None, payload, false, false, span);
    let params = vec![value];
    let return_type = lower_type(&mut cx, union_type);

    let ty = lower_type(&mut cx, union_type);
    let value = cx.push(HMIRExprKind::Local(value), span);
    let value = cx.ownership(HMIROwnershipOp::Move(value), span);
    let sum = cx.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty,
            fields: vec![(Some(name.name.clone()), value)],
        },
        span,
    );
    let root = cx.returning_block(sum, span);
    let signature = HMIRSignature::new(
        params,
        return_type,
        false,
        LinkageMode::Static,
        cx.resolver()
            .link_name(name, HIRSymbolNameScheme::Namespaced),
        HMIRContract::default(),
    );
    HMIRDefKind::Function(Box::new(HMIRFunction::new(
        HMIRFunctionStage::Runtime,
        cx.finish(),
        signature,
        Some(root),
    )))
}

fn lower_enum_variant(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    variants: &[HIREnumVariant],
    index: usize,
) -> HMIRDefKind {
    let span = TokenRange::internal();
    let int = HMIRTypeDesc::Int {
        width: HMIRIntWidth::I32,
        signed: true,
    };
    let ty = cx.type_constant(int.clone(), &span);
    // Implicit values count from the nearest explicit one rather than the previous variant, so
    // evaluating a long enum does not recurse through every variant before it
    let explicit = variants[..index]
        .iter()
        .rposition(|variant| variant.value.is_some());
    let initializer = match (&variants[index].value, explicit) {
        (Some(value), _) => lower_expr(&mut cx, value),
        (None, None) => cx.int_constant(int, index as i128, &span),
        (None, Some(explicit)) => {
            let base = QualifiedName::new(name.namespace.clone(), variants[explicit].name.clone());
            let lhs = cx.def_expr(base, &span);
            let rhs = cx.int_constant(int, (index - explicit) as i128, &span);
            cx.native(
                HMIRNativeOp::BinOp {
                    op: HMIRBinaryOp::Add,
                    lhs,
                    rhs,
                },
                &span,
            )
        }
    };
    HMIRDefKind::ComptimeGlobal(Box::new(HMIRComptimeGlobal::new(
        cx.finish(),
        ty,
        initializer,
    )))
}

fn lower_contract(
    cx: &mut BodyLowering<'_>,
    contract: &HIRFunctionContract,
    span: &TokenRange,
) -> HMIRContract {
    let precondition = contract
        .precondition
        .as_ref()
        .map(|condition| lower_expr(cx, condition));
    let postcondition = contract.postcondition.as_ref().map(|(binding, condition)| {
        cx.scoped(|this| {
            let binding = binding.as_ref().map(|name| {
                let ty = this.hole(span);
                this.declare_local(Some(name), ty, span)
            });
            (binding, lower_expr(this, condition))
        })
    });
    HMIRContract::new(contract.safe, precondition, postcondition)
}

fn lower_function_body(cx: &mut BodyLowering<'_>, body: &HIRFunctionBody) -> HMIRExprID {
    match body {
        HIRFunctionBody::Block { statements, range } => {
            let statements = lower_scope_statements(cx, statements);
            cx.block(HMIRBlockKind::Scope, statements, range)
        }
        HIRFunctionBody::Expression(expr) => lower_expr(cx, expr),
    }
}
