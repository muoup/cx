use cx_hir::ast::{
    function::{
        HIRComptimeFnPrototype, HIRFunctionBody, HIRFunctionContract, HIRFunctionPrototype,
    },
    global_var::HIREnumVariant,
    modifiers::HIRSymbolNameScheme,
    template::HIRTemplatePrototype,
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBlockKind, HMIRComptimeGlobal, HMIRContract, HMIRDefKind,
    HMIRExprID, HMIRExprKind, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIRIntWidth,
    HMIRLocalID, HMIRNativeOp, HMIROwnershipOp, HMIRSignature, HMIRTypeDesc,
};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    body::{BodyLowering, Symbol},
    expr::{lower_expr, lower_initial_value},
    plan::{DefSource, PlannedDef},
    resolve::GlobalSymbol,
    ty::{lower_comptime_value_type, lower_type},
};

pub(crate) fn lower_def(mut cx: BodyLowering<'_>, plan: &PlannedDef) -> HMIRDefKind {
    let span = plan.span();
    match plan.source() {
        DefSource::OpaqueType => HMIRDefKind::Type(cx.intern(HMIRTypeDesc::Opaque {
            size: 0,
            alignment: 1,
        })),
        DefSource::Type { template: None, ty } => {
            let ty_of = cx.type_of_types(span);
            let initializer = lower_type(&mut cx, ty);
            HMIRDefKind::ComptimeGlobal(Box::new(HMIRComptimeGlobal::new(
                cx.finish(),
                ty_of,
                initializer,
            )))
        }
        DefSource::Type {
            template: Some(template),
            ty,
        } => lower_type_generator(cx, plan.name(), template, ty, span),
        DefSource::Function {
            prototype,
            template,
            body,
        } => lower_function(cx, plan.name(), prototype, *template, *body),
        DefSource::ComptimeFunction {
            prototype,
            template,
            body,
        } => lower_comptime_function(cx, plan.name(), prototype, *template, body),
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
                initializer.map(|initializer| lower_initial_value(&mut cx, ty, initializer));
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
            template,
            union_type,
            payload,
        } => lower_constructor(cx, plan.name(), *template, union_type, payload, span),
    }
}

fn lower_type_generator(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    template: &HIRTemplatePrototype,
    ty: &HIRType,
    span: &TokenRange,
) -> HMIRDefKind {
    let (params, return_type, root) = cx.with_stage(true, |this| {
        let params = lower_template_params(this, Some(template), span);
        let return_type = this.type_of_types(span);
        let ty = lower_type(this, ty);
        (params, return_type, this.returning_block(ty, span))
    });
    let signature = HMIRSignature::new(
        params,
        return_type,
        false,
        LinkageMode::Standard,
        cx.resolver()
            .link_name(name, HIRSymbolNameScheme::Namespaced),
        HMIRContract::default(),
    );
    HMIRDefKind::Function(Box::new(HMIRFunction::new(
        HMIRFunctionStage::Comptime,
        cx.finish(),
        signature,
        Some(root),
    )))
}

fn lower_function(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    prototype: &HIRFunctionPrototype,
    template: Option<&HIRTemplatePrototype>,
    body: Option<&HIRFunctionBody>,
) -> HMIRDefKind {
    let mut params = lower_template_params(&mut cx, template, &prototype.range);
    let declared = match prototype.params.as_slice() {
        [param] if param.name.is_none() && is_void(&cx, &param.ty) => &[],
        params => params,
    };
    for param in declared {
        let ty = lower_type(&mut cx, &param.ty);
        params.push(cx.declare(param.name.as_ref(), ty, false, false, &param.ty.range));
    }
    let return_type = lower_type(&mut cx, &prototype.return_type);
    let contract = lower_contract(&mut cx, &prototype.contract, &prototype.range);
    let root = body.map(|body| lower_function_body(&mut cx, body));
    let signature = HMIRSignature::new(
        params,
        return_type,
        prototype.var_args,
        prototype.linkage,
        cx.resolver().link_name(name, prototype.symbol_naming),
        contract,
    );
    HMIRDefKind::Function(Box::new(HMIRFunction::new(
        HMIRFunctionStage::Runtime,
        cx.finish(),
        signature,
        root,
    )))
}

pub(crate) fn is_void(cx: &BodyLowering<'_>, ty: &HIRType) -> bool {
    let HIRTypeKind::Identifier {
        name,
        lookup: HIRTypeLookup::Standard,
        template_input: None,
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
    template: Option<&HIRTemplatePrototype>,
    union_type: &HIRType,
    payload: &HIRType,
    span: &TokenRange,
) -> HMIRDefKind {
    let mut params = lower_template_params(&mut cx, template, span);
    let payload = lower_type(&mut cx, payload);
    let value = cx.declare(None, payload, false, false, span);
    params.push(value);
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

fn lower_comptime_function(
    mut cx: BodyLowering<'_>,
    name: &QualifiedName,
    prototype: &HIRComptimeFnPrototype,
    template: Option<&HIRTemplatePrototype>,
    body: &HIRFunctionBody,
) -> HMIRDefKind {
    let (params, return_type, root) = cx.with_stage(true, |this| {
        let mut params = lower_template_params(this, template, &prototype.range);
        for param in &prototype.params {
            let ty = lower_comptime_value_type(this, &param.value_type);
            params.push(this.declare(
                param.name.as_ref(),
                ty,
                true,
                param.value_type.expr,
                &param.value_type.ty.range,
            ));
        }
        let return_type = lower_comptime_value_type(this, &prototype.return_type);
        (params, return_type, lower_function_body(this, body))
    });
    let signature = HMIRSignature::new(
        params,
        return_type,
        false,
        LinkageMode::Standard,
        cx.resolver()
            .link_name(name, HIRSymbolNameScheme::Namespaced),
        HMIRContract::default(),
    );
    HMIRDefKind::Function(Box::new(HMIRFunction::new(
        HMIRFunctionStage::Comptime,
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
    let initializer = match (&variants[index].value, index) {
        (Some(value), _) => lower_expr(&mut cx, value),
        (None, 0) => cx.int_constant(int, 0, &span),
        (None, _) => {
            let previous =
                QualifiedName::new(name.namespace.clone(), variants[index - 1].name.clone());
            let lhs = cx.def_expr(previous, &span);
            let rhs = cx.int_constant(int, 1, &span);
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

fn lower_template_params(
    cx: &mut BodyLowering<'_>,
    template: Option<&HIRTemplatePrototype>,
    span: &TokenRange,
) -> Vec<HMIRLocalID> {
    let Some(template) = template else {
        return Vec::new();
    };
    template
        .types
        .iter()
        .map(|name| {
            let ty = cx.type_of_types(span);
            cx.declare(Some(name), ty, true, false, span)
        })
        .collect()
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
        HIRFunctionBody::Block { statements, range } => cx.scoped(|this| {
            let statements = statements
                .iter()
                .map(|statement| lower_expr(this, statement))
                .collect();
            this.block(HMIRBlockKind::Scope, statements, range)
        }),
        HIRFunctionBody::Expression(expr) => {
            let value = lower_expr(cx, expr);
            cx.returning_block(value, &expr.range)
        }
    }
}
