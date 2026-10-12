use cx_hir::ast::{
    function::{HIRFunctionBody, HIRFunctionContract, HIRFunctionPrototype, HIRParameter},
    global_var::HIREnumVariant,
    modifiers::HIRSymbolNameScheme,
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRComptimeGlobal, HMIRContract, HMIRDefKind, HMIRFnSignature,
    HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIROp, HMIROwnershipOp,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRType, HMIRTypeKind,
        aggregate::HMIRTypeSemantics,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
    unit::function::HMIRFnPrototype,
};
use cx_log::CXResult;
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

pub(crate) fn lower_def(mut context: BodyLowering<'_>, plan: &PlannedDef) -> CXResult<HMIRDefKind> {
    let span = plan.span();
    match plan.source() {
        DefSource::OpaqueType => {
            let kind = HMIRTypeKind::Opaque {
                size: 0,
                alignment: 1,
            };
            let ty = context.declare_type(HMIRType::new(
                kind,
                HMIRTypeSemantics::default(),
                span.clone(),
                Some(plan.name().to_string()),
            ));

            Ok(HMIRDefKind::Type(ty))
        }
        DefSource::Type { ty } => {
            let ty_of = context.type_of_types(span);
            let initializer = lower_type(&mut context, ty)?;
            let global = HMIRComptimeGlobal::new(context.finish(), ty_of, initializer);

            Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
        }
        DefSource::Function { prototype, body } => {
            lower_function(context, plan.name(), prototype, *body)
        }
        DefSource::Global {
            ty,
            initializer,
            linkage,
            naming,
        } => {
            let link_name = context.resolver().link_name(plan.name(), *naming);
            let ty = lower_type(&mut context, ty)?;
            let initializer = initializer
                .map(|initializer| lower_initial_value(&mut context, ty, initializer))
                .transpose()?;
            let global = HMIRGlobal::new(context.finish(), ty, initializer, *linkage, link_name);

            Ok(HMIRDefKind::Global(Box::new(global)))
        }
        DefSource::EnumVariant { variants, index } => {
            lower_enum_variant(context, plan.name(), variants, *index)
        }
        DefSource::Constructor {
            union_type,
            payload,
        } => lower_constructor(context, plan.name(), union_type, payload, span),
    }
}

fn lower_function(
    mut context: BodyLowering<'_>,
    name: &QualifiedName,
    prototype: &HIRFunctionPrototype,
    body: Option<&HIRFunctionBody>,
) -> CXResult<HMIRDefKind> {
    let (params, return_type, contract, root) =
        context.with_stage(prototype.comptime, |this| -> CXResult<_> {
            let mut params = Vec::new();
            for param in declared_params(this, prototype)? {
                let ty = lower_type(this, &param.ty)?;
                params.push(this.declare(
                    param.name.as_ref(),
                    ty,
                    param.comptime,
                    staged_signature(&param.ty).is_some(),
                    &param.ty.range,
                ));
            }
            let return_type = lower_type(this, &prototype.return_type)?;
            let contract = lower_contract(this, &prototype.contract, &prototype.range)?;
            let root = body
                .map(|body| lower_function_body(this, body))
                .transpose()?;

            Ok((params, return_type, contract, root))
        })?;

    let stage = if prototype.comptime {
        HMIRFunctionStage::Comptime
    } else {
        HMIRFunctionStage::Runtime
    };
    let signature = HMIRFnSignature::new(
        stage,
        context.fn_params(&params),
        return_type,
        prototype.var_args,
        contract,
    );
    let symbol_name = context.resolver().link_name(name, prototype.symbol_naming);
    let prototype = HMIRFnPrototype::new(symbol_name, prototype.linkage, signature);
    let def = root.map(|root| context.finish_definition(params, root));

    Ok(HMIRDefKind::Function(Box::new(HMIRFunction::new(
        context.finish(),
        prototype,
        def,
    ))))
}

// A parameter list of a lone unnamed 'void' declares no parameters
pub(crate) fn declared_params<'h>(
    context: &BodyLowering<'_>,
    prototype: &'h HIRFunctionPrototype,
) -> CXResult<&'h [HIRParameter]> {
    Ok(match prototype.params.as_slice() {
        [param] if param.name.is_none() && is_void(context, &param.ty)? => &[],
        params => params,
    })
}

pub(crate) fn is_void(context: &BodyLowering<'_>, ty: &HIRType) -> CXResult<bool> {
    let HIRTypeKind::Identifier {
        name,
        lookup: HIRTypeLookup::Standard,
        args: None,
    } = &ty.kind
    else {
        return Ok(false);
    };

    Ok(matches!(
        context.lookup(name, None, &ty.range)?,
        Symbol::Global(GlobalSymbol::Primitive(HMIRTypeKind::Void))
    ))
}

// A sum variant used as a value is a function building the sum from the variant's payload
fn lower_constructor(
    mut context: BodyLowering<'_>,
    name: &QualifiedName,
    union_type: &HIRType,
    payload: &HIRType,
    span: &TokenRange,
) -> CXResult<HMIRDefKind> {
    let payload = lower_type(&mut context, payload)?;
    let value = context.declare(None, payload, false, false, span);
    let params = vec![value];
    let return_type = lower_type(&mut context, union_type)?;

    let ty = lower_type(&mut context, union_type)?;
    let moved = context.push(HMIRExprKind::Local(value), span);
    let moved = context.ownership(HMIROwnershipOp::Move(moved), span);
    let sum = context.aggregate_op(
        HMIRAggregateOp::Initialize {
            ty,
            fields: vec![(Some(name.name.clone()), moved)],
        },
        span,
    );
    let root = context.returning_block(sum, span);

    let signature = HMIRFnSignature::new(
        HMIRFunctionStage::Runtime,
        context.fn_params(&params),
        return_type,
        false,
        HMIRContract::default(),
    );
    let symbol_name = context
        .resolver()
        .link_name(name, HIRSymbolNameScheme::Namespaced);
    let prototype = HMIRFnPrototype::new(symbol_name, LinkageMode::Static, signature);
    let def = context.finish_definition(params, root);

    Ok(HMIRDefKind::Function(Box::new(HMIRFunction::new(
        context.finish(),
        prototype,
        Some(def),
    ))))
}

fn lower_enum_variant(
    mut context: BodyLowering<'_>,
    name: &QualifiedName,
    variants: &[HIREnumVariant],
    index: usize,
) -> CXResult<HMIRDefKind> {
    let span = TokenRange::internal();
    let int = HMIRIntType::new(HMIRIntWidth::I32, true);
    let ty = context.type_constant(HMIRTypeKind::Int(int), &span);
    // Implicit values count from the nearest explicit one rather than the previous variant, so
    // evaluating a long enum does not recurse through every variant before it
    let explicit = variants[..index]
        .iter()
        .rposition(|variant| variant.value.is_some());
    let initializer = match (&variants[index].value, explicit) {
        (Some(value), _) => lower_expr(&mut context, value)?,
        (None, None) => context.int_constant(int, index as i128, &span),
        (None, Some(explicit)) => {
            let base = QualifiedName::new(name.namespace.clone(), variants[explicit].name.clone());
            let lhs = context.def_expr(base, &span);
            let rhs = context.int_constant(int, (index - explicit) as i128, &span);
            let op = HMIRBinaryOp::Add;
            context.op(HMIROp::BinOp { op, lhs, rhs }, &span)
        }
    };
    let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);

    Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
}

fn lower_contract(
    context: &mut BodyLowering<'_>,
    contract: &HIRFunctionContract,
    span: &TokenRange,
) -> CXResult<HMIRContract> {
    let precondition = contract
        .precondition
        .as_ref()
        .map(|condition| lower_expr(context, condition))
        .transpose()?;
    let postcondition = contract
        .postcondition
        .as_ref()
        .map(|(binding, condition)| {
            context.scoped(|this| -> CXResult<_> {
                let binding = binding.as_ref().map(|name| {
                    let ty = this.hole(span);
                    this.declare_local(Some(name), ty, span)
                });
                Ok((binding, lower_expr(this, condition)?))
            })
        })
        .transpose()?;

    Ok(HMIRContract::new(
        contract.safe,
        precondition,
        postcondition,
    ))
}

fn lower_function_body(
    context: &mut BodyLowering<'_>,
    body: &HIRFunctionBody,
) -> CXResult<HMIRExprID> {
    match body {
        HIRFunctionBody::Block { statements, range } => {
            let statements = lower_scope_statements(context, statements)?;
            Ok(context.block(statements, true, false, range))
        }
        HIRFunctionBody::Expression(expr) => lower_expr(context, expr),
    }
}
