use cx_hir::ast::{
    function::{HIRFunctionBody, HIRFunctionPrototype, HIRParameter},
    global_var::HIREnumVariant,
    types::{HIRType, HIRTypeKind, HIRTypeLookup},
};
use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRComptimeGlobal, HMIRContract, HMIRDef, HMIRDefID, HMIRDefKind,
    HMIRFnSignature, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIROp,
    expr::HMIRExprID,
    ty::{
        HMIRExprTypeKind,
        aggregate::HMIRTypeSemantics,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
    unit::function::HMIRFnPrototype,
};
use cx_log::CXResult;
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;

use crate::{
    body::BodyLowering,
    expr::{lower_expr, lower_initial_value, lower_scope_statements},
    plan::{DefSource, PlannedSymbol},
    ty::{lower_type, staged_signature},
    unit::{UnitContext, def_name},
};

pub(crate) fn lower_symbol(
    unit: &UnitContext,
    id: HMIRDefID,
    symbol: &PlannedSymbol,
) -> CXResult<HMIRDef> {
    let tag = symbol
        .declarations()
        .first()
        .and_then(|declaration| declaration.tag());
    let name = def_name(symbol.name().clone(), tag);
    let mut declarations = Vec::new();
    for declaration in symbol.declarations() {
        let namespace = declaration.namespace().clone();
        let point = declaration.ordered().then_some(declaration.point());
        let mut context = BodyLowering::new(unit, namespace, false)
            .owned_by(id)
            .at(point);
        let kind = match declaration.source() {
            Some(source) => Some(lower_def(context, &name, source)?),
            None => {
                let ty = context.type_of_types(declaration.span());
                let initializer = context.expr_type(
                    HMIRExprTypeKind::Opaque {
                        size: 0,
                        alignment: 1,
                    },
                    HMIRTypeSemantics::default(),
                    Some(&name.name),
                    declaration.span(),
                );
                let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);
                Some(HMIRDefKind::ComptimeGlobal(Box::new(global)))
            }
        };
        declarations.push(declaration.lowered(kind));
    }

    Ok(HMIRDef::new(name, declarations))
}

fn lower_def(
    mut context: BodyLowering<'_>,
    name: &QualifiedName,
    source: &DefSource,
) -> CXResult<HMIRDefKind> {
    match source {
        DefSource::Type { ty } => {
            let ty_of = context.type_of_types(&ty.range);
            let initializer = lower_type(&mut context, ty)?;
            let global = HMIRComptimeGlobal::new(context.finish(), ty_of, initializer);

            Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
        }
        DefSource::Function { prototype, body } => lower_function(context, name, prototype, *body),
        DefSource::Global {
            comptime,
            ty,
            initializer,
            linkage,
            naming,
        } => {
            let link_name = context.unit().link_name(name, *naming);
            let ty = lower_type(&mut context, ty)?;
            let initializer = initializer
                .map(|initializer| lower_initial_value(&mut context, ty, initializer))
                .transpose()?;
            if *comptime {
                let Some(initializer) = initializer else {
                    let subject = "the initial value of a compile-time global".into();
                    return crate::log::log_hir_error(
                        &TokenRange::internal(),
                        &cx_log::catalogue::typecheck::CANNOT_INFER,
                        subject,
                    );
                };
                let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);
                return Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)));
            }
            let global = HMIRGlobal::new(context.finish(), ty, initializer, *linkage, link_name);

            Ok(HMIRDefKind::Global(Box::new(global)))
        }
        DefSource::EnumVariant { variants, index } => {
            lower_enum_variant(context, name, variants, *index)
        }
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
            for param in declared_params(prototype) {
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
            let contract = lower_contract(this, prototype)?;
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
    let symbol_name = context.unit().link_name(name, prototype.symbol_naming);
    let prototype = HMIRFnPrototype::new(symbol_name, prototype.linkage, signature);
    let def = root.map(|root| context.finish_definition(params, root));

    Ok(HMIRDefKind::Function(Box::new(HMIRFunction::new(
        context.finish(),
        prototype,
        def,
    ))))
}

// A parameter list of a lone unnamed 'void' declares no parameters
pub(crate) fn declared_params<'h>(prototype: &'h HIRFunctionPrototype) -> &'h [HIRParameter] {
    match prototype.params.as_slice() {
        [param] if param.name.is_none() && is_void(&param.ty) => &[],
        params => params,
    }
}

fn is_void(ty: &HIRType) -> bool {
    let HIRTypeKind::Identifier {
        name,
        lookup: HIRTypeLookup::Standard,
        args: None,
    } = &ty.kind
    else {
        return false;
    };

    name.root_name_ref()
        .is_some_and(|name| name.as_str() == "void")
}

fn lower_enum_variant(
    mut context: BodyLowering<'_>,
    name: &QualifiedName,
    variants: &[HIREnumVariant],
    index: usize,
) -> CXResult<HMIRDefKind> {
    let span = TokenRange::internal();
    let int = HMIRIntType::new(HMIRIntWidth::I32, true);
    let ty = context.type_ref(HMIRExprTypeKind::Int(int), &span);
    // Implicit values count from the nearest explicit one rather than the previous variant, so
    // evaluating a long enum does not recurse through every variant before it
    let explicit = variants[..index]
        .iter()
        .rposition(|variant| variant.value.is_some());
    let initializer = match (&variants[index].value, explicit) {
        (Some(value), _) => lower_expr(&mut context, value)?,
        (None, None) => context.int_literal(int, index as i128, &span),
        (None, Some(explicit)) => {
            let base = QualifiedName::new(name.namespace.clone(), variants[explicit].name.clone());
            let lhs = context.def_expr(base, &span);
            let rhs = context.int_literal(int, (index - explicit) as i128, &span);
            let op = HMIRBinaryOp::Add;
            context.op(HMIROp::BinOp { op, lhs, rhs }, &span)
        }
    };
    let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);

    Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
}

// The conditions are lowered into a body of their own, where a local stands for each parameter
// they may name. Those locals take their types from the signature.
fn lower_contract(
    context: &mut BodyLowering<'_>,
    prototype: &HIRFunctionPrototype,
) -> CXResult<HMIRContract> {
    let contract = &prototype.contract;
    let span = &prototype.range;
    if contract.precondition.is_none() && contract.postcondition.is_none() {
        let (body, params) = (HMIRBody::new(), Box::default());
        return Ok(HMIRContract::new(body, params, contract.safe, None, None));
    }

    let declared = declared_params(prototype);
    let mut conditions = context.detached(prototype.comptime);
    let params = declared
        .iter()
        .map(|param| {
            let span = &param.ty.range;
            let ty = conditions.hole(span);
            let quoted = staged_signature(&param.ty).is_some();
            conditions.declare(param.name.as_ref(), ty, param.comptime, quoted, span)
        })
        .collect();

    let precondition = contract
        .precondition
        .as_ref()
        .map(|condition| lower_expr(&mut conditions, condition))
        .transpose()?;
    let postcondition = contract
        .postcondition
        .as_ref()
        .map(|(binding, condition)| {
            conditions.scoped(|this| -> CXResult<_> {
                let binding = binding.as_ref().map(|name| {
                    let ty = this.hole(span);
                    this.declare_local(Some(name), ty, span)
                });
                Ok((binding, lower_expr(this, condition)?))
            })
        })
        .transpose()?;

    Ok(HMIRContract::new(
        conditions.finish(),
        params,
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
