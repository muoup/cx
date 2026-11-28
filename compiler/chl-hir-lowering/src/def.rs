use chl_hir::ast::{
    expression::{HIRExprKind, HIRExpression},
    function::{HIRFunctionBody, HIRFunctionPrototype, HIRParameter},
    global_var::HIREnumDefinition,
    types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
};
use chl_hmir::{
    HMIRBinaryOp, HMIRBodyContext, HMIRCoerceMode, HMIRComptimeGlobal, HMIRContract, HMIRDef,
    HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunction, HMIRFunctionStage, HMIRGlobal, HMIROp,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind, HMIRUndefined,
        aggregate::HMIRTypeSemantics,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
    unit::function::{HMIRFnParamDecl, HMIRFnPrototype},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_namespace::module::QualifiedName;
use chl_tokens::TokenRange;

use crate::{
    body::BodyLowering,
    expr::{lower_expr, lower_initial_value, lower_scope_statements},
    log::log_hir_error,
    plan::{DefSource, PlannedSymbol},
    ty::{lower_return_type, lower_type},
    unit::{UnitContext, def_name},
};

pub(crate) fn lower_symbol(
    unit: &UnitContext,
    id: HMIRDefID,
    symbol: &PlannedSymbol,
) -> CHLResult<HMIRDef> {
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
                let tag = declaration.tag().expect("a declaration without a source is a forward tag");
                let ty = context.type_of_types(declaration.span());
                let initializer = context.expr_type(
                    HMIRExprTypeKind::Undefined(HMIRUndefined::Forward(Some(tag))),
                    HMIRTypeSemantics::default(),
                    Some(&symbol.name().name),
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

fn lower_def<'a>(
    mut context: BodyLowering<'a>,
    name: &QualifiedName,
    source: &DefSource<'a>,
) -> CHLResult<HMIRDefKind> {
    match source {
        DefSource::Type { ty } | DefSource::EnumType { underlying: Some(ty) } => {
            let ty_of = context.type_of_types(&ty.range);
            let initializer = lower_type(&mut context, ty)?;
            let global = HMIRComptimeGlobal::new(context.finish(), ty_of, initializer);

            Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
        }
        DefSource::EnumType { underlying: None } => {
            let span = &TokenRange::internal();
            let ty = context.type_of_types(span);
            let initializer = context.type_ref(HMIRExprTypeKind::Int(ENUM_INT), span);
            let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);
            Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
        }
        DefSource::Function { prototype, body } => lower_function(context, name, prototype, *body),
        DefSource::Global {
            comptime,
            ty,
            initializer,
            linkage,
            explicit_extern,
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
                        &chl_log::catalogue::typecheck::CANNOT_INFER,
                        subject,
                    );
                };
                let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);
                return Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)));
            }
            let global = HMIRGlobal::new(
                context.finish(),
                ty,
                initializer,
                *linkage,
                link_name,
                *explicit_extern,
            );

            Ok(HMIRDefKind::Global(Box::new(global)))
        }
        DefSource::EnumVariant {
            enumeration,
            index,
            scoped_type,
        } => lower_enum_variant(context, name, enumeration, *index, scoped_type.as_ref()),
    }
}

fn lower_function<'a>(
    mut context: BodyLowering<'a>,
    name: &QualifiedName,
    prototype: &'a HIRFunctionPrototype,
    body: Option<&'a HIRFunctionBody>,
) -> CHLResult<HMIRDefKind> {
    let (defaults, decls) = lower_defaults(&mut context, prototype)?;
    let (params, holes, return_type, contract, root) =
        context.with_stage(prototype.comptime, |this| -> CHLResult<_> {
            let mut params = Vec::new();
            this.parameter_holes = Some(Vec::new());
            for param in declared_params(prototype) {
                let ty = lower_type(this, &param.ty)?;
                if let Some(name) = &param.name
                    && this
                        .parameter_holes
                        .as_ref()
                        .unwrap()
                        .iter()
                        .any(|&local| this.local(local).name() == Some(name))
                {
                    return log_hir_error(
                        &param.range,
                        &typecheck::DUPLICATE_ITEM,
                        (
                            format!("binding '{name}'"),
                            "this function signature".into(),
                        ),
                    );
                }
                params.push(this.declare(param.name.as_ref(), ty, param.comptime, &param.range));
            }
            let holes = this.parameter_holes.take().unwrap();
            let return_type = lower_return_type(this, prototype)?;
            let contract = lower_contract(this, prototype, &holes)?;
            let root = body
                .map(|body| lower_function_body(this, body))
                .transpose()?;

            Ok((params, holes, return_type, contract, root))
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
    let prototype = HMIRFnPrototype::new(
        symbol_name, prototype.linkage, prototype.constexpr, signature, decls, defaults,
    );
    let transparent = matches!(body, Some(HIRFunctionBody::TypeAlias(_)));
    let def = root.map(|root| context.finish_definition(root, transparent));

    let mut function = HMIRFunction::new(context.finish(), prototype, params.into(), def);
    function.prepend_inferred_parameters(&holes);
    Ok(HMIRDefKind::Function(Box::new(function)))
}

// A parameter list of a lone unnamed 'void' declares no parameters
pub(crate) fn declared_params(prototype: &HIRFunctionPrototype) -> &[HIRParameter] {
    match prototype.params.as_slice() {
        [param] if param.name.is_none() && is_void(&param.ty) => &[],
        params => params,
    }
}

fn lower_defaults(
    context: &mut BodyLowering<'_>,
    prototype: &HIRFunctionPrototype,
) -> CHLResult<(HMIRBodyContext, Vec<HMIRFnParamDecl>)> {
    let mut defaults = context.detached(true);
    let params = declared_params(prototype);
    let last_default = params.iter().rposition(|param| {
        param
            .default
            .as_ref()
            .is_some_and(|expr| !matches!(expr.kind, HIRExprKind::Hole))
    });
    let mut lowered = Vec::new();
    for (index, param) in params.iter().enumerate() {
        let default = match &param.default {
            None => None,
            Some(HIRExpression { kind: HIRExprKind::Hole, range }) => Some(defaults.hole(range)),
            Some(default) => Some(lower_expr(&mut defaults, default)?),
        };
        let local = if last_default.is_some_and(|last| index < last)
            && let Some(name) = &param.name
        {
            let placeholder = defaults.hole(&param.ty.range);
            let comptime = param.comptime || prototype.comptime;
            Some(defaults.declare(Some(name), placeholder, comptime, &param.range))
        } else {
            None
        };
        lowered.push(HMIRFnParamDecl::new(param.name.clone(), default, local));
    }
    Ok((defaults.finish(), lowered))
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

const ENUM_INT: HMIRIntType = HMIRIntType::new(HMIRIntWidth::I32, true);

fn lower_enum_variant<'a>(
    mut context: BodyLowering<'a>,
    name: &QualifiedName,
    enumeration: &'a HIREnumDefinition,
    index: usize,
    scoped_type: Option<&QualifiedName>,
) -> CHLResult<HMIRDefKind> {
    let variants = &enumeration.variants;
    let span = &variants[index].range;
    let fixed = enumeration.underlying.is_some();
    // Implicit values count from the nearest explicit one rather than the previous variant, so
    // evaluating a long enum does not recurse through every variant before it
    let explicit = variants[..index]
        .iter()
        .rposition(|variant| variant.value.is_some());
    let initializer = match (&variants[index].value, explicit) {
        (Some(value), _) => lower_expr(&mut context, value)?,
        (None, None) => context.int_literal(ENUM_INT, index as i128, span),
        (None, Some(explicit)) => {
            let base = QualifiedName::new(name.namespace.clone(), variants[explicit].name.clone());
            let mut lhs = context.def_expr(base, span);
            let sum_type = if fixed {
                let wide = HMIRIntType::new(HMIRIntWidth::I64, true);
                let target = context.type_ref(HMIRExprTypeKind::Int(wide), span);
                lhs = context.coerce(HMIRCoerceMode::CCast, lhs, target, span);
                wide
            } else {
                ENUM_INT
            };
            let rhs = context.int_literal(sum_type, (index - explicit) as i128, span);
            let op = HMIRBinaryOp::Add;
            context.op(HMIROp::BinOp { op, lhs, rhs }, span)
        }
    };
    let ty = if let Some(name) = scoped_type {
        context.push(
            HMIRExprKind::Name {
                name: name.clone(),
                tag: Some(HIRTagKind::Enum),
            },
            span,
        )
    } else if let Some(base) = &enumeration.underlying {
        lower_type(&mut context, base)?
    } else {
        context.type_ref(HMIRExprTypeKind::Int(ENUM_INT), span)
    };
    let initializer = if fixed {
        context.op(HMIROp::EnumMember { value: initializer, target: ty }, span)
    } else {
        initializer
    };
    let global = HMIRComptimeGlobal::new(context.finish(), ty, initializer);

    Ok(HMIRDefKind::ComptimeGlobal(Box::new(global)))
}

// The conditions are lowered into a body of their own, where a local stands for each parameter
// they may name. Those locals take their types from the signature.
fn lower_contract(
    context: &mut BodyLowering<'_>,
    prototype: &HIRFunctionPrototype,
    holes: &[chl_hmir::body::HMIRLocalID],
) -> CHLResult<HMIRContract> {
    let contract = &prototype.contract;
    let span = &prototype.range;
    if contract.precondition.is_none() && contract.postcondition.is_none() {
        let (body, params) = (HMIRBodyContext::new(), Box::default());
        return Ok(HMIRContract::new(body, params, contract.safe, None, None));
    }

    let holes = holes
        .iter()
        .map(|&local| context.local(local).clone())
        .collect::<Vec<_>>();
    let declared = declared_params(prototype);
    let mut conditions = context.detached(prototype.comptime);
    let mut params = Vec::new();
    for hole in holes {
        let ty = conditions.hole(hole.span());
        params.push(conditions.declare(hole.name(), ty, true, hole.span()));
    }
    for param in declared {
        let ty = conditions.hole(&param.ty.range);
        params.push(conditions.declare(param.name.as_ref(), ty, param.comptime, &param.range));
    }

    let precondition = contract
        .precondition
        .as_ref()
        .map(|condition| lower_expr(&mut conditions, condition))
        .transpose()?;
    let postcondition = contract
        .postcondition
        .as_ref()
        .map(|(binding, condition)| {
            conditions.scoped(|this| -> CHLResult<_> {
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
        params.into(),
        contract.safe,
        precondition,
        postcondition,
    ))
}

fn lower_function_body<'a>(
    context: &mut BodyLowering<'a>,
    body: &'a HIRFunctionBody,
) -> CHLResult<HMIRExprID> {
    match body {
        HIRFunctionBody::Block { statements, range } => {
            let statements = lower_scope_statements(context, statements)?;
            Ok(context.block(statements, true, false, range))
        }
        HIRFunctionBody::Expression(expr) => lower_expr(context, expr),
        HIRFunctionBody::TypeAlias(ty) => context.scoped(|context| lower_type(context, ty)),
    }
}
