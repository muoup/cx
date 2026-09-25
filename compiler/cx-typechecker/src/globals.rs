use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::modifiers::{HIR_CONST, HIRSymbolNameScheme};
use cx_hir::ast::types::HIRType;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::thir::contextual_eq::TypeContextEqual;
use cx_thir::thir::data::THIRType;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_thir::thir::name_mangling::mangle_rootable_name;
use cx_thir::thir::r#type::{THIRArrayLength, THIRTypeKind};
use cx_thir::type_context::THIRTypeContext;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

use crate::environment::TypeEnvironment;
use crate::symbol::completion::{complete_type, ensure_valid_type_component};
use crate::type_checking::initializer::typecheck_object_initializer;

pub(crate) fn lower_global(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    name: CXIdent,
    hir_type: &HIRType,
    linkage: LinkageMode,
    name_scheme: HIRSymbolNameScheme,
    initializer: Option<&HIRExpression>,
) -> CXResult<()> {
    let declared_type = complete_type(env, &namespace, hir_type)?;
    if !matches!(
        declared_type.kind,
        THIRTypeKind::Array {
            length: THIRArrayLength::Implicit,
            ..
        }
    ) {
        ensure_valid_type_component(
            env,
            hir_type.range(),
            &declared_type,
            "a global variable",
            true,
        )?;
    }

    let symbol_name = mangle_rootable_name(
        env.symbols.get_global_registry(),
        &QualifiedName::new(namespace.clone(), name.clone()),
        name_scheme,
    );

    let previous = env.items.generated_global(&symbol_name).cloned();
    if let Some(previous) = &previous {
        if previous.initializer.is_some() && initializer.is_some() {
            return env.log_error(
                hir_type.range(),
                &catalogue::VARIABLE_REDECLARATION,
                format!("{}", name),
            );
        }

        if !previous._type.contextual_eq(&declared_type, &env.symbols)
            && !incomplete_array_declaration_compatible(env, &previous._type, &declared_type)
        {
            return env.log_error(
                hir_type.range(),
                &catalogue::VARIABLE_REDECLARATION,
                format!("{}", name),
            );
        }
    }

    let (global_type, comptime_init) = initializer
        .map(|init| {
            typecheck_object_initializer(env, namespace, init, &declared_type)
                .map(|checked| (checked.object_type, Some(checked.value)))
        })
        .transpose()?
        .unwrap_or_else(|| (declared_type.clone(), None));

    if let Some(previous) = &previous
        && !previous._type.contextual_eq(&global_type, &env.symbols)
        && !incomplete_array_declaration_compatible(env, &previous._type, &global_type)
    {
        return env.log_error(
            hir_type.range(),
            &catalogue::VARIABLE_REDECLARATION,
            format!("{}", name),
        );
    }

    let global_type = match (&global_type.kind, previous.as_ref()) {
        (
            THIRTypeKind::Array {
                length: THIRArrayLength::Implicit,
                ..
            },
            Some(previous),
        ) if initializer.is_none()
            && matches!(
                previous._type.kind,
                THIRTypeKind::Array {
                    length: THIRArrayLength::Known(_),
                    ..
                }
            ) =>
        {
            previous._type.clone()
        }
        _ => global_type,
    };

    if matches!(
        global_type.kind,
        THIRTypeKind::Array {
            length: THIRArrayLength::Implicit,
            ..
        }
    ) && linkage != LinkageMode::Extern
    {
        return env.log_error(
            hir_type.range(),
            &catalogue::INCOMPLETE_TYPE,
            format!("{}", name),
        );
    }

    if linkage != LinkageMode::Extern {
        ensure_valid_type_component(
            env,
            hir_type.range(),
            &global_type,
            "a global variable",
            true,
        )?;
    }

    let global_value_type = env.symbols.mem_ref_to(global_type.clone());
    env.symbols.insert_value(
        QualifiedName::new(namespace.clone(), name.clone()),
        THIRExpression {
            token_range: cx_tokens::TokenRange::internal(),
            kind: THIRExpressionKind::GlobalVariable {
                symbol: CXIdent::new(symbol_name.clone()),
            },
            _type: global_value_type,
        },
    );

    if linkage == LinkageMode::Extern && previous.is_some() {
        return Ok(());
    }

    let is_mutable = !global_type.get_specifier(HIR_CONST);
    let global = THIRGlobalVariable {
        name: CXIdent::new(symbol_name),
        _type: global_type,

        is_mutable,
        initializer: comptime_init,

        linkage,
    };

    env.items.push_generated_global(global, true);
    Ok(())
}

fn incomplete_array_declaration_compatible(
    env: &TypeEnvironment,
    declaration: &THIRType,
    definition: &THIRType,
) -> bool {
    if declaration.specifiers != definition.specifiers {
        return false;
    }
    match (&declaration.kind, &definition.kind) {
        (
            THIRTypeKind::Array {
                length: THIRArrayLength::Implicit,
                inner_type: left,
            },
            THIRTypeKind::Array {
                length: THIRArrayLength::Known(_),
                inner_type: right,
            },
        )
        | (
            THIRTypeKind::Array {
                length: THIRArrayLength::Known(_),
                inner_type: left,
            },
            THIRTypeKind::Array {
                length: THIRArrayLength::Implicit,
                inner_type: right,
            },
        ) => env.type_eq(
            env.symbols.resolve_type_id(*left),
            env.symbols.resolve_type_id(*right),
        ),
        _ => false,
    }
}
