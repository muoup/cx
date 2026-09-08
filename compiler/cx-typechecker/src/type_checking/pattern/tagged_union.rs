use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern, template::HIRTemplateInput};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_util::identifier::CXIdent;

use crate::environment::TypeEnvironment;

pub struct TypeConstructor {
    pub union_name: QualifiedName,
    pub variant_name: CXIdent,
    pub template_input: Option<HIRTemplateInput>,
    pub inner_name: Option<CXIdent>,
}

pub fn resolve_type_constructor_pattern(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    pattern: &HIRPattern,
) -> CXResult<TypeConstructor> {
    let HIRPattern::Variant {
        constructor,
        template_input,
        inner,
    } = pattern
    else {
        return env.log_error(
            expr.token_range(),
            &catalogue::EXPECTED_QUALIFIED_TAGGED_UNION_VARIANT_PATTERN,
            (),
        );
    };

    let Some((union_namespace, union_name)) = constructor.namespace.clone().parent_and_name()
    else {
        return env.log_error(
            expr.token_range(),
            &catalogue::EXPECTED_TAGGED_UNION_VARIANT_PATTERN_TO_NAME_A_TYPE_MEMBER,
            (),
        );
    };

    let inner_name = match inner.as_deref() {
        None => None,
        Some(HIRPattern::Binding(name)) => Some(name.clone()),
        Some(_) => {
            return env.log_error(
                expr.token_range(),
                &catalogue::TAGGED_UNION_VARIANT_PAYLOAD_PATTERN_MUST_BE_A_BINDING,
                (),
            );
        }
    };

    let union_name = QualifiedName::new(union_namespace, union_name);

    let lookup = env
        .lookup_symbol(namespace, &union_name, None)
        .map_err(|error| env.complete_err(error, expr.token_range()))?;
    let union_name = lookup
        .map(|lookup| env.resolve_lookup(namespace, lookup))
        .transpose()?
        .and_then(|symbol| symbol.as_pattern_target(&env.symbols))
        .ok_or_else(|| {
            env.error(
                expr.token_range(),
                &catalogue::COULD_NOT_RESOLVE_PATTERN_TARGET,
                format!("{}", union_name),
            )
        })?;

    Ok(TypeConstructor {
        union_name,
        variant_name: constructor.name.clone(),
        template_input: template_input.clone(),
        inner_name,
    })
}
