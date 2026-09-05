use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern, template::HIRTemplateInput};
use cx_log::CXResult;
use cx_thir::NamespacePath;
use cx_util::{identifier::CXIdent, module::QualifiedName};

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
            "Expected qualified tagged union variant pattern".to_string(),
        );
    };

    let Some((union_namespace, union_name)) = constructor.namespace.parent_and_name() else {
        return env.log_error(
            expr.token_range(),
            "Expected tagged union variant pattern to name a type member constructor".to_string(),
        );
    };

    let inner_name = match inner.as_deref() {
        None => None,
        Some(HIRPattern::Binding(name)) => Some(name.clone()),
        Some(_) => {
            return env.log_error(
                expr.token_range(),
                "Tagged union variant payload pattern must be a binding".to_string(),
            );
        }
    };

    let union_name = QualifiedName::new(union_namespace, union_name);

    let lookup = match env
        .lookup_symbol(namespace, &union_name)
        .map_err(|error| env.complete_err(error, expr.token_range()))?
    {
        Some(lookup) => Some(lookup),
        None => env
            .lookup_tag_symbol(namespace, &union_name)
            .map_err(|error| env.complete_err(error, expr.token_range()))?,
    };
    let union_name = lookup
        .map(|lookup| env.resolve_lookup(namespace, lookup))
        .transpose()?
        .and_then(|symbol| symbol.as_pattern_target(&env.symbols))
        .ok_or_else(|| {
            env.error(
                expr.token_range(),
                format!("Could not resolve pattern target '{}'", union_name),
            )
        })?;

    Ok(TypeConstructor {
        union_name,
        variant_name: constructor.name.clone(),
        template_input: template_input.clone(),
        inner_name,
    })
}
