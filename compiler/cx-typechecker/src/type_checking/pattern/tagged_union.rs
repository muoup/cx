use cx_ast::ast::{expression::CXExpression, pattern::CXPattern, template::CXTemplateInput};
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::{environment::TypeEnvironment, log_typecheck_error, typecheck_error};

pub struct TypeConstructor {
    pub union_name: QualifiedName,
    pub variant_name: CXIdent,
    pub template_input: Option<CXTemplateInput>,
    pub inner_name: Option<CXIdent>,
}

pub fn resolve_type_constructor_pattern(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    pattern: &CXPattern,
) -> CXResult<TypeConstructor> {
    let CXPattern::Variant {
        constructor,
        template_input,
        inner,
    } = pattern
    else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Expected qualified tagged union variant pattern"
        );
    };

    let Some((union_namespace, union_name)) = constructor.namespace.parent_and_name() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Expected tagged union variant pattern to name a type member constructor"
        );
    };

    let inner_name = match inner.as_deref() {
        None => None,
        Some(CXPattern::Binding(name)) => Some(name.clone()),
        Some(_) => {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Tagged union variant payload pattern must be a binding"
            );
        }
    };

    let union_name = QualifiedName::new(union_namespace, union_name);

    let union_name = env
        .get_symbol(namespace, &union_name, Some(expr.token_range()))?
        .and_then(|symbol| symbol.as_pattern_target(&env.symbols))
        .ok_or_else(|| {
            typecheck_error!(
                env,
                Some(expr.token_range()),
                "Could not resolve pattern target '{}'",
                union_name
            )
        })?;

    Ok(TypeConstructor {
        union_name,
        variant_name: constructor.name.clone(),
        template_input: template_input.clone(),
        inner_name,
    })
}
