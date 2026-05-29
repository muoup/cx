use cx_ast::{ast::CXExpression, pattern::CXPattern};
use cx_mir::mir::program::EnvironmentNamespace;
use cx_util::{CXResult, identifier::CXIdent, namespace::QualifiedName};

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub struct TypeConstructor {
    pub union_name: QualifiedName,
    pub variant_name: CXIdent,
    pub inner_name: Option<CXIdent>,
}

pub fn resolve_type_constructor_pattern(
    env: &mut TypeEnvironment,
    _namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    pattern: &CXPattern,
) -> CXResult<TypeConstructor> {
    let CXPattern::Variant {
        union_name,
        variant_name,
        inner,
    } = pattern
    else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Expected qualified tagged union variant pattern"
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

    Ok(TypeConstructor {
        union_name: union_name.clone(),
        variant_name: variant_name.clone(),
        inner_name,
    })
}
