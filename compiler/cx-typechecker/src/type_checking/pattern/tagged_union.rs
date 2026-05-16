use cx_ast::{
    ast::{CXBinOp, CXExprKind, CXExpression},
    data::{CXTypeKind, PredeclarationType},
};
use cx_mir::mir::{data::MIRType, program::MIRBaseMappings};
use cx_util::{CXResult, identifier::CXIdent};

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub struct TypeConstructor<'a> {
    pub union_type: MIRType,
    pub variant_name: CXIdent,
    pub inner: Option<&'a CXExpression>,
}

pub fn deconstruct_type_constructor<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    pattern: &'a CXExpression,
) -> CXResult<TypeConstructor<'a>> {
    let (constructor, inner) = match &pattern.kind {
        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs: inner,
        } => (lhs.as_ref(), Some(inner.as_ref())),

        _ => (pattern, None),
    };

    let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs: union,
        rhs: variant,
    } = &constructor.kind
    else {
        return log_typecheck_error!(
            env,
            Some(pattern.token_range()),
            "Expected type constructor"
        );
    };

    let union_name = match &union.kind {
        CXExprKind::Identifier(union_name) => {
            let as_type = CXTypeKind::Identifier {
                predeclaration: PredeclarationType::None,
                name: union_name.clone(),
            }
            .to_type();

            env.complete_type(base_data, union, &as_type)?
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let as_type = CXTypeKind::TemplatedIdentifier {
                name: name.clone(),
                input: template_input.clone(),
            }
            .to_type();

            env.complete_type(base_data, union, &as_type)?
        }

        _ => {
            return log_typecheck_error!(
                env,
                Some(union.token_range()),
                "Expected union name in type constructor pattern"
            );
        }
    };

    let CXExprKind::Identifier(variant_name) = &variant.kind else {
        return log_typecheck_error!(
            env,
            Some(variant.token_range()),
            "Expected variant name in type constructor pattern"
        );
    };

    Ok(TypeConstructor {
        union_type: union_name,
        variant_name: variant_name.clone(),
        inner,
    })
}
