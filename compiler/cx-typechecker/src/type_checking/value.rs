pub(crate) mod identifiers;
pub(crate) mod literals;
pub(crate) mod locals;
pub(crate) mod moves;
pub(crate) mod unsafe_ops;

use crate::environment::TypeEnvironment;
use cx_log::CXResult;
use cx_mir::{
    mir::{
        data::{MIRType, MIRTypeKind},
        expression::{MIRExpression, MIRExpressionKind},
    },
    type_context::MIRTypeContext,
};
use cx_tokens::TokenRange;

pub(crate) struct IndirectBase {
    pub source: MIRExpression,
    pub source_type: MIRType,
    pub owned: bool,
}

pub(crate) fn resolve_indirect_base(
    env: &mut TypeEnvironment,
    mut source: MIRExpression,
) -> IndirectBase {
    loop {
        let source_type = source._type.clone();

        if let Some(inner_type) = env.symbols.mem_ref_inner(&source_type).cloned() {
            if let Some(ptr_inner) = env.symbols.ptr_inner(&inner_type).cloned() {
                let pointer = MIRExpression {
                    token_range: TokenRange::internal(),
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(source),
                    },
                    _type: env.symbols.pointer_to(ptr_inner.clone()),
                };

                return IndirectBase {
                    source: MIRExpression {
                        token_range: TokenRange::internal(),
                        kind: MIRExpressionKind::Typechange(Box::new(pointer)),
                        _type: env.symbols.mem_ref_to(ptr_inner.clone()),
                    },
                    source_type: ptr_inner,
                    owned: false,
                };
            }

            if env.symbols.mem_ref_inner(&inner_type).is_some() {
                source = MIRExpression {
                    token_range: TokenRange::internal(),
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(source),
                    },
                    _type: inner_type,
                };
                continue;
            }

            return IndirectBase {
                source,
                source_type: inner_type,
                owned: false,
            };
        }

        if let Some(inner_type) = env.symbols.ptr_inner(&source_type).cloned() {
            return IndirectBase {
                source: MIRExpression {
                    token_range: TokenRange::internal(),
                    kind: MIRExpressionKind::Typechange(Box::new(source)),
                    _type: env.symbols.mem_ref_to(inner_type.clone()),
                },
                source_type: inner_type,
                owned: false,
            };
        }

        return IndirectBase {
            source,
            source_type,
            owned: true,
        };
    }
}

pub(crate) fn ensure_valid_allocation_type(
    env: &mut TypeEnvironment,
    range: TokenRange,
    context: &str,
    ty: &MIRType,
) -> CXResult<()> {
    match &ty.kind {
        MIRTypeKind::Function { .. } => env.log_error(range, format!("Cannot create {} of function type '{}'; use a pointer to the function type instead", context, ty.display_with(&env.symbols))),
        MIRTypeKind::Str => env.log_error(range, format!("Cannot create {} of unsized type 'str'; use '&str' instead", context)),
        MIRTypeKind::Array { inner_type, .. } => {
            let inner_type = env.symbols.resolve_type_id(*inner_type).clone();
            ensure_valid_allocation_type(env, range.clone(), "an array element", &inner_type)
        }
        _ => Ok(()),
    }
}
