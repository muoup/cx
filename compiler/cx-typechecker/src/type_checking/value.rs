pub(crate) mod identifiers;
pub(crate) mod literals;
pub(crate) mod locals;
pub(crate) mod moves;
pub(crate) mod unsafe_ops;

use crate::environment::TypeEnvironment;
use cx_thir::{
    thir::{
        data::THIRType,
        expression::{THIRExpression, THIRExpressionKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

pub(crate) struct IndirectBase {
    pub source: THIRExpression,
    pub source_type: THIRType,
    pub owned: bool,
}

pub(crate) fn resolve_indirect_base(
    env: &mut TypeEnvironment,
    mut source: THIRExpression,
) -> IndirectBase {
    loop {
        let source_type = source._type.clone();

        if let Some(inner_type) = env.symbols.mem_ref_inner(&source_type).cloned() {
            if let Some(ptr_inner) = env.symbols.ptr_inner(&inner_type).cloned() {
                let pointer = THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Copy {
                        source: Box::new(source),
                    },
                    _type: env.symbols.pointer_to(ptr_inner.clone()),
                };

                return IndirectBase {
                    source: THIRExpression {
                        token_range: TokenRange::internal(),
                        kind: THIRExpressionKind::Typechange(Box::new(pointer)),
                        _type: env.symbols.mem_ref_to(ptr_inner.clone()),
                    },
                    source_type: ptr_inner,
                    owned: false,
                };
            }

            if let Some(array_inner) = env.symbols.array_inner(&inner_type).cloned() {
                let pointer = THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::TypeConversion {
                        operand: Box::new(source),
                        conversion: cx_thir::thir::expression::THIRCoercion::ReinterpretBits,
                    },
                    _type: env.symbols.pointer_to(array_inner.clone()),
                };

                return IndirectBase {
                    source: THIRExpression {
                        token_range: TokenRange::internal(),
                        kind: THIRExpressionKind::Typechange(Box::new(pointer)),
                        _type: env.symbols.mem_ref_to(array_inner.clone()),
                    },
                    source_type: array_inner,
                    owned: false,
                };
            }

            if env.symbols.mem_ref_inner(&inner_type).is_some() {
                source = THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Copy {
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
                source: THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Typechange(Box::new(source)),
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
