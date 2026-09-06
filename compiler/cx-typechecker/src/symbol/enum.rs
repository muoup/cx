use cx_log::CXResult;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        expression::{THIRBinOp, THIRExpression, THIRExpressionKind, THIRIntBinOp},
        r#type::THIRTypeKind,
    },
};
use cx_tokens::TokenRange;

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        coercion::{implicit::implicit_cast, implicit::promotion::std_rval_promotion},
        typechecker::typecheck_expr,
    },
};

pub(crate) fn resolve_enum_block(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    block_idx: usize,
    variant_index: usize,
) -> CXResult<MIRSymbol> {
    let block = env.symbols.get_global_registry().enum_block(namespace, block_idx)
        .expect("Expected enum block in registry");
    let requested = QualifiedName::new(namespace.clone(), block.variants[variant_index].name.clone());
    if let Some(symbol) = env.symbols.cached(&requested, false) {
        return Ok(symbol.clone());
    }

    let integer_type = env.get_intrinsic_type("int");
    let integer_kind = match &integer_type.kind {
        THIRTypeKind::Integer { _type, .. } => *_type,
        _ => unreachable!("intrinsic int is not an integer type"),
    };
    let one = THIRExpression {
        token_range: TokenRange::internal(),
        _type: integer_type.clone(),
        kind: THIRExpressionKind::IntLiteral(1),
    };
    let mut next_value = THIRExpression {
        token_range: TokenRange::internal(),
        _type: integer_type.clone(),
        kind: THIRExpressionKind::IntLiteral(0),
    };

    for variant in &block.variants {
        let symbol = QualifiedName::new(namespace.clone(), variant.name.clone());
        let value = variant
            .value
            .as_ref()
            .map(|expr| {
                typecheck_expr(env, namespace, expr, None)
                    .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                    .and_then(|v| std_rval_promotion(env, v))
                    .and_then(|v| implicit_cast(env, v, &integer_type))
            })
            .transpose()?
            .unwrap_or_else(|| next_value.clone());

        next_value = match &value.kind {
            THIRExpressionKind::IntLiteral(value) => THIRExpression {
                token_range: TokenRange::internal(),
                _type: integer_type.clone(),
                kind: THIRExpressionKind::IntLiteral(value + 1),
            },
            _ => THIRExpression {
                token_range: TokenRange::internal(),
                _type: integer_type.clone(),
                kind: THIRExpressionKind::BinaryOperation {
                    lhs: Box::new(value.clone()),
                    rhs: Box::new(one.clone()),
                    op: THIRBinOp::Integer {
                        itype: integer_kind,
                        op: THIRIntBinOp::ADD,
                    },
                },
            },
        };

        env.symbols.insert_value(symbol, value);
    }

    Ok(env.symbols.cached(&requested, false).expect("enum variant completed").clone())
}
