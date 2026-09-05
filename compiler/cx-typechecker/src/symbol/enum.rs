use cx_hir::ast::global_var::HIREnumDefinition;
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

pub struct EnumBlockResolution<'a> {
    env: &'a TypeEnvironment<'a>,
    block: &'a HIREnumDefinition,
    namespace: &'a NamespacePath,
}

impl<'a> EnumBlockResolution<'a> {
    pub fn variant_expr(&self, idx: usize) -> Option<&MIRSymbol> {
        self.block.variants.get(idx).and_then(|variant| {
            let symbol = QualifiedName::new(self.namespace.clone(), variant.name.clone());

            self.env
                .symbols
                .get_preresolved_symbol(&symbol)
                .or_else(|| {
                    unreachable!("Expected enum variant {symbol} to be in the global registry")
                })
        })
    }
}

pub(crate) fn resolve_enum_block<'a, 'b>(
    env: &'a mut TypeEnvironment<'b>,
    namespace: &'a NamespacePath,
    block_idx: usize,
) -> CXResult<EnumBlockResolution<'a>> {
    let (_, data) = env
        .symbols
        .get_global_registry()
        .get_bucket(namespace)
        .expect("Expected enum block to be in the global registry");

    let block = data
        .get_enum_block(block_idx)
        .expect("Expected enum block to be in the global registry");

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

    Ok(EnumBlockResolution {
        env,
        block,
        namespace,
    })
}
