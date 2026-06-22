use cx_ast::ast::global_var::CXEnumDefinition;
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::expression::{MIRExpression, MIRExpressionKind},
    symbol::MIRSymbol,
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

use crate::{
    comptime::evaluate_comptime_expression, environment::TypeEnvironment,
    type_checking::typechecker::typecheck_expr,
};

pub struct EnumBlockResolution<'a> {
    env: &'a TypeEnvironment<'a>,
    block: &'a CXEnumDefinition,
    namespace: &'a EnvironmentNamespace,
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
    namespace: &'a EnvironmentNamespace,
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

    let mut idx = 0;

    for variant in &block.variants {
        let symbol = QualifiedName::new(namespace.clone(), variant.name.clone());
        let value = variant
            .value
            .as_ref()
            .map(|expr| {
                typecheck_expr(env, namespace, expr, None)
                    .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                    .and_then(|v| evaluate_comptime_expression(env, v))
                    .and_then(|v| {
                        v.as_integer().ok_or_else(|| {
                            env.error(
                                v.token_range,
                                "Expected enum variant value to be an integer".to_string(),
                            )
                        })
                    })
            })
            .transpose()?
            .inspect(|&v| {
                idx = v;
            })
            .unwrap_or(idx);

        idx += 1;

        env.symbols.insert_value(
            symbol,
            MIRExpression {
                token_range: TokenRange::internal(),
                _type: env.get_intrinsic_type("int"),
                kind: MIRExpressionKind::IntLiteral(value),
            },
        );
    }

    Ok(EnumBlockResolution {
        env,
        block,
        namespace,
    })
}
