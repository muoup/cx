use cx_hir::ast::modifiers::HIR_CONST;
use cx_hir::decomposition::{HIRGenerationAST, HIRGenerationStmt};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::{THIRGlobalVarKind, THIRGlobalVariable};
use cx_util::linkage::LinkageMode;

pub mod environment;
pub mod log;
pub mod symbol;

pub(crate) mod requests;

pub(crate) mod comptime;
mod globals;
mod type_checking;

use crate::globals::canonicalize_global_initializer;
use crate::requests::fulfill_requests;
use crate::symbol::completion::{complete_prototype, complete_type, completed_symbol_name};
use crate::type_checking::typechecker::typecheck_expr;
use crate::{environment::TypeEnvironment, type_checking::functions::typecheck_function};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub fn typecheck(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ast: &HIRGenerationAST,
) -> CXResult<()> {
    for stmt in ast.generation_stmts.iter() {
        match stmt {
            HIRGenerationStmt::Function { prototype, body } => {
                let prototype = complete_prototype(env, namespace, prototype)?;
                typecheck_function(env, namespace, prototype.clone(), body)?;
            }

            HIRGenerationStmt::StringLiteral { name, value } => {
                let global = THIRGlobalVariable {
                    is_mutable: false,
                    linkage: LinkageMode::Static,
                    kind: THIRGlobalVarKind::StringLiteral {
                        name: name.clone(),
                        value: value.clone(),
                    },
                };

                env.items.push_generated_global(global);
            }

            HIRGenerationStmt::AddressableGlobal {
                name,
                _type,
                linkage,
                symbol_naming,
                initializer,
            } => {
                let _type = complete_type(env, namespace, _type)?;
                let symbol_name = completed_symbol_name(
                    env,
                    QualifiedName::new(namespace.clone(), name.clone()),
                    *symbol_naming,
                );
                let (global_type, comptime_init) = initializer
                    .as_ref()
                    .map(|init| {
                        let expression = typecheck_expr(env, namespace, init, Some(&_type))
                            .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
                        let (global_type, expression) = match &expression.kind {
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(operand.kind, THIRExpressionKind::ArrayInitializer { .. }) =>
                            {
                                (operand._type.clone(), operand.as_ref().clone())
                            }
                            THIRExpressionKind::ArrayInitializer { .. } => {
                                (expression._type.clone(), expression)
                            }
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) =>
                            {
                                (_type.clone(), expression)
                            }
                            _ => (_type.clone(), expression),
                        };
                        Ok((
                            global_type,
                            canonicalize_global_initializer(env, expression)?,
                        ))
                    })
                    .transpose()?
                    .unwrap_or_else(|| (_type.clone(), None));

                if !env.type_eq(&_type, &global_type) {
                    let global_value_type = env.symbols.mem_ref_to(global_type.clone());
                    env.symbols.insert_value(
                        QualifiedName::new(namespace.clone(), name.clone()),
                        THIRExpression {
                            token_range: cx_tokens::TokenRange::internal(),
                            kind: THIRExpressionKind::GlobalVariable {
                                symbol: CXIdent::new(symbol_name.clone()),
                            },
                            _type: global_value_type,
                        },
                    );
                }

                let global = THIRGlobalVariable {
                    is_mutable: _type.get_specifier(HIR_CONST),
                    linkage: *linkage,
                    kind: THIRGlobalVarKind::Variable {
                        name: CXIdent::new(symbol_name),
                        _type: global_type,
                        initializer: comptime_init,
                    },
                };

                env.items.push_generated_global(global);
            }
        }
    }

    fulfill_requests(env)?;

    Ok(())
}
