use cx_hir::ast::{HIR, HIRStmt, global_var::HIRGlobalVariable, modifiers::HIR_CONST};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::THIRFunction;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::THIRGlobalVariable;

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

pub fn typecheck(env: &mut TypeEnvironment, ast: &HIR) -> CXResult<()> {
    for definition in &ast.definition_stmts {
        let namespace = EnvironmentNamespace::from(definition.namespace.clone());

        match &definition.stmt {
            HIRStmt::FunctionDefinition {
                prototype,
                template_prototype: None,
                body: Some(body),
                ..
            } => {
                let prototype = complete_prototype(env, &namespace, prototype)?;
                typecheck_function(env, &namespace, prototype, body)?;
            }

            HIRStmt::FunctionDefinition {
                prototype,
                template_prototype: None,
                body: None,
                ..
            } => {
                let prototype = complete_prototype(env, &namespace, prototype)?;
                env.items.push_generated_function(THIRFunction {
                    prototype,
                    body: None,
                });
            }

            HIRStmt::GlobalVariableDefinition {
                variable:
                    HIRGlobalVariable::Standard {
                        name,
                        _type,
                        linkage,
                        symbol_name_scheme: symbol_naming,
                        initializer,
                        ..
                    },
                ..
            } => {
                let _type = complete_type(env, &namespace, _type)?;
                let symbol_name = completed_symbol_name(
                    env,
                    QualifiedName::new(namespace.clone(), name.clone()),
                    *symbol_naming,
                );
                let (global_type, comptime_init) = initializer
                    .as_ref()
                    .map(|init| {
                        let expression = typecheck_expr(env, &namespace, init, Some(&_type))
                            .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
                        let (global_type, expression) = match &expression.kind {
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(
                                operand.kind,
                                THIRExpressionKind::ArrayInitializer { .. }
                            ) =>
                            {
                                (operand._type.clone(), operand.as_ref().clone())
                            }
                            THIRExpressionKind::ArrayInitializer { .. } => {
                                (expression._type.clone(), expression)
                            }
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(
                                operand.kind,
                                THIRExpressionKind::GlobalVariable { .. }
                            ) =>
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
                    name: CXIdent::new(symbol_name),
                    _type: global_type,

                    is_mutable: _type.get_specifier(HIR_CONST),
                    linkage: *linkage,
                    initializer: comptime_init,
                };

                env.items.push_generated_global(global);
            }

            _ => {}
        }
    }

    fulfill_requests(env)?;

    Ok(())
}
