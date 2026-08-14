use cx_hir::ast::modifiers::HIR_CONST;
use cx_hir::decomposition::{HIRGenerationAST, HIRGenerationStmt};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::global::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::linkage::LinkageMode;

pub mod environment;
pub mod log;
pub mod symbol;

pub(crate) mod requests;

pub(crate) mod comptime;
mod type_checking;

use crate::comptime::evaluate_comptime_expression;
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
                let global = MIRGlobalVariable {
                    is_mutable: false,
                    linkage: LinkageMode::Static,
                    kind: MIRGlobalVarKind::StringLiteral {
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
                let comptime_init = initializer
                    .as_ref()
                    .map(|init| {
                        typecheck_expr(env, namespace, init, Some(&_type))
                            .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))
                            .and_then(|tc| evaluate_comptime_expression(env, tc))
                            .and_then(|ce| ce.as_integer().ok_or_else(|| {
                                env.error(init.token_range(), "Global variable initializer must be a constant integer expression".to_string())
                            }))
                    })
                    .transpose()?;

                let global = MIRGlobalVariable {
                    is_mutable: _type.get_specifier(HIR_CONST),
                    linkage: *linkage,
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(symbol_name),
                        _type,
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
