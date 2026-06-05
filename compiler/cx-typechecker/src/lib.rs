use cx_ast::ast::modifiers::{CX_CONST, CXLinkageMode};
use cx_ast::decomposition::{CXGenerationAST, CXGenerationStmt};
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::global::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::CXResult;

pub mod environment;
pub mod log;
pub(crate) mod requests;
pub mod symbol;
mod type_checking;

use crate::requests::fulfill_requests;
use crate::symbol::completion::{complete_prototype, complete_type};
use crate::type_checking::constexpr::constexpr_evaluate;
use crate::type_checking::typechecker::typecheck_expr;
use crate::{environment::TypeEnvironment, type_checking::functions::typecheck_function};

pub fn typecheck(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ast: &CXGenerationAST,
) -> CXResult<()> {
    for stmt in ast.generation_stmts.iter() {
        match stmt {
            CXGenerationStmt::Function { prototype, body } => {
                let prototype = complete_prototype(&mut env.symbols, namespace, prototype)?;
                typecheck_function(env, namespace, prototype.clone(), body)?;
            }

            CXGenerationStmt::StringLiteral { name, value } => {
                let global = MIRGlobalVariable {
                    is_mutable: false,
                    linkage: CXLinkageMode::Static,
                    kind: MIRGlobalVarKind::StringLiteral {
                        name: name.clone(),
                        value: value.clone(),
                    },
                };

                env.items.push_generated_global(global);
            }

            CXGenerationStmt::AddressableGlobal {
                name,
                _type,
                linkage,
                initializer,
            } => {
                let _type = complete_type(&mut env.symbols, namespace, _type)?;
                let constexpr_init = initializer
                    .as_ref()
                    .map(|init| {
                        typecheck_expr(env, namespace, &init, Some(&_type))
                            .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))
                            .and_then(|tc| constexpr_evaluate(env, tc))
                    })
                    .transpose()?
                    .map(|ce| {
                        ce.get_integer().ok_or_else(|| {
                            unreachable!(
                                "Global variable initializer must be a constant integer expression"
                            )
                        })
                    })
                    .transpose()?;

                let global = MIRGlobalVariable {
                    is_mutable: _type.get_specifier(CX_CONST),
                    linkage: *linkage,
                    kind: MIRGlobalVarKind::Variable {
                        name: name.clone(),
                        _type: _type,
                        initializer: constexpr_init,
                    },
                };

                env.items.push_generated_global(global);
            }
        }
    }

    fulfill_requests(env, namespace)?;

    Ok(())
}
