use cx_hir::ast::{HIR, HIRStmt, global_var::HIRGlobalVariable};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::THIRFunction;

pub mod environment;
pub mod log;
pub mod symbol;

pub(crate) mod requests;

mod globals;
mod type_checking;

use crate::globals::lower_global;
use crate::requests::fulfill_requests;
use crate::symbol::completion::complete_prototype;
use crate::{environment::TypeEnvironment, type_checking::functions::typecheck_function};

pub fn typecheck(env: &mut TypeEnvironment, ast: &HIR) -> CXResult<()> {
    for definition in &ast.definition_stmts {
        let namespace = EnvironmentNamespace::from(definition.namespace.clone());

        match &definition.stmt {
            HIRStmt::FunctionDefinition {
                prototype,
                template_prototype: None,
                body,
                ..
            } => {
                let prototype = complete_prototype(env, &namespace, prototype)?;

                match body {
                    Some(body) => typecheck_function(env, &namespace, prototype, body)?,
                    None => env.items.push_generated_function(THIRFunction {
                        prototype,
                        body: None,
                    }),
                };
            }

            HIRStmt::GlobalVariableDefinition {
                variable:
                    HIRGlobalVariable::Standard {
                        name,
                        _type: hir_type,
                        linkage,
                        symbol_name_scheme: name_scheme,
                        initializer,
                        ..
                    },
                ..
            } => lower_global(
                env,
                &namespace,
                name.clone(),
                hir_type,
                *linkage,
                *name_scheme,
                initializer.as_ref(),
            )?,

            _ => {}
        }
    }

    fulfill_requests(env)?;

    Ok(())
}
