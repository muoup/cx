use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    realize_fn_implementation,
};
use cx_util::{CXError, CXResult};
use std::collections::HashSet;

pub(crate) fn realize_templates(job: &CompilationUnit, env: &mut TypeEnvironment) -> CXResult<()> {
    let mut requests_fulfilled = HashSet::new();

    while let Some(request) = env.requests.pop() {
        match request {
            MIRFunctionGenRequest::Template {
                module_origin,
                kind,
                input,
            } => {
                let origin = match &module_origin {
                    Some(module) => env.resolve_compilation_unit(module.as_str()),
                    None => job.clone(),
                };

                if !requests_fulfilled.insert((kind.clone(), input.clone())) {
                    continue;
                }

                realize_fn_implementation(env, &origin, &kind, &input)
                    .map_err(
                        |e| CXError::create_boxed(format!("Error realizing template {} with input {:?}:\n{}", kind, input, e.error_message()))
                    )?;
            }
        }
    }

    Ok(())
}
