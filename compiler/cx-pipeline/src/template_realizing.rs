use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    type_checking::{realize_deconstructor, realize_fn_implementation},
};
use cx_util::CXResult;
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
                    Some(module) => CompilationUnit::from_str(module.as_str()),
                    None => job.clone(),
                };

                if !requests_fulfilled.insert((kind.clone(), input.clone())) {
                    continue;
                }
                
                realize_fn_implementation(env, &origin, &kind, &input)?;
            },
            
            MIRFunctionGenRequest::Deconstruction { _type } => {
                realize_deconstructor(env, job, _type)?;
            },
        }
    }

    Ok(())
}
