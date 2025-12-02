use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{environment::TypeEnvironment, type_checking::realize_fn_implementation};
use cx_util::CXResult;
use std::collections::HashSet;

pub(crate) fn realize_templates(
    job: &CompilationUnit,
    env: &mut TypeEnvironment,
) -> CXResult<()> {
    let mut requests_fulfilled = HashSet::new();

    while let Some(request) = env.requests.pop() {
        let origin = match &request.module_origin {
            Some(module) => CompilationUnit::from_str(module.as_str()),
            None => job.clone(),
        };

        if !requests_fulfilled.insert((request.name.clone(), request.input.clone())) {
            continue;
        }
        
        realize_fn_implementation(
            env,
            &origin,
            &request.name,
            &request.input,
        )?;
    }

    Ok(())
}
