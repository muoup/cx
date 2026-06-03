use cx_pipeline_data::CompilationUnit;
use cx_typechecker::environment::TypeEnvironment;
use cx_util::CXResult;

pub fn fulfill_requests(_unit: &CompilationUnit, env: &mut TypeEnvironment) -> CXResult<()> {
    while env.pop_request().is_some() {}
    Ok(())
}
