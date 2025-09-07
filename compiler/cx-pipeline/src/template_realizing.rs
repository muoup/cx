use std::collections::HashSet;
use cx_compiler_typechecker_new::environment::TCEnvironment;
use cx_compiler_typechecker_new::realize_fn_implementation;
use cx_data_pipeline::{CompilationUnit, GlobalCompilationContext};
use cx_data_typechecker::ast::TCFunctionDef;
use cx_util::mangling::mangle_template;

pub(crate) fn realize_templates(
    context: &GlobalCompilationContext,
    job: &CompilationUnit,
    env: &mut TCEnvironment
) -> Option<()> {
    let mut requests_fulfilled = HashSet::new();
    
    while let Some(request) = env.requests.pop() {
        let origin = match &request.module_origin {
            Some(module) => CompilationUnit::from_str(module.as_str()),
            None => job.clone(),
        };
        let mangle_name = mangle_template(request.name.as_str(), &request.input.args);

        if requests_fulfilled.contains(&mangle_name) {
            continue;
        }

        let other_ast = context.module_db.naive_ast.get(&origin);
        let other_data = context.module_db.structure_data.get(&origin);

        let template = env.fn_data.get_template(request.name.as_str())?
            .template
            .resource
            .clone();
        realize_fn_implementation(
            env,
            other_data.as_ref(), other_ast.as_ref(),
            &template, &request.input
        )?;

        requests_fulfilled.insert(mangle_name);
    }

    Some(())
}