use cx_parsing_data::data::NaiveFnIdent;
use cx_pipeline_data::{CompilationUnit, GlobalCompilationContext};
use cx_typechecker::{environment::TCEnvironment, type_checking::realize_fn_implementation};
use cx_typechecker_data::ast::TCBaseMappings;
use std::collections::HashSet;

pub(crate) fn realize_templates(
    context: &GlobalCompilationContext,
    base_data: &TCBaseMappings,
    job: &CompilationUnit,
    env: &mut TCEnvironment,
) -> Option<()> {
    let mut requests_fulfilled = HashSet::new();

    while let Some(request) = env.requests.pop() {
        let origin = match &request.module_origin {
            Some(module) => CompilationUnit::from_str(module.as_str()),
            None => job.clone(),
        };

        if !requests_fulfilled.insert((request.name.clone(), request.input.clone())) {
            continue;
        }

        let other_ast = context.module_db.naive_ast.get(&origin);
        let other_data = context.module_db.base_mappings.get(&origin);
        
        let template = base_data
            .fn_data 
            .get_template(&NaiveFnIdent::from(&request.name))
            .unwrap()
            .resource
            .clone();
        
        realize_fn_implementation(
            env,
            other_data.as_ref(),
            other_ast.as_ref(),
            &template,
            &request.input,
        ).unwrap();
    }

    Some(())
}
