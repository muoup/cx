use cx_compiler_typechecker_new::environment::TCEnvironment;
use cx_compiler_typechecker_new::realize_fn_prototype;
use cx_data_pipeline::{CompilationUnit, GlobalCompilationContext};
use cx_data_typechecker::ast::TCAST;
use cx_util::mangling::mangle_template;

pub(crate) fn realize_templates(
    context: &GlobalCompilationContext,
    job: &CompilationUnit,
    env: &mut TCEnvironment,
    tc_ast: &mut TCAST,
) -> Option<()> {
    let mut new_methods = Vec::new();
    
    while let Some(request) = env.requests.pop() {
        let origin = match &request.module_origin {
            Some(module) => CompilationUnit::from_str(module.as_str()),
            None => job.clone(),
        };

        let other_ast = context.module_db.naive_ast.get(&origin);
        let other_data = context.module_db.structure_data.get(&origin);
        
        let manged_name = mangle_template(&request.name, &request.input.args);
        let Some(existing) = other_data.fn_data.get(&manged_name) else {
            unreachable!("Instantiated function template not found in typechecked AST: {}", manged_name);
        };

        let stmt = realize_fn_prototype(env, other_ast.as_ref(), &request.input, existing)?;

        new_methods.push(stmt);
    }

    tc_ast.function_defs.extend(new_methods);
    Some(())
}