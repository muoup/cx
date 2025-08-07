use cx_compiler_typechecker::template_fn_typecheck;
use cx_data_pipeline::{CompilationUnit, GlobalCompilationContext};
use std::sync::Arc;

pub(crate) fn realize_templates(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit
) -> Option<()> {
    let ast = context.module_db.typechecked_ast
        .get(unit);

    let mut typecheck_data = context.module_db
        .typecheck_data
        .take(&unit);

    let mut new_methods = Vec::new();

    while !typecheck_data.requests.is_empty() {
        let take = std::mem::take(&mut typecheck_data.requests);
        let requests = take.into_iter()
            .map(|request| match ast.function_map.template_source(&request.template_name) {
                Some(source) => (source, request),
                None => (unit.identifier().to_string(), request)
            })
            .collect::<Vec<_>>();

        for (source, request) in requests {
            let other_ast = context.module_db
                .typechecked_ast
                .get(&CompilationUnit::from_str(&source));
            let lex = context.module_db
                .lex_tokens
                .get(&CompilationUnit::from_str(&source));

            let (data, prototype, stmt) = template_fn_typecheck(&lex, &other_ast, request)?;
            
            new_methods.push((prototype, stmt));
            typecheck_data.extend(data);
        }
    }

    drop(ast);

    let (mut lock, mut ast) = context.module_db.typechecked_ast
        .take_lock(unit);

    for (prototype, stmt) in new_methods {
        ast.global_stmts.push(stmt);
        ast.function_map.insert(prototype.name.to_string(), prototype);
    }
    
    lock.insert(unit.clone(), Arc::from(ast));

    context.module_db
        .typecheck_data
        .insert(unit.clone(), typecheck_data);
    Some(())
}