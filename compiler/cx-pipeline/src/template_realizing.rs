use cx_data_pipeline::{CompilationUnit, GlobalCompilationContext};
use std::sync::Arc;
use cx_compiler_typechecker_new::realize_fn_prototype;
use cx_util::mangling::mangle_template;

pub(crate) fn realize_templates(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit
) -> Option<()> {
    let ast = context.module_db.naive_ast
        .get(unit);
    let tc_ast = context.module_db.typechecked_ast
        .get(unit);
    
    let mut new_methods = Vec::new();
    
    for template in tc_ast.fn_map.templates.values() {
        let (other_lex, other_ast) = match &template.template.external_module {
            Some(module) => {
                let module = CompilationUnit::from_str(module.as_str());

                (context.module_db.naive_ast.get(&module),
                 context.module_db.typechecked_ast.get(&module))
            },
            None => (ast.clone(), tc_ast.clone())
        };
        
        for input in template.instantiated.iter() {
            let manged_name = mangle_template(template.template.resource.name.as_str(), &input.args);
            let Some(existing) = tc_ast.fn_map.get(&manged_name) else {
                unreachable!("Instantiated function template not found in typechecked AST: {}", manged_name);
            };
            
            let stmt = realize_fn_prototype((&other_lex, &other_ast), input, existing)?;
            
            new_methods.push(stmt);
        }
    }

    drop(ast);
    drop(tc_ast);

    let (mut lock, mut ast) = context.module_db.typechecked_ast
        .take_lock(unit);

    ast.function_defs.extend(new_methods);
    
    lock.insert(unit.clone(), Arc::from(ast));

    Some(())
}