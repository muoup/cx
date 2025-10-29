use cx_parsing_data::ast::VisibilityMode;
use cx_pipeline_data::{CompilationUnit, GlobalCompilationContext};
use cx_typechecker_data::ast::TCBaseMappings;
use cx_util::CXResult;

mod log;

pub mod type_checking;
pub mod type_completion;

pub mod environment;

pub fn gather_interface(context: &GlobalCompilationContext, unit: &CompilationUnit) -> CXResult<()> {
    let ast = context
        .module_db
        .naive_ast
        .get(unit);
    let mut base_type_map = ast.type_data.clone();
    let mut base_fn_map = ast.function_data.clone();
    let mut base_globals = ast.global_variables.clone();

    for import in ast.imports.iter() {
        let unit = CompilationUnit::from_str(import.as_str());
        let ast = context.module_db.naive_ast.get(&unit);

        for (type_name, cx_type) in ast.type_data.standard_iter() {
            if cx_type.visibility != VisibilityMode::Public {
                continue;
            };

            base_type_map.insert_standard(type_name.clone(), cx_type.transfer(import));
        }

        for (fn_name, cx_fn) in ast.function_data.standard_iter() {
            if cx_fn.visibility != VisibilityMode::Public {
                continue;
            };

            base_fn_map.insert_standard(fn_name.clone(), cx_fn.transfer(import));
        }

        for (type_template_name, type_template) in ast.type_data.template_iter() {
            if type_template.visibility != VisibilityMode::Public {
                continue;
            };

            base_type_map
                .insert_template(type_template_name.clone(), type_template.transfer(import));
        }

        for (fn_template_name, fn_template) in ast.function_data.template_iter() {
            if fn_template.visibility != VisibilityMode::Public {
                continue;
            };

            base_fn_map.insert_template(fn_template_name.clone(), fn_template.transfer(import));
        }
        
        for (global_name, global_var) in ast.global_variables.iter() {
            if global_var.visibility != VisibilityMode::Public {
                continue;
            };
            
            base_globals.insert(global_name.clone(), global_var.transfer(import));
        }
    }

    context
        .module_db
        .base_mappings
        .insert(unit.clone(), TCBaseMappings {
            unit: unit.as_str().to_owned(),
            type_data: base_type_map,
            fn_data: base_fn_map,
            global_variables: base_globals,
        });

    Some(())
}
