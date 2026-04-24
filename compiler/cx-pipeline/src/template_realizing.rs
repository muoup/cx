use cx_ast::data::CXFunctionKind;
use cx_mir::mir::data::MIRTemplateInput;
use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    realize_fn_implementation,
};
use cx_util::CXResult;

pub(crate) fn realize_templates(job: &CompilationUnit, env: &mut TypeEnvironment) -> CXResult<()> {
    let mut requests_fulfilled = Vec::new();

    while let Some(request) = env.pop_function_generation_request() {
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

                if request_was_fulfilled(env, &requests_fulfilled, &kind, &input) {
                    continue;
                }

                requests_fulfilled.push((kind.clone(), input.clone()));
                realize_fn_implementation(env, &origin, &kind, &input)?;
            }
        }
    }

    Ok(())
}

fn request_was_fulfilled(
    env: &TypeEnvironment,
    requests_fulfilled: &[(CXFunctionKind, MIRTemplateInput)],
    kind: &CXFunctionKind,
    input: &MIRTemplateInput,
) -> bool {
    requests_fulfilled
        .iter()
        .any(|(existing_kind, existing_input)| {
            existing_kind == kind && existing_input.contextual_eq(input, &env.symbols.context)
        })
}
