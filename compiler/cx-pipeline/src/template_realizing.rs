use cx_ast::data::CXFunctionKind;
use cx_mir::mir::data::MIRTemplateInput;
use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    realize_fn_implementation,
};
use cx_util::CXResult;

struct TemplateRequestReceipt {
    module_origin: Option<String>,
    kind: CXFunctionKind,
    input: MIRTemplateInput
}

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

                if request_was_fulfilled(env, &requests_fulfilled, &module_origin, &kind, &input) {
                    continue;
                }

                requests_fulfilled.push(TemplateRequestReceipt { module_origin: module_origin.clone(), kind: kind.clone(), input: input.clone() });
                realize_fn_implementation(env, &origin, &kind, &input)?;
            }
        }
    }

    Ok(())
}

fn request_was_fulfilled(
    env: &TypeEnvironment,
    requests_fulfilled: &[TemplateRequestReceipt],
    module_origin: &Option<String>,
    kind: &CXFunctionKind,
    input: &MIRTemplateInput,
) -> bool {
    requests_fulfilled
        .iter()
        .any(|receipt| {
            &receipt.module_origin == module_origin
                && &receipt.kind == kind
                && receipt.input.contextual_eq(input, &env.symbols.context)
        })
}
