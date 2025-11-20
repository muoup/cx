use std::collections::HashMap;

use cranelift::{codegen::Context, prelude::settings};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cx_bytecode_data::compilation_unit::BCUnit;
use cx_util::log_error;

use crate::{GlobalState, bc_codegen::{instruction::generate_function, types::codegen_fn_prototype}};

mod types;
mod instruction;
mod context;

pub fn bytecode_codegen(bc: &BCUnit, output: &str) -> Option<Vec<u8>> {
    let settings_builder = settings::builder();
    let flags = settings::Flags::new(settings_builder);

    let native_builder = cranelift_native::builder().unwrap();
    let isa = native_builder.finish(flags).unwrap();

    let mut global_state = GlobalState {
        object_module: ObjectModule::new(
            ObjectBuilder::new(
                isa.clone(),
                output,
                cranelift_module::default_libcall_names(),
            )
            .unwrap(),
        ),

        context: Context::new(),
        target_frontend_config: isa.frontend_config(),
        function_ids: HashMap::new(),
        function_sigs: &mut HashMap::new(),
    };

    // for global_var in bc.global_vars.iter() {
    //     generate_global(&mut global_state, global_var)?;
    // }

    for fn_prototype in bc.function_prototypes.iter() {
        codegen_fn_prototype(&mut global_state, fn_prototype);
    }

    for func in bc.functions.iter() {
        let Some(func_id) = global_state.function_ids.get(&func.prototype.name).cloned() else {
            log_error!(
                "Function not found in function map: {}",
                func.prototype.name
            );
        };

        let func_sig = global_state
            .function_sigs
            .remove(&func.prototype.name)
            .unwrap_or_else(|| {
                panic!("Function prototype not found for: {}", func.prototype.name);
            });

        generate_function(&mut global_state, func_id, func_sig, func)?;
    }

    global_state.object_module.finish().emit().ok()
}
