use std::collections::HashMap;
use std::iter;
use std::process::id;
use cranelift::codegen::{ir, Context};
use cranelift::codegen::ir::Fact::Def;
use cranelift::codegen::ir::{Function, GlobalValue};
use cranelift::codegen::isa::TargetFrontendConfig;
use cranelift::prelude::{settings, Configurable, FunctionBuilder, Value};
use cranelift_module::{DataDescription, DataId, FuncId, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use crate::codegen::codegen::{codegen_fn_prototype, codegen_function};
use crate::codegen::routines::string_literal;
use crate::parse::verify::bytecode::ValueID;
use crate::parse::verify::context::{FnMap, FunctionPrototype, TypeMap};
use crate::parse::verify::VerifiedAST;

mod codegen;
mod value_type;
mod routines;
mod instruction;

pub(crate) type VariableTable = HashMap<ValueID, Value>;

pub(crate) struct FunctionState<'a> {
    pub(crate) object_module: &'a mut ObjectModule,
    pub(crate) target_frontend_config: &'a TargetFrontendConfig,

    pub(crate) function_ids: &'a HashMap<String, FuncId>,
    pub(crate) global_strs: &'a Vec<DataId>,

    pub(crate) type_map: &'a TypeMap,
    pub(crate) fn_map: &'a FnMap,

    pub(crate) builder: FunctionBuilder<'a>,

    pub(crate) fn_params: Vec<Value>,

    pub(crate) function_prototype: &'a FunctionPrototype,
    pub(crate) variable_table: VariableTable,

    pub(crate) pointer_type: ir::Type,

    pub(crate) current_block_exited: bool,
}

pub(crate) struct GlobalState<'a> {
    pub(crate) context: Context,
    pub(crate) object_module: ObjectModule,
    pub(crate) target_frontend_config: TargetFrontendConfig,

    pub(crate) fn_map: &'a FnMap,
    pub(crate) type_map: &'a TypeMap,

    pub(crate) global_strs: Vec<DataId>,

    pub(crate) function_ids: HashMap<String, FuncId>,
    pub(crate) function_sigs: &'a mut HashMap<String, ir::Signature>,
}

pub fn ast_codegen(ast: &VerifiedAST, output: &str) -> Option<()> {
    let settings_builder = settings::builder();
    let flags = settings::Flags::new(settings_builder);

    let native_builder = cranelift_native::builder().unwrap();
    let isa = native_builder.finish(flags).unwrap();

    let mut data_description = DataDescription::new();

    let mut global_state = GlobalState {
        object_module: ObjectModule::new(
            ObjectBuilder::new(
                isa.clone(),
                output,
                cranelift_module::default_libcall_names(),
            ).unwrap()
        ),

        fn_map: &ast.fn_map,
        type_map: &ast.type_map,

        context: Context::new(),
        target_frontend_config: isa.frontend_config(),
        global_strs: Vec::new(),
        function_ids: HashMap::new(),
        function_sigs: &mut HashMap::new(),
    };

    for global_str in ast.global_strs.iter() {
        let global_val = string_literal(&mut global_state.object_module, global_str);

        println!("Global string: {:?}", global_val);

        global_state.global_strs.push(global_val);
    }

    for (_, fn_prototype) in &ast.fn_map {
        codegen_fn_prototype(&mut global_state, fn_prototype);
    }

    for func in &ast.fn_defs {
        let func_id = global_state.function_ids.get(&func.prototype.name).cloned().unwrap();
        let func_sig = global_state.function_sigs.remove(&func.prototype.name).unwrap_or_else(|| {
            panic!("Function signature redefine: {}", func.prototype.name);
        });

        codegen_function(&mut global_state, func_id, func_sig, func)?;
    }

    println!("Outputting to {}", output);

    let obj = global_state.object_module.finish();
    std::fs::write(output, obj.emit().unwrap()).expect("Failed to write object file");

    println!("Successfully generated object file to {}", output);

    Some(())
}