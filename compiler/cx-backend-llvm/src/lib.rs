use crate::attributes::*;
use crate::mangling::string_literal_name;
use crate::typing::{any_to_basic_type, cx_llvm_prototype, cx_llvm_type};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionMap, BCFunctionPrototype, BCTypeMap, BytecodeFunction, ElementID, ProgramBytecode, ValueID};
use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassBuilderOptions, PassManagerSubType};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyType, FunctionType};
use inkwell::values::{AnyValue, AnyValueEnum, AsValueRef, BasicValue};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;
use cx_util::format::dump_data;

pub(crate) mod typing;
mod instruction;
mod mangling;
mod attributes;
mod arithmetic;

pub(crate) struct GlobalState<'a> {
    module: Module<'a>,
    context: &'a Context,

    functions: HashMap<String, FunctionType<'a>>,

    type_map: &'a BCTypeMap,
    function_map: &'a BCFunctionMap,
}

pub(crate) struct FunctionState<'a> {
    current_function: String,
    
    builder: Builder<'a>,
    value_map: HashMap<ValueID, CodegenValue<'a>>,
}

impl<'a> FunctionState<'a> {
    pub(crate) fn get_val_ref(&self, id: &ValueID) -> Option<&CodegenValue<'a>> {
        self.value_map.get(id)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CodegenValue<'a> {
    Value(AnyValueEnum<'a>),
    FunctionRef(String),
    NULL
}

impl<'a> CodegenValue<'a> {
    pub fn get_value(&self) -> AnyValueEnum<'a> {
        match self {
            CodegenValue::Value(value) => value.clone(),

            _ => panic!("Expected a value, found: {:?}", self)
        }
    }

    pub fn get_function_ref(&self) -> &str {
        match self {
            CodegenValue::FunctionRef(name) => name,

            _ => panic!("Expected a function reference, found: {:?}", self)
        }
    }
}

pub fn bytecode_aot_codegen(
    bytecode: &ProgramBytecode,
    output_path: &str
) -> Option<()> {
    let context = Context::create();
    Target::initialize_native(&InitializationConfig::default()).expect(
        "Failed to initialize native"
    );

    let mut global_state = GlobalState {
        module: context.create_module(output_path),
        context: &context,

        functions: HashMap::new(),

        type_map: &bytecode.type_map,
        function_map: &bytecode.fn_map,
    };

    for _type in global_state.type_map.values() {
        cache_type(&mut global_state, _type).unwrap();
    }

    for prototypes in global_state.function_map.values() {
        cache_prototype(&mut global_state, prototypes).unwrap();
    }

    for (i, str) in bytecode.global_strs.iter().enumerate() {
        let val =
            context
                .const_string(str.as_bytes(), true)
                .as_basic_value_enum();

        let global = global_state.module.add_global(
            val.get_type(),
            None,
            string_literal_name(i).as_str()
        );

        global.set_linkage(Linkage::Private);
        global.set_initializer(&val);
        global.set_unnamed_addr(true);
        global.set_constant(true);
    }

    for func in bytecode.fn_defs.iter() {
        fn_aot_codegen(func, &global_state)
            .expect(format!("Failed to generate function code for function: {}", func.prototype.name).as_str());
    }

    let target = Target::from_triple(
        &TargetMachine::get_default_triple()
    ).expect("Failed to get target from triple");

    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default
        )
        .expect("Failed to create target machine");
    
    global_state.module.verify().unwrap_or_else(|err| panic!("Module verification failed with error: {:#?}", err));
    global_state.module.set_triple(&TargetMachine::get_default_triple());
    
    // println!("{}", global_state.module.print_to_string().to_string_lossy());
    
    // global_state.module
    //     .run_passes(
    //         "default<O1>",
    //         &target_machine,
    //         PassBuilderOptions::create()
    //     )
    //     .expect("Failed to run passes");
    
    let output = global_state.module.print_to_string();
    dump_data(&output.to_string_lossy());
    
    let asm = target_machine
        .write_to_memory_buffer(
            &global_state.module,
            inkwell::targets::FileType::Assembly,
        )
        .expect("Failed to write module to memory buffer");
    
    dump_data(&String::from_utf8_lossy(asm.as_slice()));

    target_machine
        .write_to_file(&global_state.module, inkwell::targets::FileType::Object, Path::new(output_path))
        .expect("Failed to add analysis passes");

    Some(())
}

fn fn_aot_codegen(
    bytecode: &BytecodeFunction,
    global_state: &GlobalState
) -> Option<()> {
    let func_val = global_state
        .module
        .get_function(bytecode.prototype.name.as_str())
        .expect("Failed to get function from module");
    let builder = global_state.context.create_builder();

    let mut function_state = FunctionState {
        current_function: bytecode.prototype.name.clone(),
        
        builder,
        value_map: HashMap::new(),
    };

    for i in 0..bytecode.blocks.len() {
        global_state.context.append_basic_block(func_val, format!("block_{}", i).as_str());
    }

    for (block_id, block) in bytecode.blocks.iter().enumerate() {
        let block_val = func_val.get_basic_blocks().get(block_id).unwrap().clone();
        function_state.builder.position_at_end(block_val);

        for (value_id, inst) in block.body.iter().enumerate() {
            let value = instruction::generate_instruction(
                &global_state,
                &function_state,
                &func_val,
                inst
            ).expect(format!(
                "Failed to generate instruction {}", inst
            ).as_str());

            function_state.value_map.insert(
                ValueID { block_id: block_id as ElementID, value_id: value_id as ElementID },
                value
            );
            
            if inst.instruction.is_block_terminating() {
                break;
            }
        }
    }

    Some(())
}

fn cache_type<'a>(
    global_state: &GlobalState<'a>,
    _type: &BCType
) -> Option<()> {
    let BCTypeKind::Struct { fields, name } = &_type.kind else {
        return Some(());
    };

    let fields = fields
        .iter()
        .map(|(_, field_type)| {
            let type_ = cx_llvm_type(global_state, field_type)?;

            any_to_basic_type(type_)
        })
        .collect::<Option<Vec<_>>>()?;
    
    let struct_type = cx_llvm_type(global_state, _type)?.into_struct_type();
    struct_type.set_body(&fields, false);
    struct_type.as_any_type_enum();

    Some(())
}

fn cache_prototype<'a>(
    global_state: &mut GlobalState<'a>,
    prototype: &'a BCFunctionPrototype,
) -> Option<()> {
    let llvm_prototype = cx_llvm_prototype(
        global_state,
        prototype
    ).unwrap();

    let func = global_state.module.add_function(
        prototype.name.as_str(),
        llvm_prototype,
        None
    );

    for i in 0..prototype.params.len() {
        func.add_attribute(
            AttributeLoc::Param(i as u32),
            noundef(global_state.context)
        )
    }

    global_state.functions.insert(
        prototype.name.to_string(),
        func.get_type()
    );

    Some(())
}