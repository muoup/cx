use crate::attributes::*;
use crate::typing::{any_to_basic_type, bc_llvm_prototype, bc_llvm_type, convert_linkage};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionMap, BCFunctionPrototype, BlockID, BytecodeFunction, ElementID, FunctionBlock, MIRValue, ProgramBytecode};
use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassBuilderOptions, PassManagerSubType};
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{AnyType, FunctionType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, FunctionValue, GlobalValue};

use crate::globals::generate_global_variable;
use crate::instruction::{inst_num, reset_num};
use cx_data_pipeline::OptimizationLevel;
use cx_util::format::dump_data;
use inkwell::basic_block::BasicBlock;
use std::collections::HashMap;

pub(crate) mod typing;
mod instruction;
mod attributes;
mod arithmetic;
mod routines;
mod globals;

pub(crate) struct GlobalState<'a> {
    module: Module<'a>,
    context: &'a Context,

    globals: Vec<GlobalValue<'a>>,

    functions: HashMap<String, FunctionType<'a>>,
    function_map: &'a BCFunctionMap,
}

pub(crate) struct FunctionState<'a> {
    context: &'a Context,

    current_function: String,
    
    defer_block_offset: usize,
    in_defer: bool,

    builder: Builder<'a>,
    value_map: HashMap<MIRValue, CodegenValue<'a>>,
}

impl<'a> FunctionState<'a> {
    pub(crate) fn get_value(&self, id: &MIRValue) -> Option<CodegenValue<'a>> {
        match id {
            MIRValue::IntImmediate { val, type_ } => {
                let int_type = bc_llvm_type(self.context, type_)?;
                let int_val = int_type.into_int_type().const_int(*val as u64, true).as_any_value_enum();

                Some(CodegenValue::Value(int_val))
            },
            MIRValue::FloatImmediate { val, type_ } => {
                let float_type = bc_llvm_type(self.context, type_)?;
                let float_val = float_type.into_float_type()
                    .const_float(f64::from_bits(*val as u64))
                    .as_any_value_enum();

                Some(CodegenValue::Value(float_val))
            },

            MIRValue::LoadOf(_type, val) => {
                let ptr_val = self.get_value(val)?
                    .get_value();
                let ptr_type = bc_llvm_type(self.context, _type)?;
                let basic_type = any_to_basic_type(ptr_type)?;

                let load_inst = self.builder.build_load(
                        basic_type,
                        ptr_val.into_pointer_value(),
                        &*inst_num()
                    )
                    .ok()?
                    .as_any_value_enum();

                Some(CodegenValue::Value(load_inst))
            },

            MIRValue::FunctionRef(_) => panic!("Function references should be handled at a higher level"),

            MIRValue::BlockResult { .. } |
            MIRValue::Global(..) => self.value_map.get(id).cloned(),

            MIRValue::NULL => Some(CodegenValue::NULL),
        }
    }
    
    pub(crate) fn get_block(&self, function_val: &FunctionValue<'a>, block_id: BlockID) -> Option<BasicBlock> {
        match block_id {
            BlockID::Block(id) =>
                function_val.get_basic_blocks()
                    .get(id as usize)
                    .cloned(),
            BlockID::DeferredBlock(id) =>
                function_val.get_basic_blocks()
                    .get(id as usize + self.defer_block_offset)
                    .cloned(),
            _ => None
        }
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
            CodegenValue::Value(value) => *value,

            _ => panic!("Expected a value, found: {self:?}")
        }
    }

    pub fn get_function_ref(&self) -> &str {
        match self {
            CodegenValue::FunctionRef(name) => name,

            _ => panic!("Expected a function reference, found: {self:?}")
        }
    }
}

pub fn bytecode_aot_codegen(
    bytecode: &ProgramBytecode,
    output_path: &str,
    optimization_level: OptimizationLevel,
) -> Option<Vec<u8>> {
    let context = Context::create();
    Target::initialize_native(&InitializationConfig::default()).expect(
        "Failed to initialize native"
    );

    let mut global_state = GlobalState {
        module: context.create_module(output_path),
        context: &context,

        globals: Vec::new(),
        functions: HashMap::new(),

        function_map: &bytecode.fn_map,
    };

    for prototypes in global_state.function_map.values() {
        cache_prototype(&mut global_state, prototypes).unwrap();
    }

    for global in bytecode.global_vars.iter() {
        generate_global_variable(&mut global_state, global)
            .unwrap_or_else(|| panic!("Failed to generate global variable: {}", global.name));
    }

    for func in bytecode.fn_defs.iter() {
        fn_aot_codegen(func, &global_state)
            .unwrap_or_else(|| panic!("Failed to generate function code for function: {}", func.prototype.name));
    }

    let target = Target::from_triple(
        &TargetMachine::get_default_triple()
    ).expect("Failed to get target from triple");

    let (pass_manager_str, inkwell_optimization_level) = match optimization_level {
        OptimizationLevel::O0 => ("default<O0>", inkwell::OptimizationLevel::None),
        OptimizationLevel::O1 => ("default<O1>", inkwell::OptimizationLevel::Less),
        OptimizationLevel::O2 => ("default<O2>", inkwell::OptimizationLevel::Default),
        OptimizationLevel::O3 => ("default<O3>", inkwell::OptimizationLevel::Aggressive),
        OptimizationLevel::Osize => ("default<Os>", inkwell::OptimizationLevel::Default),
        OptimizationLevel::Ofast => ("default<O3>", inkwell::OptimizationLevel::Aggressive),
    };

    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "generic",
            "",
            inkwell_optimization_level,
            RelocMode::PIC,
            CodeModel::Default
        )
        .expect("Failed to create target machine");

    global_state.module.verify()
        .unwrap_or_else(|err| {
            dump_data(&global_state.module.print_to_string().to_string_lossy());
            panic!("Module verification failed: {}", err.to_string());
        });
    global_state.module.set_triple(&TargetMachine::get_default_triple());
    
    global_state.module
        .run_passes(
            pass_manager_str,
            &target_machine,
            PassBuilderOptions::create()
        )
        .expect("Failed to run passes");

    dump_data(&format!("{}", global_state.module.print_to_string().to_string_lossy()));

    let buff = target_machine
        .write_to_memory_buffer(&global_state.module, inkwell::targets::FileType::Object)
        .expect("Failed to export module to file");

    Some(buff.as_slice().to_vec())
}

fn fn_aot_codegen(
    bytecode: &BytecodeFunction,
    global_state: &GlobalState
) -> Option<()> {
    reset_num();
    
    let func_val = global_state
        .module
        .get_function(bytecode.prototype.name.as_str())
        .unwrap_or_else(|| panic!("Failed to get function from module: {}", bytecode.prototype.name));
    let builder = global_state.context.create_builder();

    let mut function_state = FunctionState {
        context: global_state.context,
        current_function: bytecode.prototype.name.clone(),
        
        defer_block_offset: bytecode.blocks.len(),
        in_defer: false,

        builder,
        value_map: HashMap::new(),
    };

    for (i, global) in global_state.globals.iter().enumerate() {
        function_state.value_map.insert(
            MIRValue::Global(i as ElementID),
            CodegenValue::Value(global.as_any_value_enum())
        );
    }

    for i in 0..bytecode.blocks.len() {
        global_state.context.append_basic_block(func_val, format!("block_{i}").as_str());
    }
    
    for i in 0..bytecode.defer_blocks.len() {
        global_state.context.append_basic_block(func_val, format!("defer_block_{i}").as_str());
    }

    function_state.in_defer = false;
    
    for (block_id, block) in bytecode.blocks.iter().enumerate() {
        codegen_block(global_state, &mut function_state, &func_val, BlockID::Block(block_id as ElementID), block);
    }

    function_state.in_defer = true;
    
    for (block_id, block) in bytecode.defer_blocks.iter().enumerate() {
        codegen_block(global_state, &mut function_state, &func_val, BlockID::DeferredBlock(block_id as ElementID), block);
    }

    Some(())
}

fn codegen_block<'a>(
    global_state: &GlobalState<'a>,
    function_state: &mut FunctionState<'a>,
    func_val: &FunctionValue<'a>,
    block_id: BlockID,
    block: &FunctionBlock
) {
    let block_val = function_state.get_block(func_val, block_id)
        .unwrap_or_else(|| panic!("Block with ID {block_id} not found in function"));
    function_state.builder.position_at_end(block_val);

    for (value_id, inst) in block.body.iter().enumerate() {
        println!("Generating instruction: {inst}");

        let Some(value) = instruction::generate_instruction(
            global_state,
            function_state,
            func_val,
            inst
        ) else {
            panic!("Failed to generate instruction: {inst} in function: {}", function_state.current_function);
        };

        function_state.value_map.insert(
            MIRValue::BlockResult {
                block_id, 
                value_id: value_id as ElementID 
            },
            value
        );
        
        if inst.instruction.is_block_terminating() {
            break;
        }
    }
}

fn cache_type<'a>(
    global_state: &GlobalState<'a>,
    _type: &BCType
) -> Option<()> {
    let BCTypeKind::Struct { fields, .. } = &_type.kind else {
        return Some(());
    };

    let fields = fields
        .iter()
        .map(|(_, field_type)| {
            let type_ = bc_llvm_type(global_state.context, field_type)?;

            any_to_basic_type(type_)
        })
        .collect::<Option<Vec<_>>>()?;
    
    let struct_type = bc_llvm_type(global_state.context, _type)?.into_struct_type();
    struct_type.set_body(&fields, false);
    struct_type.as_any_type_enum();

    Some(())
}

fn cache_prototype<'a>(
    global_state: &mut GlobalState<'a>,
    prototype: &'a BCFunctionPrototype,
) -> Option<()> {
    let llvm_prototype = bc_llvm_prototype(
        global_state,
        prototype
    ).unwrap();

    let func = global_state.module.add_function(
        prototype.name.as_str(),
        llvm_prototype,
        Some(convert_linkage(prototype.linkage))
    );

    // get_type_attributes(global_state.context, &prototype.return_type)
    //     .into_iter()
    //     .for_each(|attr| {
    //         func.add_attribute(AttributeLoc::Return, attr);
    //     });

    for (i, _type) in prototype.params.iter().enumerate() {
        get_type_attributes(global_state.context, &_type._type)
            .into_iter()
            .for_each(|attr| {
                func.add_attribute(AttributeLoc::Param(i as u32), attr);
            });
    }

    global_state.functions.insert(
        prototype.name.to_string(),
        func.get_type()
    );

    Some(())
}