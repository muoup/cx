use crate::attributes::*;
use crate::mangling::string_literal_name;
use crate::typing::{any_to_basic_type, create_fn_proto, cx_llvm_prototype, cx_llvm_type};
use cx_data_ast::parse::ast::{CXFunctionPrototype, FunctionMap, TypeMap};
use cx_data_ast::parse::value_type::{CXTypeUnion, CXValType};
use cx_data_bytecode::builder::{BytecodeFunction, ElementID, ValueID};
use cx_data_bytecode::ProgramBytecode;
use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{InitializationConfig, Target};
use inkwell::types::{AnyType, AnyTypeEnum, AsTypeRef, BasicType, FunctionType};
use inkwell::values::{AnyValue, AnyValueEnum, AsValueRef, BasicValue};
use std::collections::HashMap;

pub(crate) mod typing;
mod instruction;
mod mangling;
mod attributes;

pub(crate) struct GlobalState<'a> {
    module: Module<'a>,
    context: &'a Context,

    functions: HashMap<String, FunctionType<'a>>,

    type_map: &'a TypeMap,
    function_map: &'a FunctionMap,
}

pub(crate) struct FunctionState<'a> {
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

    for (name, _type) in global_state.type_map.iter() {
        cache_type(&mut global_state, name, _type).unwrap();
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
        fn_aot_codegen(func, &global_state)?;
    }

    let output = global_state.module.print_to_string();

    println!(
        "LLVM IR:\n{}\n",
        output.to_string_lossy()
    );

    None
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
            )?;

            function_state.value_map.insert(
                ValueID { block_id: block_id as ElementID, value_id: value_id as ElementID },
                value
            );
        }
    }

    Some(())
}

fn cache_type<'a>(
    global_state: &GlobalState<'a>,
    name: &str,
    _type: &CXValType
) -> Option<()> {
    let CXTypeUnion::Structured { fields, .. } = &_type.internal_type else {
        return Some(());
    };

    let fields = fields
        .iter()
        .map(|(_, _type)| {
            let llvm_type = cx_llvm_type(global_state, _type)?;
            any_to_basic_type(llvm_type)
        })
        .collect::<Option<Vec<_>>>()
        .unwrap();

    let opaque_type = global_state.module.get_struct_type(name)
        .unwrap_or(global_state.context.opaque_struct_type(name));

    opaque_type.set_body(&fields, false);

    Some(())
}

fn cache_prototype<'a>(
    global_state: &mut GlobalState<'a>,
    prototype: &'a CXFunctionPrototype
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

    for i in 0..prototype.parameters.len() {
        func.add_attribute(
            AttributeLoc::Param(i as u32),
            noundef(global_state.context)
        )
    }

    global_state.functions.insert(
        prototype.name.to_owned(),
        func.get_type()
    );

    Some(())
}