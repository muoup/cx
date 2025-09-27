use crate::builder::BytecodeBuilder;
use cx_data_typechecker::cx_types::{CXType, CXTypeKind};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionPrototype, BCParameter, BCPtrBinOp, LinkageType, MIRValue, VirtualInstruction};
use cx_util::mangling::{mangle_deconstructor, mangle_destructor};
use crate::aux_routines::get_cx_struct_field_by_index;

const STANDARD_FREE: &str = "__stdfree";
const STANDARD_FREE_ARRAY: &str = "__stdfreearray";
const STANDARD_FREE_ARRAY_NOOP: &str = "__stdfreearray_destructor_noop";

pub(crate) fn deconstructor_prototype(type_: &CXType) -> Option<BCFunctionPrototype> {
    let Some(name) = type_.get_name() else {
        return None;
    };
    let deconstructor_name = mangle_deconstructor(name);

    Some(
        BCFunctionPrototype {
            name: deconstructor_name,
            return_type: BCType::unit(),
            params: vec![BCParameter { name: None, _type: BCType::default_pointer() }],
            var_args: false,
            linkage: LinkageType::ODR
        }
    )
}

fn get_deconstructor(
    builder: &mut BytecodeBuilder,
    type_: &CXType
) -> Option<String> {
    let Some(type_name) = type_.get_name() else {
        return None;
    };
    let deconstructor_name = mangle_destructor(type_name);

    if builder.fn_map.contains_key(&deconstructor_name) {
        Some(deconstructor_name)
    } else {
        None
    }
}

pub fn deconstruct_variable(builder: &mut BytecodeBuilder, var: &MIRValue, _type: &CXType) -> Option<()> {
    match &_type.kind {
        CXTypeKind::Structured { .. } => {
            if let Some(deconstructor) = get_deconstructor(builder, _type) {
                builder.call(&deconstructor, vec![var.clone()])?;
            }
        },

        CXTypeKind::StrongPointer { inner_type, is_array } => {
            let deconstructor = builder.get_deconstructor(inner_type);

            let inner_val = builder.load_value(var.clone(), BCType::default_pointer())?;
            let deconstruct = builder.create_named_block("ptr_not_null");
            let post_deconstruct = builder.create_named_block("ptr_is_null");

            let zero = builder.int_const(0, 8, true);
            let null = builder.add_instruction(
                VirtualInstruction::IntToPtr { value: zero },
                BCType::default_pointer()
            )?;
            let cmp = builder.add_instruction(
                VirtualInstruction::PointerBinOp {
                    op: BCPtrBinOp::EQ,
                    left: inner_val.clone(),
                    right: null,
                    ptr_type: BCType::default_pointer()
                },
                BCType { kind: BCTypeKind::Bool }
            )?;

            builder.add_instruction(
                VirtualInstruction::Branch { condition: cmp, true_block: post_deconstruct, false_block: deconstruct },
                BCType::unit()
            )?;

            builder.set_current_block(deconstruct);

            match (deconstructor, is_array) {
                (Some(prototype), true) => {
                    // Array of objects with deconstructor
                    let deconstructor = builder.fn_ref(&prototype)?;
                    let ptr_to = builder.add_instruction(
                        VirtualInstruction::GetFunctionAddr { func: deconstructor },
                        BCType::default_pointer()
                    )?;
                    let type_size = builder.convert_cx_type(inner_type.as_ref())?
                        .fixed_size();
                    let size_imm = MIRValue::IntImmediate {
                        val: type_size as i64,
                        type_: BCTypeKind::Unsigned { bytes: 8 }.into()
                    };

                    builder.call(STANDARD_FREE_ARRAY, vec![inner_val, size_imm, ptr_to])?;
                },

                (None, true) => {
                    // Array of objects without deconstructor
                    builder.call(STANDARD_FREE_ARRAY_NOOP, vec![inner_val])?;
                },

                (Some(prototype), false) => {
                    // Single object with deconstructor
                    builder.call(&prototype, vec![inner_val.clone()])?;
                    builder.call(STANDARD_FREE, vec![inner_val])?;
                },

                (None, false) => {
                    // Single object without deconstructor
                    builder.call(STANDARD_FREE, vec![inner_val])?;
                },
            }

            builder.add_instruction(
                VirtualInstruction::Jump { target: post_deconstruct },
                BCType::unit()
            )?;

            builder.set_current_block(post_deconstruct);
        },

        _ => ()
    }

    Some(())
}

pub fn generate_deconstructor(builder: &mut BytecodeBuilder, _type: &CXType) -> Option<()> {
    let deconstructor_prototype = deconstructor_prototype(_type)?;

    builder.new_function(deconstructor_prototype.clone());

    let self_val = builder.add_instruction(
        VirtualInstruction::FunctionParameter { param_index: 0 },
        BCType::default_pointer()
    )?;

    let as_bc = builder.convert_cx_type(&_type)?;

    match &_type.kind {
        CXTypeKind::Structured { fields, .. } => {
            for (i, (_, field_type)) in fields.iter().enumerate() {
                let struct_access = get_cx_struct_field_by_index(builder, &as_bc, i)?;
                let ptr = builder.add_instruction(
                    VirtualInstruction::StructAccess {
                        struct_: self_val.clone(),
                        struct_type: as_bc.clone(),

                        field_index: i,
                        field_offset: struct_access.offset
                    },
                    BCType::default_pointer()
                )?;

                deconstruct_variable(builder, &ptr, field_type)?;
            }

            if let Some(destructor) = builder.get_destructor(&_type) {
                builder.call(&destructor, vec![self_val])?;
            }
        },

        _ => panic!("PANIC: Deconstructor generation for type kind {} not implemented", _type),
    }

    builder.finish_function();
    Some(())
}