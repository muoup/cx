use crate::builder::BytecodeBuilder;
use crate::BytecodeResult;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::node_type_map::{AllocationType, DeconstructorData};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionPrototype, BCParameter, BCPtrBinOp, ValueID, VirtualInstruction};
use crate::aux_routines::get_cx_struct_field_by_index;

const STANDARD_FREE: &str = "__stdfree";
const STANDARD_FREE_ARRAY: &str = "__stdfreearray";
const STANDARD_FREE_ARRAY_NOOP: &str = "__stdarrayfreearray_destructor_noop";

fn deconstructor_name(type_: &CXType) -> String {
    format!("deconstruct_{}", type_.uuid)
}

fn deconstructor_prototype(type_: &CXType) -> BCFunctionPrototype {
    let deconstructor_name = deconstructor_name(type_);

    BCFunctionPrototype {
        name: deconstructor_name,
        return_type: BCType::unit(),
        params: vec![
            BCParameter { name: None, _type: BCType::from(BCTypeKind::Pointer) }
        ],
        var_args: false,
    }
}

fn load_mem(
    builder: &mut BytecodeBuilder,
    val: ValueID,
    unloaded: bool,
) -> BytecodeResult<ValueID> {
    if !unloaded {
        return Some(val);
    }
    
    builder.add_instruction_bt(
        VirtualInstruction::Load {
            value: val,
        },
        BCType::from(BCTypeKind::Pointer)
    )
}

fn if_tag_call(
    builder: &mut BytecodeBuilder,
    pointer: ValueID,
    function_name: &str,
    args: Vec<ValueID>,
) -> BytecodeResult<()> {
    let run = builder.create_named_block("free");
    let skip = builder.create_named_block("skip");
 
    let has_tag = builder.add_instruction_bt(
        VirtualInstruction::HasPointerTag {
            value: pointer,
        },
        BCType::from(BCTypeKind::Bool)
    )?;
    
    builder.add_instruction_bt(
        VirtualInstruction::Branch {
            condition: has_tag, 
            true_block: run,
            false_block: skip,
        },
        BCType::from(BCTypeKind::Bool)
    )?;
    
    builder.set_current_block(run);
    let func = builder.fn_ref(function_name)?
        .expect("INTERNAL PANIC: Function not found");
    
    builder.add_instruction(
        VirtualInstruction::DirectCall {
            func, args,
            method_sig: builder.fn_map.get(function_name).unwrap().clone()
        },
        CXType::unit()
    )?;
    builder.add_instruction_bt(
        VirtualInstruction::Jump { target: skip },
        BCType::from(BCTypeKind::Unit)
    )?;
    
    builder.set_current_block(skip);
    
    Some(())
}

pub fn try_invoke_deconstructor(
    builder: &mut BytecodeBuilder,
    val: ValueID,
    type_: &CXType,
    unloaded: bool,
) -> BytecodeResult<bool> {
    let intrinsic_type = type_.intrinsic_type(&builder.cx_type_map)?.clone();

    match &intrinsic_type.kind {
        CXTypeKind::StrongPointer { inner, is_array: false, .. } => {
            try_invoke_deconstructor(builder, val, inner, false)?;

            let val = load_mem(builder, val, unloaded)?;
            if_tag_call(
                builder,
                val,
                STANDARD_FREE,
                vec![val]
            )?;

            Some(true)
        },

        CXTypeKind::StrongPointer { inner, is_array: true, .. } => {
            let deconstructor_opt = get_deconstructor(builder, inner.as_ref())?;

            let val = load_mem(builder, val, unloaded)?;
            
            if let Some(deconstructor_name) = deconstructor_opt {
                let deconstructor = builder.fn_ref(&deconstructor_name)?
                    .expect("INTERNAL PANIC: Deconstructor function not found");

                let type_size = builder.convert_cx_type(inner.as_ref())?
                    .fixed_size();
                let size_imm = builder.add_instruction_bt(
                    VirtualInstruction::Immediate { value: type_size as i32 },
                    BCType::from(BCTypeKind::Unsigned { bytes: 8 })
                )?;

                let ptr_to = builder.add_instruction_bt(
                    VirtualInstruction::GetFunctionAddr {
                        func: deconstructor,
                    },
                    BCType::from(BCTypeKind::Pointer)
                )?;
                
                if_tag_call(
                    builder,
                    val,
                    STANDARD_FREE_ARRAY,
                    vec![val, size_imm, ptr_to]
                )?;
            } else {
                if_tag_call(
                    builder,
                    val,
                    STANDARD_FREE_ARRAY_NOOP,
                    vec![val]
                )?;
            }

            Some(true)
        },

        _ => {
            if let Some(deconstructor_name) = get_deconstructor(builder, type_)? {
                let func = builder.fn_ref(&deconstructor_name)?
                    .expect("INTERNAL PANIC: Deconstructor function not found");
                
                builder.add_instruction(
                    VirtualInstruction::DirectCall {
                        func,
                        args: vec![val],
                        method_sig: builder.fn_map.get(&deconstructor_name).unwrap().clone(),
                    },
                    CXType::unit()
                );

                Some(true)
            } else {
                Some(false)
            }
        },
    };

    Some(false)
}

fn get_deconstructor(
    builder: &mut BytecodeBuilder,
    type_: &CXType
) -> BytecodeResult<Option<String>> {
    let intrinsic_type = type_.intrinsic_type(&builder.cx_type_map)?;
    let deconstructor_name = deconstructor_name(intrinsic_type);

    if builder.fn_map.contains_key(&deconstructor_name) {
        Some(Some(deconstructor_name))
    } else {
        Some(None)
    }
}

pub fn generate_deconstructor(
    builder: &mut BytecodeBuilder,
    data: &DeconstructorData
) -> Option<()> {
    let deconstructor_prototype = deconstructor_prototype(&data._type);

    builder.fn_map.insert(deconstructor_prototype.name.clone(), deconstructor_prototype.clone());
    builder.new_function(deconstructor_prototype);

    let struct_val = builder.add_instruction_bt(
        VirtualInstruction::FunctionParameter { param_index: 0 },
        BCType::from(BCTypeKind::Pointer)
    )?;

    let as_bc = builder.convert_cx_type(&data._type)?;

    if let CXTypeKind::Structured { fields, .. } = &data._type.kind {
        for dec_type in data.deallocations.iter() {
            if !dec_type.has_deconstructor && matches!(dec_type.allocation_type, AllocationType::None) {
                continue;
            }

            let access = get_cx_struct_field_by_index(builder, &as_bc, dec_type.index)?;
            let field = builder.add_instruction_bt(
                VirtualInstruction::StructAccess {
                    field_index: access.index,
                    field_offset: access.offset,
                    struct_: struct_val,
                    struct_type: as_bc.clone(),
                },
                access._type.clone()
            )?;

            try_invoke_deconstructor(builder, field, &fields[dec_type.index].1, true)?;
        }
    }

    builder.finish_function(true);

    Some(())
}