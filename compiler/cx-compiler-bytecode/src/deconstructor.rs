use crate::builder::BytecodeBuilder;
use crate::BytecodeResult;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::node_type_map::{AllocationType, DeconstructorData};
use cx_data_bytecode::types::{BCType, BCTypeKind};
use cx_data_bytecode::{BCFunctionPrototype, BCParameter, BCPtrBinOp, LinkageType, ValueID, VirtualInstruction};
use cx_data_bytecode::mangling::mangle_destructor;
use crate::aux_routines::get_cx_struct_field_by_index;

const STANDARD_FREE: &str = "__stdfree";
const STANDARD_FREE_ARRAY: &str = "__stdfreearray";
const STANDARD_FREE_ARRAY_NOOP: &str = "__stdfreearray_destructor_noop";

fn deconstructor_name(type_: &CXType) -> String {
    format!("deconstruct_{}", type_.uuid)
}

fn deconstructor_prototype(type_: &CXType) -> BCFunctionPrototype {
    let deconstructor_name = deconstructor_name(type_);
    let this_param_type = BCType::from(BCTypeKind::Pointer { nullable: false, dereferenceable: 0 });

    BCFunctionPrototype {
        name: deconstructor_name,
        return_type: BCType::unit(),
        params: vec![
            BCParameter { name: None, _type: this_param_type },
        ],
        var_args: false,
        linkage: LinkageType::ODR
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
        BCType::default_pointer()
    )
}

fn if_owned_call(
    builder: &mut BytecodeBuilder,
    pointer: ValueID,
    invoke_deconstructor: Option<&CXType>,
    function_name: &str,
    args: Vec<ValueID>,
) -> BytecodeResult<()> {
    let run = builder.create_named_block("free");
    let skip = builder.create_named_block("skip");

    let zero = builder.int_const(0, 8, false)?;
    let zero_ptr = builder.add_instruction_bt(
        VirtualInstruction::IntToPtr {
            value: zero
        },
        BCType::default_pointer()
    )?;

    let nonnull = builder.add_instruction_bt(
        VirtualInstruction::PointerBinOp {
            left: pointer,
            right: zero_ptr,
            ptr_type: BCType::unit(),
            op: BCPtrBinOp::NE,
        },
        BCType::from(BCTypeKind::Bool)
    )?;
    
    builder.add_instruction_bt(
        VirtualInstruction::Branch {
            condition: nonnull,
            true_block: run,
            false_block: skip,
        },
        BCType::from(BCTypeKind::Bool)
    )?;
    
    builder.set_current_block(run);

    if let Some(inner_type) = invoke_deconstructor {
        deconstruct_variable(builder, pointer, inner_type, false)?;
    }

    let func = builder.fn_ref(function_name)?
        .unwrap_or_else(|| panic!("INTERNAL PANIC: Function not found: {function_name}"));

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

pub fn deconstruct_variable(
    builder: &mut BytecodeBuilder,
    val: ValueID,
    type_: &CXType,
    unloaded: bool,
) -> BytecodeResult<bool> {
    let intrinsic_type = type_.intrinsic_type(&builder.cx_type_map)?.clone();

    match &intrinsic_type.kind {
        CXTypeKind::StrongPointer { inner, is_array: false, .. } => {
            let val = load_mem(builder, val, unloaded)?;

            if_owned_call(
                builder,
                val,
                Some(inner.as_ref()),
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
                    BCType::default_pointer()
                )?;
                
                if_owned_call(
                    builder,
                    val,
                    None,
                    STANDARD_FREE_ARRAY,
                    vec![val, size_imm, ptr_to]
                )?;
            } else {
                if_owned_call(
                    builder,
                    val,
                    None,
                    STANDARD_FREE_ARRAY_NOOP,
                    vec![val]
                )?;
            }

            Some(true)
        },

        _ => {
            let mut generated = false;
            let deconstructor_prototype = deconstructor_prototype(type_);
            
            if let Some(name) = intrinsic_type.get_destructor(&builder.cx_type_map) {
                let deconstructor = builder.add_instruction_bt(
                    VirtualInstruction::FunctionReference {
                        name: mangle_destructor(name),
                    },
                    BCType::default_pointer()
                )?;
                
                builder.add_instruction(
                    VirtualInstruction::DirectCall {
                        func: deconstructor,
                        args: vec![val],
                        method_sig: deconstructor_prototype.clone(),
                    },
                    CXType::unit()
                )?;
                
                generated = true;
            }
            
            if let Some(deconstructor_name) = get_deconstructor(builder, type_)? {
                let func = builder.fn_ref(&deconstructor_name)?
                    .expect("INTERNAL PANIC: Deconstructor function not found");
                
                builder.add_instruction(
                    VirtualInstruction::DirectCall {
                        func,
                        args: vec![val],
                        method_sig: deconstructor_prototype
                    },
                    CXType::unit()
                );

                generated = true;
            }
            
            Some(generated)
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
    builder.new_function(deconstructor_prototype.clone());

    let struct_val = builder.add_instruction_bt(
        VirtualInstruction::FunctionParameter { param_index: 0 },
        BCType::default_pointer()
    )?;
    
    if let Some(name) = data._type.get_destructor(&builder.cx_type_map) {
        let deconstructor_name = mangle_destructor(name);
        let deconstructor = builder.fn_ref(&deconstructor_name)?
            .expect("INTERNAL PANIC: Deconstructor function not found");

        builder.add_instruction(
            VirtualInstruction::DirectCall {
                func: deconstructor,
                args: vec![struct_val],
                method_sig: deconstructor_prototype.clone(),
            },
            CXType::unit()
        )?;
    }

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

            deconstruct_variable(builder, field, &fields[dec_type.index].1, true)?;
        }
    }

    builder.finish_function(true);

    Some(())
}