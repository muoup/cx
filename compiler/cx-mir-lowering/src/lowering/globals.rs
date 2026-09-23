use std::collections::HashMap;

use cx_lmir::{
    LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue,
    LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::registry::MIRTypeRegistry;
use cx_mir::{
    MIRConstant, MIRGlobalID, MIRGlobalRef, MIRGlobalState, MIRGlobalVariable, MIRTypeKind, MIRUnit,
};

use super::typing::{convert_float_type, convert_integer_type, convert_linkage, convert_type};

pub(super) fn lower_global(
    mir: &MIRUnit,
    global: &MIRGlobalVariable,
    types: &MIRTypeRegistry,
    global_indices: &HashMap<MIRGlobalID, u32>,
) -> LMIRGlobalValue {
    let linkage = if matches!(global.state(), MIRGlobalState::External) {
        LinkageType::External
    } else {
        convert_linkage(global.linkage())
    };

    let lowered_type = convert_type(global.ty(), types);
    let lowered = match global.state() {
        MIRGlobalState::External => LMIRGlobalType::Variable {
            _type: lowered_type,
            state: LMIRGlobalState::External,
        },
        MIRGlobalState::ZeroInitialized | MIRGlobalState::Initialized(MIRConstant::Unit) => {
            LMIRGlobalType::Variable {
                _type: lowered_type,
                state: LMIRGlobalState::ZeroInitialized,
            }
        }
        MIRGlobalState::Initialized(constant) => LMIRGlobalType::Variable {
            _type: lowered_type,
            state: LMIRGlobalState::Initialized(lower_global_initializer(
                mir,
                constant,
                global_indices,
            )),
        },
    };

    LMIRGlobalValue {
        name: global.name().clone(),
        _type: lowered,
        linkage,
    }
}

fn lower_global_initializer(
    mir: &MIRUnit,
    constant: &MIRConstant,
    global_indices: &HashMap<MIRGlobalID, u32>,
) -> LMIRGlobalInitializer {
    match constant {
        MIRConstant::Integer { value, ty } => LMIRGlobalInitializer::Integer {
            value: *value,
            _type: convert_integer_type(*ty),
        },
        MIRConstant::Float { value, ty } => LMIRGlobalInitializer::Float {
            value: *value,
            _type: convert_float_type(*ty),
        },
        MIRConstant::Aggregate { ty, fields }
            if matches!(
                mir.types().definition(*ty).unwrap().kind(),
                MIRTypeKind::Union { .. }
            ) && fields.iter().all(|(_, value)| is_zero_constant(value)) =>
        {
            LMIRGlobalInitializer::Null
        }
        MIRConstant::Aggregate { fields, .. } => LMIRGlobalInitializer::Aggregate {
            fields: fields
                .iter()
                .map(|(index, value)| {
                    (*index, lower_global_initializer(mir, value, global_indices))
                })
                .collect(),
        },
        MIRConstant::Nullptr { .. } => LMIRGlobalInitializer::Null,
        MIRConstant::GlobalRef(MIRGlobalRef { global, offset, .. }) => {
            let global = *global_indices
                .get(global)
                .expect("global initializer references a filtered global");
            if *offset == 0 {
                LMIRGlobalInitializer::Global(global)
            } else {
                LMIRGlobalInitializer::GlobalOffset {
                    global,
                    offset: *offset,
                }
            }
        }
        MIRConstant::Function(function) => LMIRGlobalInitializer::Function(
            mir.function(*function)
                .expect("invalid MIR function constant")
                .prototype()
                .symbol_name
                .to_string(),
        ),
        MIRConstant::String(_) => todo!(),
        
        MIRConstant::Unit | MIRConstant::Undefined => {
            panic!("unsupported MIR global initializer: {constant:?}")
        }
    }
}

fn is_zero_constant(constant: &MIRConstant) -> bool {
    match constant {
        MIRConstant::Integer { value, .. } => *value == 0,
        MIRConstant::Nullptr { .. } => true,
        MIRConstant::Aggregate { fields, .. } => {
            fields.iter().all(|(_, value)| is_zero_constant(value))
        }
        _ => false,
    }
}
