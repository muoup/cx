use std::collections::HashMap;

use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::types::{LMIRIntegerType, LMIRType};
use cx_lmir::{
    LMIRABISlot, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRGlobalInitializer,
    LMIRParameter, LMIRParameterABI, LMIRReturnABI, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRConstant, MIRGlobalID, MIRTypeKind, MIRTypeRegistryBuilder,
    MIRUnit,
};
use cx_util::{identifier::CXIdent};

use super::typing::{convert_float_type, convert_integer_type};

pub(super) fn lower_global_initializer(
    mir: &MIRUnit,
    constant: &MIRConstant,
    global_indices: &HashMap<MIRGlobalID, u32>,
) -> LMIRGlobalInitializer {
    match constant {
        MIRConstant::Bool(value) => LMIRGlobalInitializer::Integer {
            value: i128::from(*value),
            _type: LMIRIntegerType::I1,
            signed: false,
        },
        MIRConstant::Integer { value, ty, signed } => LMIRGlobalInitializer::Integer {
            value: *value,
            _type: convert_integer_type(*ty),
            signed: *signed,
        },
        MIRConstant::Float { value, ty } => LMIRGlobalInitializer::Float {
            value: *value,
            _type: convert_float_type(*ty),
        },
        MIRConstant::Aggregate { ty, fields }
            if matches!(mir.types().kind(*ty).unwrap(), MIRTypeKind::Union { .. })
                && fields.iter().all(|(_, value)| is_zero_constant(value)) =>
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
        MIRConstant::Null { .. } => LMIRGlobalInitializer::Null,
        MIRConstant::Global { global, .. } => LMIRGlobalInitializer::Global(
            *global_indices
                .get(global)
                .expect("global initializer references a filtered global"),
        ),
        MIRConstant::GlobalOffset { global, offset, .. } => LMIRGlobalInitializer::GlobalOffset {
            global: *global_indices
                .get(global)
                .expect("global initializer references a filtered global"),
            offset: *offset,
        },
        MIRConstant::Function(function) => LMIRGlobalInitializer::Function(
            mir.function(*function)
                .expect("invalid MIR function constant")
                .prototype()
                .signature
                .symbol_name
                .to_string(),
        ),
        MIRConstant::Unit | MIRConstant::String(_) | MIRConstant::Undefined => {
            panic!("unsupported MIR global initializer: {constant:?}")
        }
    }
}

fn is_zero_constant(constant: &MIRConstant) -> bool {
    match constant {
        MIRConstant::Bool(value) => !value,
        MIRConstant::Integer { value, .. } => *value == 0,
        MIRConstant::Null { .. } => true,
        MIRConstant::Aggregate { fields, .. } => {
            fields.iter().all(|(_, value)| is_zero_constant(value))
        }
        _ => false,
    }
}

pub(super) fn assertion_prototype(types: &MIRTypeRegistryBuilder) -> LMIRFunctionPrototype {
    let pointer = LMIRType::default_pointer(types.architecture());
    LMIRFunctionPrototype {
        name: CXIdent::new(ASSERTION.symbol_name()),
        linkage: LinkageType::External,
        signature: LMIRFunctionSignature {
            return_type: LMIRType::unit(),
            return_abi: LMIRReturnABI::Void,
            params: vec![
                LMIRParameter {
                    name: Some(CXIdent::new("condition")),
                    _type: LMIRType::bool(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: LMIRType::bool(),
                            offset: 0,
                        }],
                    },
                },
                LMIRParameter {
                    name: Some(CXIdent::new("message")),
                    _type: pointer.clone(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: pointer,
                            offset: 0,
                        }],
                    },
                },
            ],
            var_args: false,
        },
    }
}
