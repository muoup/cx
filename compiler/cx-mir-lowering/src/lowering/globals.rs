use cx_lmir::{
    LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{MIRConstant, MIRGlobalState, MIRGlobalVariable, MIRTypeKind};

use crate::context::GlobalContext;

use super::typing::{convert_float_type, convert_integer_type, convert_linkage, convert_type};

pub(super) fn lower_globals(context: &mut GlobalContext<'_>) {
    for (index, id) in context.unit.global_order().iter().copied().enumerate() {
        context.global_indices.insert(id, index as u32);
        let global = context.unit.global(id).expect("missing ordered MIR global");
        context.globals.push(LMIRGlobalValue {
            name: global.name().clone(),
            _type: LMIRGlobalType::Variable {
                _type: convert_type(global.ty(), context.unit.types()),
                state: LMIRGlobalState::External,
            },
            linkage: convert_linkage(global.linkage()),
        });
    }
    for (index, id) in context.unit.global_order().iter().copied().enumerate() {
        let global = context.unit.global(id).expect("missing ordered MIR global");
        context.globals[index] = lower_global(context, global);
    }
}

fn lower_global(context: &mut GlobalContext<'_>, global: &MIRGlobalVariable) -> LMIRGlobalValue {
    let linkage = if matches!(global.state(), MIRGlobalState::External) {
        LinkageType::External
    } else {
        convert_linkage(global.linkage())
    };
    let state = match global.state() {
        MIRGlobalState::External => LMIRGlobalState::External,
        MIRGlobalState::ZeroInitialized | MIRGlobalState::Initialized(MIRConstant::Unit) => {
            LMIRGlobalState::ZeroInitialized
        }
        MIRGlobalState::Initialized(value) => {
            LMIRGlobalState::Initialized(lower_initializer(context, value))
        }
    };
    LMIRGlobalValue {
        name: global.name().clone(),
        _type: LMIRGlobalType::Variable {
            _type: convert_type(global.ty(), context.unit.types()),
            state,
        },
        linkage,
    }
}

fn lower_initializer(
    context: &mut GlobalContext<'_>,
    value: &MIRConstant,
) -> LMIRGlobalInitializer {
    match value {
        MIRConstant::Integer { value, ty } => LMIRGlobalInitializer::Integer {
            value: *value,
            _type: convert_integer_type(*ty),
        },
        MIRConstant::Float { value, ty } => LMIRGlobalInitializer::Float {
            value: *value,
            _type: convert_float_type(*ty),
        },
        MIRConstant::Aggregate { ty, fields } => {
            if matches!(
                context.unit.types().definition(*ty).unwrap().kind(),
                MIRTypeKind::Union { .. }
            ) && fields.iter().all(|(_, value)| zero(value))
            {
                return LMIRGlobalInitializer::Null;
            }
            LMIRGlobalInitializer::Aggregate {
                fields: fields
                    .iter()
                    .map(|(index, value)| (*index, lower_initializer(context, value)))
                    .collect(),
            }
        }
        MIRConstant::GlobalRef(reference) => {
            let global = context.global_indices[&reference.global];
            if reference.offset == 0 {
                LMIRGlobalInitializer::Global(global)
            } else {
                LMIRGlobalInitializer::GlobalOffset {
                    global,
                    offset: reference.offset,
                }
            }
        }
        MIRConstant::String(value) => LMIRGlobalInitializer::Global(context.string(value)),
        MIRConstant::Function(id) => LMIRGlobalInitializer::Function(
            context
                .unit
                .function(*id)
                .expect("missing function in global initializer")
                .prototype()
                .symbol_name
                .to_string(),
        ),
        MIRConstant::Nullptr { .. } | MIRConstant::Unit => LMIRGlobalInitializer::Null,
        MIRConstant::Undefined => panic!("undefined MIR global initializer"),
    }
}

fn zero(value: &MIRConstant) -> bool {
    match value {
        MIRConstant::Integer { value, .. } => *value == 0,
        MIRConstant::Nullptr { .. } | MIRConstant::Unit => true,
        MIRConstant::Aggregate { fields, .. } => fields.iter().all(|(_, value)| zero(value)),
        _ => false,
    }
}
