use cx_lmir::{
    LMIRGlobalInitializer, LMIRGlobalState, LMIRGlobalType, LMIRGlobalValue, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{MIRConstant, MIRGlobalState, MIRGlobalVariable, MIRIntType, MIRTypeID, MIRTypeKind};

use crate::context::GlobalContext;

use super::typing::{convert_float_type, convert_integer_type, convert_linkage, convert_type};

pub(super) fn lower_globals(context: &mut GlobalContext<'_>) {
    for (index, id) in context.unit.global_order().iter().copied().enumerate() {
        context.global_indices.insert(id, index as u32);
        let global = context.unit.global(id).expect("missing ordered MIR global");
        context.globals.push(LMIRGlobalValue {
            name: global.name().clone(),
            ty: LMIRGlobalType::Variable {
                ty: convert_type(global.ty(), context.unit.types()),
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
            LMIRGlobalState::Initialized(lower_initializer(context, value, global.ty()))
        }
    };
    LMIRGlobalValue {
        name: global.name().clone(),
        ty: LMIRGlobalType::Variable {
            ty: convert_type(global.ty(), context.unit.types()),
            state,
        },
        linkage,
    }
}

fn lower_initializer(
    context: &mut GlobalContext<'_>,
    value: &MIRConstant,
    destination_ty: MIRTypeID,
) -> LMIRGlobalInitializer {
    let destination_kind = context
        .unit
        .types()
        .definition(destination_ty)
        .expect("global initializer has an invalid destination type")
        .kind()
        .clone();

    match value {
        MIRConstant::Integer { value, ty } => LMIRGlobalInitializer::Integer {
            value: *value,
            ty: convert_integer_type(*ty),
        },
        MIRConstant::Float { value, ty } => LMIRGlobalInitializer::Float {
            value: *value,
            ty: convert_float_type(*ty),
        },
        MIRConstant::Aggregate { fields, .. } => {
            if matches!(destination_kind, MIRTypeKind::Union { .. })
                && fields.iter().all(|(_, value)| zero(value))
            {
                return LMIRGlobalInitializer::Null;
            }
            LMIRGlobalInitializer::Aggregate {
                fields: fields
                    .iter()
                    .map(|(index, value)| {
                        let field_ty = aggregate_field_type(context, destination_ty, *index);
                        (*index, lower_initializer(context, value, field_ty))
                    })
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
        MIRConstant::String(value) => match destination_kind {
            MIRTypeKind::Array { length, inner } if is_character_type(context, inner) => {
                let bytes = value.as_bytes();
                assert!(
                    bytes.len() <= length,
                    "string initializer is larger than its destination array"
                );
                LMIRGlobalInitializer::Aggregate {
                    fields: (0..length)
                        .map(|index| {
                            let byte = bytes.get(index).copied().unwrap_or(0);
                            (
                                index,
                                LMIRGlobalInitializer::Integer {
                                    value: i128::from(byte),
                                    ty: convert_integer_type(MIRIntType::I8),
                                },
                            )
                        })
                        .collect(),
                }
            }
            MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. } => {
                LMIRGlobalInitializer::Global(context.string(value))
            }
            _ => panic!("string initializer has an invalid destination type"),
        },
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

fn aggregate_field_type(
    context: &GlobalContext<'_>,
    aggregate_ty: MIRTypeID,
    index: usize,
) -> MIRTypeID {
    match context
        .unit
        .types()
        .definition(aggregate_ty)
        .expect("aggregate initializer has an invalid destination type")
        .kind()
    {
        MIRTypeKind::Array { length, inner } => {
            assert!(index < *length, "array initializer index is out of bounds");
            *inner
        }
        MIRTypeKind::Structured { fields } => fields
            .get(index)
            .expect("struct initializer index is out of bounds")
            .ty(),
        MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => variants
            .get(index)
            .expect("union initializer index is out of bounds")
            .ty(),
        _ => panic!("aggregate initializer has a non-aggregate destination type"),
    }
}

fn is_character_type(context: &GlobalContext<'_>, ty: MIRTypeID) -> bool {
    matches!(
        context
            .unit
            .types()
            .definition(ty)
            .expect("array initializer has an invalid element type")
            .kind(),
        MIRTypeKind::Integer {
            ty: MIRIntType::I8,
            ..
        }
    )
}

fn zero(value: &MIRConstant) -> bool {
    match value {
        MIRConstant::Integer { value, .. } => *value == 0,
        MIRConstant::Nullptr { .. } | MIRConstant::Unit => true,
        MIRConstant::Aggregate { fields, .. } => fields.iter().all(|(_, value)| zero(value)),
        _ => false,
    }
}
