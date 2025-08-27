use std::collections::HashSet;
use cx_data_typechecker::cx_types::{CXType, CXTypeKind};
use cx_data_bytecode::node_type_map::{AllocationType, DeconstructionType, DeconstructorData};
use crate::{TypeCheckResult, TypeEnvironment};

#[derive(Default)]
struct GenerationData {
    seen: HashSet<u64>,
    deconstructors: Vec<DeconstructorData>,
}

pub(crate) fn generate_deconstructor_data(type_env: &TypeEnvironment) -> Option<Vec<DeconstructorData>> {
    let mut data = GenerationData::default();

    for _type in type_env.type_map.values() {
        generate_deconstructor_data_for_type(type_env, &mut data, _type)?;
    }

    Some(data.deconstructors)
}

fn generate_deconstructor_data_for_type(
    env: &TypeEnvironment,
    data: &mut GenerationData,
    type_: &CXType
) -> TypeCheckResult<()> {
    if data.seen.contains(&type_.uuid) {
        return Some(());
    }

    data.seen.insert(type_.uuid);

    match &type_.kind {
        CXTypeKind::StrongPointer { inner_type: inner, .. } =>
            generate_deconstructor_data_for_type(env, data, inner),

        CXTypeKind::Structured { fields, .. } => {
            let mut deconstructor_data = DeconstructorData {
                _type: type_.clone(),
                deconstructions: Vec::new(),
            };
            
            let mut deconstructor_needed = env.typecheck_data.destructor_exists(type_);
            
            for (i, (_, field_type)) in fields.iter().enumerate() {
                generate_deconstructor_data_for_type(env, data, field_type)?;
                
                let allocation_type = match field_type.kind {
                    CXTypeKind::StrongPointer { is_array: false, .. }
                        => AllocationType::Single,
                    CXTypeKind::StrongPointer { is_array: true, .. }
                        => AllocationType::Array,
                    
                    _ => AllocationType::None,
                };
                
                let deconstruction_type = DeconstructionType {
                    index: i,
                    has_destructor: env.destructor_exists(field_type),
                    allocation_type,
                };
                
                deconstructor_needed |= 
                       deconstruction_type.has_destructor 
                    || !matches!(deconstruction_type.allocation_type, AllocationType::None);
                
                deconstructor_data.deconstructions.push(deconstruction_type);
            }
            
            if !deconstructor_needed {
                return Some(());
            }
            
            data.deconstructors.push(deconstructor_data);
            Some(())
        },

        _ => Some(())
    }
}