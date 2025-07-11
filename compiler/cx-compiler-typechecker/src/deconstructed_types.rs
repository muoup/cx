use std::collections::HashSet;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::node_type_map::{AllocationType, DeconstructionType, DeconstructorData};
use uuid::uuid;
use crate::{TypeCheckResult, TypeEnvironment};

#[derive(Default)]
struct GenerationData {
    seen: HashSet<u64>,
    generated_for: HashSet<u64>,

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
    let _type = type_.intrinsic_type(&env.type_map)?;

    if data.seen.contains(&type_.uuid) {
        return Some(());
    }

    data.seen.insert(type_.uuid);

    match &_type.kind {
        CXTypeKind::StrongPointer { inner, .. } =>
            generate_deconstructor_data_for_type(env, data, inner),

        CXTypeKind::Structured { fields, .. } => {
            let mut deconstructor_data = DeconstructorData {
                _type: type_.clone(),
                
                rec_deconstructor_calls: Vec::new(),
                free_indices: Vec::new(),
                
                deallocations: Vec::new(),
            };
            
            if _type.has_destructor(env.type_map) || fields.iter().any(|(_, field_type)| field_type.is_strong_ptr(env.type_map)) {
                data.generated_for.insert(type_.uuid);
            }

            for (i, (_, field_type)) in fields.iter().enumerate() {
                generate_deconstructor_data_for_type(env, data, field_type)?;
                
                let allocation_type = match field_type.intrinsic_type_kind(&env.type_map)? {
                    CXTypeKind::StrongPointer { is_array: false, .. }
                        => AllocationType::Single,
                    CXTypeKind::StrongPointer { is_array: true, .. }
                        => AllocationType::Array,
                    
                    _ => AllocationType::None,
                };
                
                let deconstruction_type = DeconstructionType {
                    index: i,
                    has_deconstructor: data.generated_for.contains(&field_type.uuid),
                    allocation_type,
                };
                
                deconstructor_data.deallocations.push(deconstruction_type);

                if data.generated_for.contains(&field_type.uuid) {
                    deconstructor_data.rec_deconstructor_calls.push(i);
                }

                if field_type.is_strong_ptr(&env.type_map) {
                    deconstructor_data.free_indices.push(i);
                }
            }
            
            if deconstructor_data.deallocations.is_empty() {
                return Some(());
            }

            data.deconstructors.push(deconstructor_data);
            Some(())
        },

        _ => Some(())
    }
}