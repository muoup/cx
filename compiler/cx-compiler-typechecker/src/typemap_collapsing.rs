use cx_data_ast::parse::maps::CXTypeMap;
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_util::log_error;

pub fn collapse_typemap<MapRef>(self_map: &CXTypeMap, import_maps: &[MapRef]) -> Option<CXTypeMap> 
    where MapRef: AsRef<CXTypeMap>
{
    // this is horrifying code, please forgive me
    let mut new_map = self_map.clone();
    let pairs = self_map.iter()
        .map(|(name, _type)| (name.clone(), _type.clone()))
        .collect::<Vec<_>>();
    
    for (name, mut _type) in pairs.into_iter() {
        collapse_type(&mut _type, &mut new_map, import_maps)?;
        new_map.insert(name, _type);
    }
    
    for map in import_maps {
        for (name, _type) in map.as_ref().iter() {
            if _type.visibility_mode == VisibilityMode::Private { continue; }
            if new_map.contains_key(name) { continue; }
            
            new_map.insert(name.clone(), _type.clone());
        }
    }
    
    Some(new_map)
}

pub fn collapse_type<MapRef>(_type: &mut CXType, self_map: &mut CXTypeMap, import_maps: &[MapRef]) -> Option<()> 
    where MapRef: AsRef<CXTypeMap>
{
    match &mut _type.kind {
        CXTypeKind::Array { inner_type: inner, .. } |
        CXTypeKind::StrongPointer { inner, .. } |
        CXTypeKind::PointerTo { inner_type: inner, .. }
            => collapse_type(inner.as_mut(), self_map, import_maps)?,
        
        CXTypeKind::Union { fields, .. } |
        CXTypeKind::Structured { fields, .. } =>
            for (_, _type) in fields.iter_mut() {
                collapse_type(_type, self_map, import_maps)?;
            },
        
        CXTypeKind::Function { prototype, .. } => {
            collapse_type(&mut prototype.return_type, self_map, import_maps)?;
            
            for param in prototype.params.iter_mut() {
                collapse_type(&mut param._type, self_map, import_maps)?;
            }
        },

        CXTypeKind::VariableLengthArray { .. } |
        CXTypeKind::MemoryReference(_) 
            => unreachable!("Unexpected type encountered in collapse_type: {:?}", _type.kind),
        
        CXTypeKind::Opaque { .. } => todo!("Opaque types"),
        
        CXTypeKind::Integer { .. } |
        CXTypeKind::Float { .. } |
        CXTypeKind::Bool { .. } |
        CXTypeKind::Unit { .. }
            => {}, // Primitive types do not need collapsing
    }
    
    Some(())
}