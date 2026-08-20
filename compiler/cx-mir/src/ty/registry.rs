use std::collections::HashMap;

use cx_target::ArchitectureConfig;

use crate::{MIRTypeKind, ty::interface::MTRegistry};

use super::{MIRLayoutError, MIRType, MIRTypeID};

#[derive(Debug, Clone)]
pub struct MIRTypeRegistryBuilder {
    architecture: ArchitectureConfig,
    definitions: Vec<Option<MIRType>>,

    interner: HashMap<MIRType, MIRTypeID>,
    debug_names: HashMap<MIRTypeID, String>,
    next_id: usize,
}

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    architecture: ArchitectureConfig,
    definitions: Vec<MIRType>,
    debug_names: HashMap<MIRTypeID, String>,
}

impl MTRegistry for MIRTypeRegistry {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(id.index())
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.definitions
            .iter()
            .position(|t| t == ty)
            .map(MIRTypeID::new)
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(|s| s.as_str())
    }
}

impl MTRegistry for MIRTypeRegistryBuilder {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.definitions.get(id.index()).and_then(Option::as_ref)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.interner.get(ty).copied()
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id)
            .map(|s| s.as_str())
    }
}

impl MIRTypeRegistryBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            definitions: Vec::new(),
            interner: HashMap::new(),
            debug_names: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn intern(&mut self, definition: MIRType) -> MIRTypeID {
        if let Some(id) = self.interner.get(&definition).copied() {
            return id;
        }

        let id = MIRTypeID::new(self.next_id);
        self.next_id += 1;
        self.ensure_capacity(id.index());
        self.definitions[id.index()] = Some(definition.clone());
        self.interner.insert(definition, id);
        id
    }

    pub fn set_debug_name(&mut self, id: MIRTypeID, name: String) {
        self.debug_names.insert(id, name);
    }

    pub fn reserve_id_space(&mut self, end: usize) {
        self.next_id = self.next_id.max(end);
        let end = end;
        
        if self.definitions.len() < end {
            self.definitions.resize_with(end, || None);
        }
    }

    pub fn find(&self, definition: &MIRType) -> Option<MIRTypeID> {
        self.interner.get(definition).copied()
    }

    pub fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID> {
        self.interner
            .iter()
            .find_map(|(ty, id)| if &ty.kind == kind { Some(*id) } else { None })
    }

    pub fn define(&mut self, id: MIRTypeID, definition: MIRType) -> Result<(), MIRLayoutError> {
        self.ensure_capacity(id.index());
        self.next_id = self.next_id.max(id.index() + 1);
        let slot = &mut self.definitions[id.index()];
        if slot.is_some() {
            return Err(MIRLayoutError::DuplicateType(id));
        }
        *slot = Some(definition.clone());
        self.interner.entry(definition).or_insert(id);
        Ok(())
    }

    fn ensure_capacity(&mut self, index: usize) {
        if self.definitions.len() <= index {
            let len = index + 1;
            self.definitions.resize_with(len, || None);
        }
    }
}
