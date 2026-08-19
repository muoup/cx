use std::collections::{HashMap, HashSet};

use cx_target::ArchitectureConfig;

use crate::ty::interface::MTRegistry;

use super::{
    MIRBitfieldAccess, MIRField, MIRFunctionType, MIRLayoutError, MIRType, MIRTypeID, MIRTypeKind,
    MIRTypeLayout,
};

#[derive(Debug, Clone)]
pub struct MIRTypeRegistryBuilder {
    architecture: ArchitectureConfig,
    definitions: Vec<Option<MIRType>>,
 
    interner: HashMap<MIRType, MIRTypeID>,
    debug_names: Vec<Option<String>>,
    next_id: usize,
}

#[derive(Debug, Clone)]
pub struct MIRTypeRegistry {
    architecture: ArchitectureConfig,
    definitions: Vec<MIRType>,
    layouts: Vec<MIRTypeLayout>,
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
}

impl MIRTypeRegistryBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,
            definitions: Vec::new(),
            interner: HashMap::new(),
            debug_names: Vec::new(),
            layouts: Vec::new(),
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

    pub fn reserve_id_space(&mut self, end: usize) {
        self.next_id = self.next_id.max(end);
        let end = end;
        if self.definitions.len() < end {
            self.definitions.resize_with(end, || None);
            self.debug_names.resize(end, None);
            self.layouts.resize(end, None);
        }
    }

    pub fn find(&self, definition: &MIRType) -> Option<MIRTypeID> {
        self.interner.get(definition).copied()
    }

    pub fn define(&mut self, id: MIRTypeID, definition: MIRType) -> Result<(), MIRLayoutError> {
        self.ensure_capacity(id.index());
        self.next_id = self.next_id.max(id.index() + 1);
        let slot = &mut self.definitions[id.index()];
        if slot.is_some() {
            return Err(MIRLayoutError::DuplicateType(id));
        }
        *slot = Some(definition.clone());
        self.layouts[id.index()] = None;
        self.interner.entry(definition).or_insert(id);
        Ok(())
    }

    fn ensure_capacity(&mut self, index: usize) {
        if self.definitions.len() <= index {
            let len = index + 1;
            self.definitions.resize_with(len, || None);
            self.debug_names.resize(len, None);
            self.layouts.resize(len, None);
        }
    }

    pub fn same_type(&self, left: MIRTypeID, right: MIRTypeID) -> bool {
        if left == right {
            return true;
        }

        self.same_type_inner(left, right, &mut HashSet::new())
    }

    fn same_type_inner(
        &self,
        left: MIRTypeID,
        right: MIRTypeID,
        compared: &mut HashSet<(MIRTypeID, MIRTypeID)>,
    ) -> bool {
        if !compared.insert((left, right)) {
            return true;
        }

        let (Some(left), Some(right)) = (self.definition(left), self.definition(right)) else {
            return false;
        };
        left.minimum_alignment == right.minimum_alignment
            && same_kind(&left.kind, &right.kind, |left, right| {
                self.same_type_inner(left, right, compared)
            })
    }
}
