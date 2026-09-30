use std::collections::HashMap;

use crate::{
    constant::HMIRConstant,
    def::HMIRDefRef,
    ids::{HMIRNominalID, HMIRTypeID},
    ty::{
        desc::HMIRTypeDesc,
        nominal::{HMIRAggregateKind, HMIRMoveSemantics, HMIRNominalDesc},
    },
};

#[derive(Debug, Clone, Default)]
pub struct HMIRTypeInterner {
    types: Vec<HMIRTypeDesc>,
    type_ids: HashMap<HMIRTypeDesc, HMIRTypeID>,
    nominals: Vec<HMIRNominalDesc>,
    nominal_ids: HashMap<(HMIRDefRef, Vec<HMIRConstant>), HMIRNominalID>,
}

impl HMIRTypeInterner {
    pub fn intern(&mut self, desc: HMIRTypeDesc) -> HMIRTypeID {
        if let Some(id) = self.type_ids.get(&desc) {
            return *id;
        }

        let id = HMIRTypeID::new(self.types.len());
        self.types.push(desc.clone());
        self.type_ids.insert(desc, id);
        id
    }

    pub fn get(&self, id: HMIRTypeID) -> &HMIRTypeDesc {
        &self.types[id.index()]
    }

    pub fn intern_nominal(
        &mut self,
        def: HMIRDefRef,
        args: Vec<HMIRConstant>,
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
    ) -> HMIRNominalID {
        let key = (def, args);
        if let Some(id) = self.nominal_ids.get(&key) {
            return *id;
        }

        let id = HMIRNominalID::new(self.nominals.len());
        let (def, args) = key.clone();
        self.nominals
            .push(HMIRNominalDesc::new(def, args, kind, semantics));
        self.nominal_ids.insert(key, id);
        id
    }

    pub fn nominal(&self, id: HMIRNominalID) -> &HMIRNominalDesc {
        &self.nominals[id.index()]
    }

    pub fn nominal_mut(&mut self, id: HMIRNominalID) -> &mut HMIRNominalDesc {
        &mut self.nominals[id.index()]
    }
}
