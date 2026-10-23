use std::collections::{HashMap, HashSet};

use cx_hmir::ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics};
use cx_mir::MIRTypeID;
use cx_tokens::TokenRange;
use index_vec::IndexVec;

use crate::value::{HMIRValue, constant::HMIRConstant};

mod demand;
pub(crate) mod memo;

use memo::{MemoContext, MemoContextID, MemoEntry, MemoID, MemoKey};

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    incomplete: HashSet<HMIRTypeID>,
    realized: HashMap<HMIRTypeID, HMIRType>,
    lowered: HashMap<HMIRTypeID, MIRTypeID>,
    memo: HashMap<MemoKey, MemoID>,
    entries: IndexVec<MemoID, MemoEntry>,
    contexts: IndexVec<MemoContextID, MemoContext>,
    context_ids: HashMap<MemoContext, MemoContextID>,
    active: Vec<MemoID>,
    universe: Option<HMIRTypeID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            realized: HashMap::new(),
            incomplete: HashSet::new(),
            lowered: HashMap::new(),
            memo: HashMap::new(),
            entries: IndexVec::new(),
            contexts: IndexVec::new(),
            context_ids: HashMap::new(),
            active: Vec::new(),
            universe: None,
        }
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let universe = if matches!(ty.kind(), HMIRTypeKind::Type) {
            if let Some(id) = self.universe {
                return id;
            }
            None
        } else {
            Some(self.universe())
        };
        let id = MemoID::new(self.entries.len());
        let ty_id = Self::type_id(id);
        let value = HMIRValue::constant(universe.unwrap_or(ty_id), HMIRConstant::Type(ty_id));
        self.entries.push(MemoEntry::ready(value));
        self.realized.insert(ty_id, ty);
        if universe.is_none() {
            self.universe = Some(ty_id);
        }
        ty_id
    }

    pub fn universe(&mut self) -> HMIRTypeID {
        if let Some(id) = self.universe {
            return id;
        }
        self.realize_type(HMIRType::new(
            HMIRTypeKind::Type,
            HMIRTypeSemantics::default(),
            TokenRange::internal(),
            None,
        ))
    }

    pub fn intern_context(&mut self, context: MemoContext) -> MemoContextID {
        if let Some(id) = self.context_ids.get(&context) {
            return *id;
        }
        let id = self.contexts.push(context.clone());
        self.context_ids.insert(context, id);
        id
    }

    pub fn reserve(&mut self, key: MemoKey) -> MemoID {
        if let Some(id) = self.memo.get(&key) {
            return *id;
        }
        let id = self.entries.push(MemoEntry::reserved(key.clone()));
        self.memo.insert(key, id);
        id
    }

    pub fn type_id(id: MemoID) -> HMIRTypeID {
        HMIRTypeID::new(id.index())
    }
    pub fn type_memo(ty: HMIRTypeID) -> MemoID {
        MemoID::new(ty.index())
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        self.realized.get(&ty)
    }

    pub fn settled(&self, ty: HMIRTypeID) -> bool {
        match self.realized.get(&ty).map(HMIRType::kind) {
            Some(HMIRTypeKind::Nominal(inner)) => self.settled(*inner),
            realized => realized.is_some(),
        }
    }

    pub fn complete_type(&mut self, id: HMIRTypeID, ty: HMIRType) {
        self.realized.insert(id, ty);
        self.incomplete.remove(&id);
    }

    pub fn alias_type(&mut self, id: HMIRTypeID, target: HMIRTypeID, span: &TokenRange) {
        let kind = HMIRTypeKind::Nominal(target);
        self.complete_type(
            id,
            HMIRType::new(kind, HMIRTypeSemantics::default(), span.clone(), None),
        );
    }

    pub fn forward_type(
        &mut self,
        id: HMIRTypeID,
        semantics: HMIRTypeSemantics,
        span: &TokenRange,
        name: Option<String>,
    ) {
        let kind = HMIRTypeKind::Opaque {
            size: 0,
            alignment: 1,
        };
        self.complete_type(id, HMIRType::new(kind, semantics, span.clone(), name));
        self.mark_incomplete(id);
    }

    pub fn mark_incomplete(&mut self, id: HMIRTypeID) {
        self.incomplete.insert(id);
    }
    pub fn is_incomplete(&self, id: HMIRTypeID) -> bool {
        self.incomplete.contains(&id)
    }
    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        self.lowered.get(&ty).copied()
    }
    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        self.lowered.insert(ty, mir);
    }
}
