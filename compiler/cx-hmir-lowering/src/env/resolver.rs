use rustc_hash::FxHashMap;

use cx_hmir::ty::{HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics};
use cx_mir::MIRTypeID;
use cx_tokens::TokenRange;
use index_vec::IndexVec;

use crate::value::{HMIRValue, constant::HMIRConstant};

mod demand;
mod identity;
pub(crate) mod memo;

use memo::{MemoContext, MemoContextID, MemoEntry, MemoID, MemoKey};

#[derive(Debug, Default)]
struct TypeState {
    realized: Option<HMIRType>,
    incomplete: bool,
    prepared: bool,
    lowered: Option<MIRTypeID>,
}

#[derive(Debug)]
pub(crate) struct HMIRResolver {
    identities: FxHashMap<u64, Vec<HMIRTypeID>>,
    types: IndexVec<HMIRTypeID, TypeState>,
    memo: FxHashMap<MemoKey, MemoID>,
    entries: IndexVec<MemoID, MemoEntry>,
    contexts: IndexVec<MemoContextID, MemoContext>,
    context_ids: FxHashMap<MemoContext, MemoContextID>,
    active: Vec<MemoID>,
    universe: Option<HMIRTypeID>,
}

impl HMIRResolver {
    pub fn new() -> Self {
        Self {
            identities: FxHashMap::default(),
            types: IndexVec::new(),
            memo: FxHashMap::default(),
            entries: IndexVec::new(),
            contexts: IndexVec::new(),
            context_ids: FxHashMap::default(),
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
        self.state_mut(ty_id).realized = Some(ty);
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

    fn state(&self, ty: HMIRTypeID) -> Option<&TypeState> {
        self.types.get(ty)
    }
    fn state_mut(&mut self, ty: HMIRTypeID) -> &mut TypeState {
        if self.types.len() <= ty.index() {
            self.types.resize_with(ty.index() + 1, TypeState::default);
        }
        &mut self.types[ty]
    }

    pub fn realized_type(&self, ty: HMIRTypeID) -> Option<&HMIRType> {
        self.state(ty)?.realized.as_ref()
    }

    pub fn settled(&self, ty: HMIRTypeID) -> bool {
        match self.realized_type(ty).map(HMIRType::kind) {
            Some(HMIRTypeKind::Nominal(inner)) => self.settled(*inner),
            realized => realized.is_some(),
        }
    }

    pub fn complete_type(&mut self, id: HMIRTypeID, ty: HMIRType) {
        let state = self.state_mut(id);
        state.realized = Some(ty);
        state.incomplete = false;
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
        self.state_mut(id).incomplete = true;
    }
    pub fn is_incomplete(&self, id: HMIRTypeID) -> bool {
        self.state(id).is_some_and(|state| state.incomplete)
    }
    pub fn prepared(&self, ty: HMIRTypeID) -> bool {
        self.state(ty).is_some_and(|state| state.prepared)
    }
    pub fn mark_prepared(&mut self, ty: HMIRTypeID) {
        self.state_mut(ty).prepared = true;
    }
    pub fn lowered_type(&self, ty: HMIRTypeID) -> Option<MIRTypeID> {
        self.state(ty)?.lowered
    }
    pub fn cache_lowered(&mut self, ty: HMIRTypeID, mir: MIRTypeID) {
        self.state_mut(ty).lowered = Some(mir);
    }
}
