use rustc_hash::FxHashMap;

use cx_hmir::{
    HMIRDefID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::{HMIRMoveSemantics, HMIRTypeSemantics},
    },
};
use cx_mir::MIRTypeID;
use cx_tokens::TokenRange;
use index_vec::IndexVec;

use crate::value::{self, HMIRValue};

pub(crate) mod demand;
mod key;

use key::TypeKey;

#[derive(Debug)]
struct TypeState {
    ty: HMIRType,
    pending: Option<HMIRDefID>,
    prepared: bool,
    lowered: Option<MIRTypeID>,
}

#[derive(Debug, Clone)]
pub(crate) enum SymbolState {
    Declared,
    // A provisional value stands for the symbol while its own evaluation refers to it
    Evaluating(Option<HMIRValue>),
    Ready(HMIRValue),
}

#[derive(Debug, Default)]
pub(crate) struct HMIRResolver {
    types: IndexVec<HMIRTypeID, TypeState>,
    interned: FxHashMap<TypeKey, HMIRTypeID>,
    symbols: FxHashMap<HMIRDefID, SymbolState>,
    active: Vec<HMIRDefID>,
    deferred: Vec<HMIRTypeID>,
    generated: FxHashMap<(HMIRDefID, Vec<HMIRValue>), HMIRValue>,
    origins: FxHashMap<HMIRTypeID, (HMIRDefID, Vec<HMIRValue>)>,
}

impl HMIRResolver {
    // Void is always type 0, which unit values rely on
    pub fn new() -> Self {
        let mut resolver = Self::default();
        let void = HMIRType::new(
            HMIRTypeKind::Void,
            HMIRTypeSemantics::default(),
            TokenRange::Internal,
            None,
        );
        resolver.intern(void);
        resolver
    }

    // A named aggregate is a type of its own; any other type is the one type of its structure
    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let semantics = *ty.semantics();
        if let HMIRTypeKind::Nominal(inner) = *ty.kind() {
            let full = semantics.apply(self.semantics(inner));
            return self.qualified(self.underlying(inner), full, ty);
        }
        // REVIEW: Why is undefined tagged here? Do we need to intern incomplete types?
        let aggregate = matches!(
            ty.kind(),
            HMIRTypeKind::Struct { .. }
                | HMIRTypeKind::Union { .. }
                | HMIRTypeKind::TaggedUnion { .. }
                | HMIRTypeKind::Undefined(HMIRUndefined::Forward(_))
        );
        // REVIEW: debug_name is not the correct proxy for strong identifiers, we need both a strong_identifier, and a debug_name,
        // the former dictates equality, the latter just for prettied formats and need not be identifier-shaped (e.g. foo::type_name).
        if aggregate && ty.debug_name().is_some() {
            let id = self.reserve_type(ty.clone());
            return self.qualified(id, semantics, ty);
        }
        let own = match aggregate {
            true => HMIRTypeSemantics {
                constant: false,
                volatile: false,
                ..semantics
            },
            false => HMIRTypeSemantics::default(),
        };
        let bare = HMIRType::new(
            ty.kind().clone(),
            own,
            ty.decl_span().clone(),
            ty.debug_name().map(str::to_owned),
        );
        let base = self.intern(bare);
        self.qualified(base, semantics, ty)
    }

    pub fn reserve_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        self.types.push(TypeState {
            ty,
            pending: None,
            prepared: false,
            lowered: None,
        })
    }

    pub fn complete_type(&mut self, id: HMIRTypeID, ty: HMIRType) {
        self.types[id].ty = ty;
    }

    pub fn realized_type(&self, id: HMIRTypeID) -> &HMIRType {
        &self.types[id].ty
    }

    pub fn pending(&self, id: HMIRTypeID) -> Option<HMIRDefID> {
        self.types[id].pending
    }
    pub fn set_pending(&mut self, id: HMIRTypeID, def: Option<HMIRDefID>) {
        self.types[id].pending = def;
    }

    pub fn prepared(&self, id: HMIRTypeID) -> bool {
        self.types[id].prepared
    }
    pub fn mark_prepared(&mut self, id: HMIRTypeID) {
        self.types[id].prepared = true;
    }

    pub fn lowered_type(&self, id: HMIRTypeID) -> Option<MIRTypeID> {
        self.types[id].lowered
    }
    pub fn cache_lowered(&mut self, id: HMIRTypeID, mir: MIRTypeID) {
        self.types[id].lowered = Some(mir);
    }

    pub fn symbol(&self, def: HMIRDefID) -> SymbolState {
        self.symbols
            .get(&def)
            .cloned()
            .unwrap_or(SymbolState::Declared)
    }
    pub fn set_symbol(&mut self, def: HMIRDefID, state: SymbolState) {
        self.symbols.insert(def, state);
    }

    pub fn active(&self) -> &[HMIRDefID] {
        &self.active
    }
    pub fn enter(&mut self, def: HMIRDefID) {
        self.active.push(def);
    }
    pub fn leave(&mut self) {
        self.active.pop();
    }

    pub fn defer(&mut self, ty: HMIRTypeID) {
        self.deferred.push(ty);
    }
    pub fn take_deferred(&mut self) -> Option<HMIRTypeID> {
        self.deferred.pop()
    }

    pub fn generated(&self, def: HMIRDefID, args: &[HMIRValue]) -> Option<&HMIRValue> {
        self.generated.get(&(def, args.to_vec()))
    }
    pub fn record_generated(&mut self, def: HMIRDefID, args: Vec<HMIRValue>, value: HMIRValue) {
        if let Some(ty) = value::as_type(&value) {
            let ty = self.underlying(ty);
            self.origins.entry(ty).or_insert((def, args.clone()));
        }
        self.generated.insert((def, args), value);
    }
    pub fn forget_generated(&mut self, def: HMIRDefID, args: &[HMIRValue]) {
        self.generated.remove(&(def, args.to_vec()));
    }
    pub fn origin(&self, ty: HMIRTypeID) -> Option<(HMIRDefID, &[HMIRValue])> {
        let (def, args) = self.origins.get(&ty)?;
        Some((*def, args.as_slice()))
    }

    fn intern(&mut self, ty: HMIRType) -> HMIRTypeID {
        let key = key::key(&ty);
        if let Some(id) = self.interned.get(&key) {
            return *id;
        }
        let id = self.reserve_type(ty);
        self.interned.insert(key, id);
        id
    }

    // 'base' with only the specifiers of 'full' it does not carry itself
    fn qualified(&mut self, base: HMIRTypeID, full: HMIRTypeSemantics, ty: HMIRType) -> HMIRTypeID {
        let own = *self.types[base].ty.semantics();
        let added = HMIRTypeSemantics {
            constant: full.constant && !own.constant,
            volatile: full.volatile && !own.volatile,
            unsafe_move: full.unsafe_move && !own.unsafe_move,
            move_semantics: match full.move_semantics > own.move_semantics {
                true => full.move_semantics,
                false => HMIRMoveSemantics::POD,
            },
        };
        if added == HMIRTypeSemantics::default() {
            return base;
        }
        let nominal = HMIRType::new(
            HMIRTypeKind::Nominal(base),
            added,
            ty.decl_span().clone(),
            None,
        );
        self.intern(nominal)
    }

    fn underlying(&self, id: HMIRTypeID) -> HMIRTypeID {
        match self.types[id].ty.kind() {
            HMIRTypeKind::Nominal(inner) => self.underlying(*inner),
            _ => id,
        }
    }

    fn semantics(&self, id: HMIRTypeID) -> HMIRTypeSemantics {
        let ty = &self.types[id].ty;
        match ty.kind() {
            HMIRTypeKind::Nominal(inner) => ty.semantics().apply(self.semantics(*inner)),
            _ => *ty.semantics(),
        }
    }
}
