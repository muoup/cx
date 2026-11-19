use rustc_hash::FxHashMap;

use chl_hmir::{
    HMIRDefID, HMIRUnitID,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, NominalID,
        aggregate::{HMIRMoveSemantics, HMIRTypeSemantics},
    },
};
use chl_mir::MIRTypeID;
use chl_tokens::TokenRange;
use index_vec::IndexVec;
use chl_util::identifier::CHLIdent;

use crate::value::{self, HMIRValue};

pub(crate) mod demand;
mod key;

use key::TypeKey;

#[derive(Debug)]
struct TypeState {
    ty: HMIRType,
    symbol_name: Option<CHLIdent>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct NominalSite {
    pub unit: HMIRUnitID,
    pub expression: HMIRExprID,
    pub specialization: Option<HMIRTypeID>,
    pub inner: HMIRTypeID,
}

#[derive(Debug, Default)]
pub(crate) struct HMIRResolver {
    types: IndexVec<HMIRTypeID, TypeState>,
    interned: FxHashMap<TypeKey, HMIRTypeID>,
    symbols: FxHashMap<HMIRDefID, SymbolState>,
    active: Vec<HMIRDefID>,
    deferred: Vec<HMIRTypeID>,
    generated: FxHashMap<HMIRDefID, FxHashMap<Vec<HMIRValue>, HMIRValue>>,
    origins: FxHashMap<HMIRTypeID, (HMIRDefID, Vec<HMIRValue>)>,
    next_nominal: u64,
    nominal_sites: FxHashMap<NominalSite, HMIRTypeID>,
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
        resolver.realize_type(void);
        resolver
    }

    pub fn realize_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        let semantics = *ty.semantics();
        if let HMIRTypeKind::Nominal { inner, strong_identifier: None } = *ty.kind() {
            let full = semantics.apply(self.semantics(inner));
            return self.qualified(self.underlying(inner), full, ty.decl_span());
        }
        // REVIEW: Why is undefined tagged here? Do we need to intern incomplete types?
        let aggregate = matches!(
            ty.kind(),
            HMIRTypeKind::Struct { .. }
                | HMIRTypeKind::Union { .. }
                | HMIRTypeKind::TaggedUnion { .. }
                | HMIRTypeKind::Undefined(HMIRUndefined::Forward(_))
        );
        let own = match aggregate {
            true => HMIRTypeSemantics {
                constant: false,
                volatile: false,
                ..semantics
            },
            false => HMIRTypeSemantics::default(),
        };
        let base = self.intern(key::key(ty.kind(), own), || {
            HMIRType::new(
                ty.kind().clone(),
                own,
                ty.decl_span().clone(),
                ty.debug_name().filter(|_| !aggregate).map(str::to_owned),
            )
        });
        self.qualified(base, semantics, ty.decl_span())
    }

    fn nominal(&mut self, inner: HMIRTypeID, span: &TokenRange, name: Option<String>) -> HMIRTypeID {
        let strong_identifier = Some(NominalID(self.next_nominal));
        self.next_nominal = self.next_nominal.checked_add(1).expect("nominal identifiers exhausted");
        let kind = HMIRTypeKind::Nominal { inner, strong_identifier };
        self.reserve_type(HMIRType::new(kind, HMIRTypeSemantics::default(), span.clone(), name))
    }

    pub fn nominal_at(&mut self, site: NominalSite, span: &TokenRange) -> HMIRTypeID {
        if let Some(&id) = self.nominal_sites.get(&site) {
            return id;
        }
        let id = self.nominal(site.inner, span, None);
        self.nominal_sites.insert(site, id);
        id
    }

    pub fn reserve_nominal(&mut self, representation: HMIRType) -> HMIRTypeID {
        let span = representation.decl_span().clone();
        let name = representation.debug_name().map(str::to_owned);
        let inner = self.reserve_type(representation);
        self.nominal(inner, &span, name)
    }

    pub fn set_nominal_placeholder(&mut self, id: HMIRTypeID, ty: HMIRType) {
        let HMIRTypeKind::Nominal { inner, .. } = *self.realized_type(id).kind() else {
            unreachable!("a reserved nominal type has a representation");
        };
        self.complete_type(inner, ty);
    }

    pub fn complete_nominal(&mut self, id: HMIRTypeID, inner: HMIRTypeID) {
        let ty = self.realized_type(id);
        let HMIRTypeKind::Nominal { strong_identifier: Some(strong), .. } = *ty.kind() else {
            unreachable!("a reserved nominal type has a strong identifier");
        };
        let ty = HMIRType::new(
            HMIRTypeKind::Nominal { inner, strong_identifier: Some(strong) },
            *ty.semantics(),
            ty.decl_span().clone(),
            ty.debug_name().map(str::to_owned),
        );
        self.complete_type(id, ty);
    }

    pub fn reserve_type(&mut self, ty: HMIRType) -> HMIRTypeID {
        self.types.push(TypeState {
            ty,
            symbol_name: None,
            pending: None,
            prepared: false,
            lowered: None,
        })
    }

    fn complete_type(&mut self, id: HMIRTypeID, ty: HMIRType) {
        self.types[id].ty = ty;
    }

    pub fn realized_type(&self, id: HMIRTypeID) -> &HMIRType {
        &self.types[id].ty
    }

    pub fn type_name(&self, id: HMIRTypeID) -> Option<&CHLIdent> {
        self.types[self.underlying(id)].symbol_name.as_ref()
    }

    pub fn name_type(&mut self, id: HMIRTypeID, name: CHLIdent) {
        let id = self.underlying(id);
        self.types[id].symbol_name = Some(name);
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

    pub fn symbol(&self, def: HMIRDefID) -> &SymbolState {
        self.symbols.get(&def).unwrap_or(&SymbolState::Declared)
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
        self.generated.get(&def)?.get(args)
    }
    pub fn record_generated(
        &mut self,
        def: HMIRDefID,
        args: Vec<HMIRValue>,
        value: HMIRValue,
        track_origin: bool,
    ) {
        if track_origin && let Some(ty) = value::as_type(&value) {
            let ty = self.underlying(ty);
            self.origins.entry(ty).or_insert((def, args.clone()));
        }
        self.generated.entry(def).or_default().insert(args, value);
    }
    pub fn forget_generated(&mut self, def: HMIRDefID, args: &[HMIRValue]) {
        if let Some(generated) = self.generated.get_mut(&def) {
            generated.remove(args);
        }
    }
    pub fn origin(&self, ty: HMIRTypeID) -> Option<(HMIRDefID, &[HMIRValue])> {
        let (def, args) = self.origins.get(&ty)?;
        Some((*def, args.as_slice()))
    }

    fn intern(&mut self, key: TypeKey, ty: impl FnOnce() -> HMIRType) -> HMIRTypeID {
        if let Some(id) = self.interned.get(&key) {
            return *id;
        }
        let id = self.reserve_type(ty());
        self.interned.insert(key, id);
        id
    }

    // 'base' with only the specifiers of 'full' it does not carry itself
    fn qualified(&mut self, base: HMIRTypeID, full: HMIRTypeSemantics, span: &TokenRange) -> HMIRTypeID {
        let own = self.semantics(base);
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
        let kind = HMIRTypeKind::Nominal { inner: base, strong_identifier: None };
        self.intern(key::key(&kind, added), || {
            HMIRType::new(kind, added, span.clone(), None)
        })
    }

    fn underlying(&self, id: HMIRTypeID) -> HMIRTypeID {
        match self.types[id].ty.kind() {
            HMIRTypeKind::Nominal { inner, strong_identifier: None } => self.underlying(*inner),
            _ => id,
        }
    }

    fn semantics(&self, id: HMIRTypeID) -> HMIRTypeSemantics {
        let ty = &self.types[id].ty;
        match ty.kind() {
            HMIRTypeKind::Nominal { inner, .. } => ty.semantics().apply(self.semantics(*inner)),
            _ => *ty.semantics(),
        }
    }
}
