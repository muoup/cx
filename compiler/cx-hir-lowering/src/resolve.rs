use std::collections::HashMap;

use cx_hir::{
    ast::{function::HIRComptimeFnPrototype, types::HIRTagKind},
    registry::GlobalSymbolRegistry,
    symbols::{HIRSymbol, HIRSymbolKind, TypeConstructorData},
};
use cx_hmir::{HMIRDefID, HMIRDefRef, HMIRFloatWidth, HMIRIntWidth, HMIRTypeDesc};
use cx_namespace::{
    lookup::{QualifiedLookup, QualifiedLookupResult},
    module::{NamespacePath, QualifiedName},
};
use cx_target::ArchitectureConfig;
use cx_thir::{
    intrinsic_types::INTRINSIC_TYPES,
    thir::data::{THIRFloatType, THIRIntType, THIRTypeKind},
};
use cx_util::identifier::CXIdent;

pub(crate) struct Resolver<'a> {
    registry: &'a GlobalSymbolRegistry,
    architecture: ArchitectureConfig,
    defs: HashMap<QualifiedName, HMIRDefID>,
}

pub(crate) enum GlobalSymbol {
    Def(HMIRDefRef),
    ComptimeFunction(HMIRDefRef, Box<HIRComptimeFnPrototype>),
    Constructor(TypeConstructorData, CXIdent),
    Primitive(HMIRTypeDesc),
}

struct RegistryLookup<'a> {
    registry: &'a GlobalSymbolRegistry,
    tag: Option<HIRTagKind>,
}

impl QualifiedLookup for RegistryLookup<'_> {
    type Output = Vec<HIRSymbol>;

    fn lookup_exact(
        &self,
        _: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output> {
        if self.tag.is_none()
            && let Some(symbols) = self.registry.resolve(name, false)
        {
            return Some(symbols);
        }
        let symbols = self
            .registry
            .resolve(name, true)?
            .into_iter()
            .filter(|symbol| self.tag.is_none() || symbol.tag == self.tag)
            .collect::<Vec<_>>();
        (!symbols.is_empty()).then_some(symbols)
    }

    fn resolve_aliases(
        &self,
        lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath> {
        self.registry.resolve_aliases(lexical_namespace, namespace)
    }
}

pub(crate) fn def_name(name: QualifiedName, tag: Option<HIRTagKind>) -> QualifiedName {
    match tag {
        Some(tag) => QualifiedName::new(
            name.namespace,
            CXIdent::from(format!("{} {}", tag.prefix(), name.name).as_str()),
        ),
        None => name,
    }
}

impl<'a> Resolver<'a> {
    pub(crate) fn new(registry: &'a GlobalSymbolRegistry, architecture: ArchitectureConfig) -> Self {
        Self {
            registry,
            architecture,
            defs: HashMap::new(),
        }
    }

    pub(crate) fn declare_def(&mut self, name: QualifiedName, id: HMIRDefID) {
        self.defs.entry(name).or_insert(id);
    }

    pub(crate) fn def_ref(&self, name: QualifiedName) -> HMIRDefRef {
        match self.defs.get(&name) {
            Some(id) => HMIRDefRef::Local(*id),
            None => HMIRDefRef::External(name),
        }
    }

    pub(crate) fn resolve(
        &self,
        namespace: &NamespacePath,
        name: &QualifiedName,
        tag: Option<HIRTagKind>,
    ) -> GlobalSymbol {
        let lookup = RegistryLookup {
            registry: self.registry,
            tag,
        };
        let QualifiedLookupResult::Found {
            resolved_name,
            value,
        } = lookup.qualified_lookup(namespace, name)
        else {
            if tag.is_none()
                && name.namespace.is_root()
                && let Some(primitive) = self.primitive(name.name.as_str())
            {
                return GlobalSymbol::Primitive(primitive);
            }
            return GlobalSymbol::Def(self.def_ref(def_name(name.clone(), tag)));
        };

        let Some(symbol) = value.into_iter().next() else {
            return GlobalSymbol::Def(self.def_ref(resolved_name));
        };
        match symbol.kind {
            HIRSymbolKind::ComptimeFunction(function) => GlobalSymbol::ComptimeFunction(
                self.def_ref(resolved_name),
                Box::new(function.base().clone()),
            ),
            HIRSymbolKind::TypeConstructor(constructor) => {
                GlobalSymbol::Constructor(constructor.base().clone(), resolved_name.name)
            }
            _ => GlobalSymbol::Def(self.def_ref(def_name(resolved_name, symbol.tag))),
        }
    }

    fn primitive(&self, name: &str) -> Option<HMIRTypeDesc> {
        let (_, kind) = INTRINSIC_TYPES
            .iter()
            .find(|(intrinsic, _)| *intrinsic == name)?;

        Some(match kind(&self.architecture)? {
            THIRTypeKind::Void => HMIRTypeDesc::Void,
            THIRTypeKind::Unreachable => HMIRTypeDesc::Unreachable,
            THIRTypeKind::Str => HMIRTypeDesc::Str,
            THIRTypeKind::Integer { signed, ty } => HMIRTypeDesc::Int {
                width: int_width(ty),
                signed,
            },
            THIRTypeKind::Float { ty } => HMIRTypeDesc::Float {
                width: match ty {
                    THIRFloatType::F32 => HMIRFloatWidth::F32,
                    THIRFloatType::F64 => HMIRFloatWidth::F64,
                },
            },
            THIRTypeKind::Opaque { size, alignment } => HMIRTypeDesc::Opaque { size, alignment },
            _ => return None,
        })
    }
}

fn int_width(ty: THIRIntType) -> HMIRIntWidth {
    match ty {
        THIRIntType::I1 => HMIRIntWidth::I1,
        THIRIntType::I8 => HMIRIntWidth::I8,
        THIRIntType::I16 => HMIRIntWidth::I16,
        THIRIntType::I32 => HMIRIntWidth::I32,
        THIRIntType::I64 => HMIRIntWidth::I64,
        THIRIntType::I128 => HMIRIntWidth::I128,
    }
}
