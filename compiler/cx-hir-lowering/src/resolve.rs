use std::{cell::RefCell, collections::HashMap};

use cx_hir::{
    ast::{
        function::HIRComptimeFnPrototype, modifiers::HIRSymbolNameScheme,
        template::HIRTemplatePrototype,
        types::{HIRTagKind, HIRType, HIRTypeKind, HIRTypeLookup},
    },
    intrinsic_types::{HIRIntrinsicType, INTRINSIC_TYPES},
    registry::{ExportNameMode, GlobalSymbolRegistry},
    symbols::{HIRSymbol, HIRSymbolKind, TypeConstructorData},
};
use cx_hmir::{HMIRDef, HMIRDefID, HMIRDefRef, HMIRFloatWidth, HMIRIntWidth, HMIRTypeDesc};
use cx_namespace::{
    lookup::{QualifiedLookup, QualifiedLookupResult},
    mangling::mangle_namespace_symbol,
    module::{NamespacePath, QualifiedName},
};
use cx_target::ArchitectureConfig;
use cx_util::identifier::CXIdent;

pub(crate) struct Resolver<'a> {
    registry: &'a GlobalSymbolRegistry,
    architecture: ArchitectureConfig,
    defs: HashMap<QualifiedName, HMIRDefID>,
    // Function-level statics become defs placed after the planned ones
    planned: usize,
    statics: RefCell<Vec<HMIRDef>>,
}

pub(crate) enum GlobalSymbol {
    Def(HMIRDefRef),
    // A function and the number of leading comptime parameters its template declares
    Function(HMIRDefRef, usize),
    ComptimeFunction(HMIRDefRef, Box<HIRComptimeFnPrototype>, usize),
    Constructor(TypeConstructorData, CXIdent, HMIRDefRef),
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
    pub(crate) fn new(
        registry: &'a GlobalSymbolRegistry,
        architecture: ArchitectureConfig,
        planned: usize,
    ) -> Self {
        Self {
            registry,
            architecture,
            defs: HashMap::new(),
            planned,
            statics: RefCell::new(Vec::new()),
        }
    }

    pub(crate) fn next_static(&self) -> HMIRDefID {
        HMIRDefID::new(self.planned + self.statics.borrow().len())
    }

    pub(crate) fn push_static(&self, def: HMIRDef) {
        self.statics.borrow_mut().push(def);
    }

    pub(crate) fn take_statics(&self) -> Vec<HMIRDef> {
        self.statics.take()
    }

    pub(crate) fn declare_def(&mut self, name: QualifiedName, id: HMIRDefID) {
        self.defs.entry(name).or_insert(id);
    }

    pub(crate) fn link_name(&self, name: &QualifiedName, scheme: HIRSymbolNameScheme) -> CXIdent {
        if scheme == HIRSymbolNameScheme::Unmangled
            || name.namespace.is_root()
            || self.registry.export_name_mode(&name.namespace) == ExportNameMode::Root
        {
            return name.name.clone();
        }
        CXIdent::new(mangle_namespace_symbol(name))
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
            HIRSymbolKind::Function(function) => GlobalSymbol::Function(
                self.def_ref(resolved_name),
                template_arity(function.template_prototype.as_ref()),
            ),
            HIRSymbolKind::ComptimeFunction(function) => GlobalSymbol::ComptimeFunction(
                self.def_ref(resolved_name),
                Box::new(function.base().clone()),
                template_arity(function.template_prototype.as_ref()),
            ),
            HIRSymbolKind::TypeConstructor(constructor) => {
                let variant = resolved_name.name.clone();
                let def = self.def_ref(resolved_name);
                GlobalSymbol::Constructor(constructor.base().clone(), variant, def)
            }
            _ => GlobalSymbol::Def(self.def_ref(def_name(resolved_name, symbol.tag))),
        }
    }

    pub(crate) fn constructor_payload(&self, data: &TypeConstructorData) -> Option<HIRType> {
        constructor_payload(self.registry, data)
    }

    fn primitive(&self, name: &str) -> Option<HMIRTypeDesc> {
        let (_, kind) = INTRINSIC_TYPES
            .iter()
            .find(|(intrinsic, _)| *intrinsic == name)?;

        Some(match kind(&self.architecture)? {
            HIRIntrinsicType::Void => HMIRTypeDesc::Void,
            HIRIntrinsicType::Unreachable => HMIRTypeDesc::Unreachable,
            HIRIntrinsicType::Str => HMIRTypeDesc::Str,
            HIRIntrinsicType::Bool => HMIRTypeDesc::Int {
                width: HMIRIntWidth::I1,
                signed: false,
            },
            HIRIntrinsicType::Integer { signed, bytes } => HMIRTypeDesc::Int {
                width: match bytes {
                    1 => HMIRIntWidth::I8,
                    2 => HMIRIntWidth::I16,
                    4 => HMIRIntWidth::I32,
                    _ => HMIRIntWidth::I64,
                },
                signed,
            },
            HIRIntrinsicType::Float { bytes } => HMIRTypeDesc::Float {
                width: match bytes {
                    4 => HMIRFloatWidth::F32,
                    _ => HMIRFloatWidth::F64,
                },
            },
            HIRIntrinsicType::Opaque { size, alignment } => HMIRTypeDesc::Opaque { size, alignment },
        })
    }
}

// The type a sum variant carries, which its constructor takes
pub(crate) fn constructor_payload(
    registry: &GlobalSymbolRegistry,
    data: &TypeConstructorData,
) -> Option<HIRType> {
    let HIRTypeKind::Identifier { name, lookup, .. } = &data.union_type.kind else {
        return None;
    };
    let tagged = matches!(lookup, HIRTypeLookup::Tag(_));
    registry
        .resolve(name, tagged)?
        .into_iter()
        .find_map(|symbol| {
            let HIRSymbolKind::Type(ty) = symbol.kind else {
                return None;
            };
            let HIRTypeKind::TaggedUnion { variants, .. } = &ty.base.kind else {
                return None;
            };
            let (_, payload) = variants.get(data.variant_index)?.standard_parts()?;
            Some(payload.clone())
        })
}

fn template_arity(template: Option<&HIRTemplatePrototype>) -> usize {
    template.map_or(0, |template| template.types.len())
}
