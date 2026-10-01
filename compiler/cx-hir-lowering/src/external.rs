use cx_hir::{
    ast::{function::HIRFunctionKind, global_var::HIREnumDefinition, types::HIRTagKind},
    registry::GlobalSymbolRegistry,
    symbols::{HIRSymbol, HIRSymbolKind},
};
use cx_hmir::{HMIRDef, HMIRUnit};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    body::BodyLowering,
    def::lower_def,
    plan::{DefSource, PlannedDef, is_forward_declaration},
    resolve::Resolver,
};

const TAGS: [HIRTagKind; 3] = [HIRTagKind::Struct, HIRTagKind::Union, HIRTagKind::Enum];

// Lowers a single registry symbol into a unit whose only def is the requested one; every
// reference it makes, including to itself, is external.
pub fn generate_external_hmir(
    registry: &GlobalSymbolRegistry,
    architecture: ArchitectureConfig,
    name: &QualifiedName,
) -> Option<HMIRUnit> {
    let (tag, bare) = split_tag(name);
    let symbols = registry
        .resolve(&bare, tag.is_some())?
        .into_iter()
        .filter(|symbol| tag.is_none() || symbol.tag == tag)
        .collect::<Vec<_>>();
    let symbol = preferred_symbol(&symbols, &bare)?;

    let enum_block: HIREnumDefinition;
    let span = TokenRange::internal();
    let (namespace, source) = match &symbol.kind {
        HIRSymbolKind::Function(function) => (
            lexical_namespace(&bare, &function.base.kind),
            DefSource::Function {
                prototype: &function.base,
                template: function.template_prototype.as_ref(),
                body: function.data.as_ref(),
            },
        ),
        HIRSymbolKind::ComptimeFunction(function) => (
            lexical_namespace(&bare, &function.base.kind),
            DefSource::ComptimeFunction {
                prototype: &function.base,
                template: function.template_prototype.as_ref(),
                body: &function.data,
            },
        ),
        HIRSymbolKind::Type(ty) if is_forward_declaration(&bare.name, tag, &ty.base) => {
            (bare.namespace.clone(), DefSource::OpaqueType)
        }
        HIRSymbolKind::Type(ty) => (
            bare.namespace.clone(),
            DefSource::Type {
                template: ty.template_prototype.as_ref(),
                ty: &ty.base,
            },
        ),
        HIRSymbolKind::AddressableGlobal {
            ty, symbol_naming, ..
        } => (
            bare.namespace.clone(),
            DefSource::Global {
                ty,
                mutable: true,
                initializer: None,
                linkage: LinkageMode::Extern,
                naming: *symbol_naming,
            },
        ),
        HIRSymbolKind::EnumIdent {
            enum_block_idx,
            variant_index,
        } => {
            enum_block = registry.enum_block(&bare.namespace, *enum_block_idx)?;
            (
                bare.namespace.clone(),
                DefSource::EnumVariant {
                    variants: &enum_block.variants,
                    index: *variant_index,
                },
            )
        }
        HIRSymbolKind::TypeConstructor(_) => return None,
    };

    let plan = PlannedDef::new(name.clone(), namespace.clone(), span.clone(), source);
    let resolver = Resolver::new(registry, architecture, 1);
    let mut unit = HMIRUnit::new(namespace);
    let cx = BodyLowering::new(&resolver, unit.types_mut(), plan.namespace().clone(), false);
    let kind = lower_def(cx, &plan);
    unit.push_def(HMIRDef::new(name.clone(), span, kind));
    for def in resolver.take_statics() {
        unit.push_def(def);
    }
    Some(unit)
}

fn split_tag(name: &QualifiedName) -> (Option<HIRTagKind>, QualifiedName) {
    for tag in TAGS {
        if let Some(bare) = name
            .name
            .as_str()
            .strip_prefix(tag.prefix())
            .and_then(|rest| rest.strip_prefix(' '))
        {
            return (
                Some(tag),
                QualifiedName::new(name.namespace.clone(), CXIdent::from(bare)),
            );
        }
    }
    (None, name.clone())
}

fn preferred_symbol<'s>(symbols: &'s [HIRSymbol], name: &QualifiedName) -> Option<&'s HIRSymbol> {
    symbols
        .iter()
        .find(|symbol| match &symbol.kind {
            HIRSymbolKind::Function(function) => function.data.is_some(),
            HIRSymbolKind::Type(ty) => !is_forward_declaration(&name.name, symbol.tag, &ty.base),
            _ => true,
        })
        .or_else(|| symbols.first())
}

fn lexical_namespace(name: &QualifiedName, kind: &HIRFunctionKind) -> NamespacePath {
    match kind {
        HIRFunctionKind::Standard(_) => name.namespace.clone(),
        HIRFunctionKind::AssociatedFunction { .. } => name
            .namespace
            .clone()
            .parent()
            .unwrap_or_else(NamespacePath::root),
    }
}
