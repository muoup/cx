use std::collections::{BTreeSet, HashSet};

use cx_mir::{
    MIRField, MIRUndefined, MIRTypeID, MIRTypeKind,
    ty::{interface::MTRegistry, registry::MIRTypeRegistry},
};

use crate::declarator::{aggregate_keyword, aggregate_name, declare, definition_kind};

pub(crate) struct Definitions<'a> {
    types: &'a MIRTypeRegistry,
    visited: HashSet<String>,
    deferred: Vec<MIRTypeID>,
    pub forward: BTreeSet<String>,
    pub ordered: Vec<String>,
}

impl<'a> Definitions<'a> {
    pub fn new(types: &'a MIRTypeRegistry) -> Self {
        Self {
            types,
            visited: HashSet::new(),
            deferred: Vec::new(),
            forward: BTreeSet::new(),
            ordered: Vec::new(),
        }
    }

    // Pointees are deferred so that only by-value members order definitions, which cannot be cyclic
    pub fn collect(&mut self, ty: MIRTypeID) {
        self.visit(ty);
        while let Some(pointee) = self.deferred.pop() {
            self.visit(pointee);
        }
    }

    fn visit(&mut self, ty: MIRTypeID) {
        let kind = definition_kind(self.types, ty);
        match kind {
            MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner } => self.deferred.push(*inner),
            MIRTypeKind::Array { inner, .. } => self.visit(*inner),
            MIRTypeKind::Undefined(MIRUndefined::Array { element, .. }) => self.visit(*element),
            MIRTypeKind::Undefined(_) => {
                let keyword = aggregate_keyword(kind).expect("an undefined tag has a keyword");
                self.forward.insert(format!("{keyword} {}", aggregate_name(self.types, ty)));
            }
            MIRTypeKind::Function { signature } => {
                self.visit(signature.return_type());
                for param in signature.params() {
                    self.visit(param.ty());
                }
            }
            MIRTypeKind::Structured { fields } | MIRTypeKind::Union { variants: fields } | MIRTypeKind::TaggedUnion { variants: fields } => {
                let name = aggregate_name(self.types, ty);
                if !self.visited.insert(name.clone()) {
                    return;
                }
                let keyword = aggregate_keyword(kind).expect("an aggregate has a keyword");
                self.forward.insert(format!("{keyword} {name}"));
                for field in fields {
                    self.visit(field.ty());
                }
                self.ordered.push(self.define(kind, keyword, &name));
            }
            _ => {}
        }
    }

    fn define(&self, kind: &MIRTypeKind, keyword: &str, name: &str) -> String {
        let body = match kind {
            MIRTypeKind::TaggedUnion { variants } => {
                let data = self.members(variants, "        ");
                format!("    union {{\n{data}    }} data;\n    uint8_t tag;\n")
            }
            MIRTypeKind::Structured { fields } | MIRTypeKind::Union { variants: fields } => self.members(fields, "    "),
            _ => unreachable!("only aggregates are defined"),
        };
        let guard = format!("CX_HEADER_{}_{name}", keyword.to_uppercase());
        format!("#ifndef {guard}\n#define {guard}\n{keyword} {name} {{\n{body}}};\n#endif")
    }

    fn members(&self, fields: &[MIRField], indent: &str) -> String {
        fields
            .iter()
            .enumerate()
            .filter(|(_, field)| {
                let flexible = matches!(
                    definition_kind(self.types, field.ty()),
                    MIRTypeKind::Undefined(MIRUndefined::Array { flexible: true, .. })
                );
                flexible || self.types.layout(field.ty()).size() != 0
            })
            .map(|(index, field)| {
                let member = match field {
                    MIRField::Standard { name, type_id } => {
                        let name = name.clone().unwrap_or(format!("field{index}"));
                        declare(self.types, *type_id, &name)
                    }
                    MIRField::Bitfield { name, integer_type_id, width } => {
                        let name = name.as_deref().unwrap_or("");
                        format!("{} : {width}", declare(self.types, *integer_type_id, name))
                    }
                };
                format!("{indent}{member};\n")
            })
            .collect()
    }
}
