use cx_hir::{
    ast::{
        HIRDefinition, HIRStmt,
        global_var::HIRGlobalVariable,
        template::{HIRTemplateInput, HIRTemplatePrototype},
    },
    symbols::{
        HIRFunctionSymbol, HIRSymbol, HIRSymbolData, HIRSymbolKind, HIRTypeConstructorSymbol, SymbolIdentifier, SymbolNamespaceData, TypeConstructorData
    },
};

use cx_hir::ast::types::{HIRType, HIRTypeKind, HIRTypeLookup};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_preparse_data::NamespaceAliases;

pub struct ExtractionEnv<'a> {
    namespace: &'a NamespacePath,
    symbol_buckets: Vec<(NamespacePath, SymbolNamespaceData)>,
    namespace_friends: Vec<(NamespacePath, NamespacePath)>,
}

impl<'a> ExtractionEnv<'a> {
    pub fn new(namespace: &'a NamespacePath, namespace_aliases: NamespaceAliases) -> Self {
        Self {
            namespace,
            symbol_buckets: vec![(
                namespace.clone(),
                SymbolNamespaceData::new_with_namespace_aliases(namespace_aliases),
            )],
            namespace_friends: Vec::new(),
        }
    }

    pub fn destructure(
        self,
    ) -> (
        Vec<(NamespacePath, SymbolNamespaceData)>,
        Vec<(NamespacePath, NamespacePath)>,
    ) {
        (self.symbol_buckets, self.namespace_friends)
    }

    pub fn get_bucket_mut(&mut self, namespace: &NamespacePath) -> &mut SymbolNamespaceData {
        if let Some(idx) = self
            .symbol_buckets
            .iter_mut()
            .position(|(bucket_namespace, _)| bucket_namespace == namespace)
        {
            return &mut self.symbol_buckets[idx].1;
        };

        if !namespace.is_root() && namespace.strip_prefix(self.namespace).is_none() {
            panic!(
                "Namespace {} is not a child of current namespace {}",
                namespace, self.namespace
            );
        };

        if !namespace.is_root() {
            let relation = (self.namespace.clone(), namespace.clone());
            if !self.namespace_friends.contains(&relation) {
                self.namespace_friends.push(relation);
            }
        }

        let data = SymbolNamespaceData::new();
        self.symbol_buckets.push((namespace.clone(), data));
        &mut self.symbol_buckets.last_mut().unwrap().1
    }

    pub fn extract_stmt(&mut self, definition: &HIRDefinition) {
        extract_from_stmt(self, definition)
    }
}

fn insert_symbol(
    env: &mut ExtractionEnv,
    namespace: &NamespacePath,
    identifier: SymbolIdentifier,
    symbol: HIRSymbol,
) {
    env.get_bucket_mut(namespace)
        .insert_symbol(identifier, symbol);
}

fn extract_from_stmt(env: &mut ExtractionEnv, definition: &HIRDefinition) {
    let stmt_namespace = definition.namespace.clone();

    let base_namespace = if stmt_namespace.is_root() {
        &stmt_namespace
    } else {
        env.namespace
    };

    match &definition.stmt {
        HIRStmt::TypeDefinition {
            name,
            visibility,
            template_prototype,
            _type,
            tag,
        } => {
            let Some(name) = name else {
                return;
            };

            let symbol_kind = HIRSymbolKind::Type(HIRSymbolData::new(
                _type.clone(),
                (),
                template_prototype.clone(),
            ));
            let symbol = HIRSymbol::new(*visibility, symbol_kind);
            let identifier = match tag {
                Some(kind) => SymbolIdentifier::Tag {
                    kind: *kind,
                    name: name.to_string(),
                },
                None => SymbolIdentifier::Standard(name.to_string()),
            };

            insert_symbol(env, base_namespace, identifier, symbol);

            if let HIRTypeKind::TaggedUnion { variants, .. } = &_type.kind {
                let union_name = QualifiedName::new(base_namespace.clone(), name.clone());
                let union_type = HIRTypeKind::Identifier {
                    name: union_name,
                    lookup: HIRTypeLookup::Standard,
                    template_input: template_prototype
                        .clone()
                        .map(convert_template_proto_to_args),
                }
                .to_type();
                let variant_namespace = base_namespace.child(name.clone());

                for (variant_index, variant) in variants.iter().enumerate() {
                    let Some((variant_name, _)) = variant.standard_parts() else {
                        continue;
                    };

                    let symbol = HIRSymbol::new(
                        *visibility,
                        HIRSymbolKind::TypeConstructor(match template_prototype.clone() {
                            Some(prototype) => HIRTypeConstructorSymbol::Template {
                                base: TypeConstructorData {
                                    union_type: union_type.clone(),
                                    variant_index,
                                },
                                template_data: (),
                                template_prototype: prototype,
                            },
                            None => HIRTypeConstructorSymbol::Standard {
                                base: TypeConstructorData {
                                    union_type: union_type.clone(),
                                    variant_index,
                                }
                            },
                        }),
                    );

                    insert_symbol(
                        env,
                        &variant_namespace,
                        SymbolIdentifier::Standard(variant_name.clone()),
                        symbol,
                    );
                }
            }
        }

        HIRStmt::FunctionDefinition {
            prototype,
            visibility,
            template_prototype,
            body,
        } => {
            let QualifiedName {
                name,
                namespace: q_namespace,
            } = prototype.kind.into_key();
            let namespace = base_namespace.join(&q_namespace);
            let symbol = match template_prototype {
                Some(input) => {
                    let Some(body) = body else {
                        return;
                    };

                    HIRSymbol::new(
                        *visibility,
                        HIRSymbolKind::Function(HIRFunctionSymbol::Template {
                            base: prototype.clone(),
                            template_data: body.clone(),
                            template_prototype: input.clone(),
                        })
                    )
                }
                None => HIRSymbol::new(
                    *visibility,
                    HIRSymbolKind::Function(HIRFunctionSymbol::Standard {
                        base: prototype.clone()
                    })
                )
            };

            insert_symbol(
                env,
                &namespace,
                SymbolIdentifier::Standard(name.to_string()),
                symbol,
            );
        }

        HIRStmt::ComptimeFunctionDefinition {
            prototype,
            visibility,
            template_prototype,
            body,
        } => {
            let QualifiedName {
                name,
                namespace: q_namespace,
            } = prototype.kind.into_key();
            let namespace = base_namespace.join(&q_namespace);
            let symbol = match template_prototype {
                Some(input) => HIRSymbol::new(
                    *visibility,
                    HIRSymbolKind::ComptimeFunction(HIRSymbolData::new(
                        prototype.clone(),
                        body.clone(),
                        Some(input.clone()),
                    )),
                ),
                None => HIRSymbol::new(
                    *visibility,
                    HIRSymbolKind::ComptimeFunction(HIRSymbolData::new(
                        prototype.clone(),
                        body.clone(),
                        None,
                    )),
                ),
            };

            insert_symbol(
                env,
                &namespace,
                SymbolIdentifier::Standard(name.to_string()),
                symbol,
            );
        }

        HIRStmt::GlobalVariableDefinition {
            visibility,
            variable,
        } => match &variable {
            HIRGlobalVariable::EnumDefinition(def) => {
                let bucket = env.get_bucket_mut(base_namespace);
                let e_idx = bucket.insert_enum_block(def.clone());

                for (v_idx, variant) in def.variants.iter().enumerate() {
                    let symbol = HIRSymbol::new(
                        *visibility,
                        HIRSymbolKind::EnumIdent {
                            enum_block_idx: e_idx,
                            variant_index: v_idx,
                        },
                    );

                    insert_symbol(
                        env,
                        base_namespace,
                        SymbolIdentifier::Standard(variant.name.as_string()),
                        symbol,
                    );
                }
            }

            HIRGlobalVariable::Standard {
                name,
                _type,
                symbol_name_scheme: symbol_naming,
                ..
            } => {
                let symbol = HIRSymbol::new(
                    *visibility,
                    HIRSymbolKind::AddressableGlobal {
                        name: name.clone(),
                        _type: _type.clone(),
                        symbol_naming: *symbol_naming,
                    },
                );

                insert_symbol(
                    env,
                    base_namespace,
                    SymbolIdentifier::Standard(name.to_string()),
                    symbol,
                );
            }
        },
    };
}

fn convert_template_proto_to_args(prototype: HIRTemplatePrototype) -> HIRTemplateInput {
    let params = prototype
        .types
        .into_iter()
        .map(|name| {
            HIRTypeKind::Identifier {
                name: QualifiedName::new_raw(name),
                lookup: HIRTypeLookup::Standard,
                template_input: None,
            }
            .to_type()
        })
        .collect::<Vec<HIRType>>();

    HIRTemplateInput { params }
}
