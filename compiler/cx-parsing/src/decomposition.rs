use cx_ast::{
    ast::{
        CXASTDefinition, CXASTStmt,
        global_var::CXGlobalVariable,
        template::{CXTemplateInput, CXTemplatePrototype},
    },
    decomposition::{CXGenerationAST, CXGenerationStmt},
    symbols::{CXSymbol, CXSymbolKind, SymbolNamespaceData},
};

use cx_ast::ast::types::{CXType, CXTypeKind, PredeclarationType};
use cx_log::CXResult;
use cx_preparse_data::NamespaceAliases;
use cx_util::namespace::{NamespacePath, QualifiedName};

pub struct DecompositionEnv<'a> {
    namespace: &'a NamespacePath,
    symbol_buckets: Vec<(NamespacePath, SymbolNamespaceData)>,
    namespace_friends: Vec<(NamespacePath, NamespacePath)>,
    stmts: Vec<CXGenerationStmt>,
}

impl<'a> DecompositionEnv<'a> {
    pub fn new(namespace: &'a NamespacePath, namespace_aliases: NamespaceAliases) -> Self {
        Self {
            namespace,
            symbol_buckets: vec![(
                namespace.clone(),
                SymbolNamespaceData::new_with_namespace_aliases(namespace_aliases),
            )],
            namespace_friends: Vec::new(),
            stmts: Vec::new(),
        }
    }

    pub fn destructure(
        self,
    ) -> (
        Vec<(NamespacePath, SymbolNamespaceData)>,
        Vec<(NamespacePath, NamespacePath)>,
        CXGenerationAST,
    ) {
        let ast = CXGenerationAST {
            generation_stmts: self.stmts,
        };

        (self.symbol_buckets, self.namespace_friends, ast)
    }

    pub fn get_bucket_mut(&mut self, namespace: &NamespacePath) -> &mut SymbolNamespaceData {
        if let Some(idx) = self
            .symbol_buckets
            .iter_mut()
            .position(|(bucket_namespace, _)| bucket_namespace == namespace)
        {
            return &mut self.symbol_buckets[idx].1;
        };

        if !namespace.is_root() && namespace.strip(self.namespace).is_none() {
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

    pub fn decompose_stmt(&mut self, definition: CXASTDefinition) -> CXResult<()> {
        decompose_stmt(self, definition)
    }
}

fn insert_symbol(
    env: &mut DecompositionEnv,
    namespace: &NamespacePath,
    name: impl Into<String>,
    symbol: CXSymbol,
) -> CXResult<()> {
    let name = name.into();

    if let Some(existing) = env
        .get_bucket_mut(namespace)
        .get_symbol(name.as_str())
        .cloned()
    {
        let mut definitions = match existing.kind {
            CXSymbolKind::DuplicateDefinition(definitions) => definitions,
            kind => vec![kind],
        };
        definitions.push(symbol.kind);

        env.get_bucket_mut(namespace).insert_symbol(
            name,
            CXSymbol::new(
                existing.visibility,
                CXSymbolKind::DuplicateDefinition(definitions),
            ),
        );
        return Ok(());
    }

    env.get_bucket_mut(namespace).insert_symbol(name, symbol);
    Ok(())
}

fn decompose_stmt(env: &mut DecompositionEnv, definition: CXASTDefinition) -> CXResult<()> {
    let stmt_namespace = definition.namespace;

    let base_namespace = if stmt_namespace.is_root() {
        &stmt_namespace
    } else {
        env.namespace
    };

    match definition.stmt {
        CXASTStmt::TypeDefinition {
            name,
            visibility,
            template_prototype,
            _type,
        } => {
            let Some(name) = name else {
                return Ok(());
            };

            let symbol = match template_prototype.clone() {
                Some(input) => CXSymbol::new(
                    visibility,
                    CXSymbolKind::TypeTemplate {
                        template: input,
                        definition: _type.clone(),
                    },
                ),
                None => CXSymbol::new(visibility, CXSymbolKind::Type(_type.clone())),
            };

            insert_symbol(env, base_namespace, name.to_string(), symbol)?;

            if let CXTypeKind::TaggedUnion { variants, .. } = &_type.kind {
                let union_name = QualifiedName::new(base_namespace.clone(), name.clone());
                let union_type = CXTypeKind::Identifier {
                    name: union_name,
                    predeclaration: PredeclarationType::None,
                    template_input: template_prototype
                        .clone()
                        .map(convert_template_proto_to_args),
                }
                .to_type();
                let variant_namespace = base_namespace.child(name);

                for (variant_index, variant) in variants.iter().enumerate() {
                    let Some((variant_name, _)) = variant.standard_parts() else {
                        continue;
                    };

                    let symbol = CXSymbol::new(
                        visibility,
                        CXSymbolKind::TypeConstructor {
                            template: template_prototype.clone(),
                            union_type: union_type.clone(),
                            variant_index,
                        },
                    );

                    insert_symbol(env, &variant_namespace, variant_name.clone(), symbol)?;
                }
            }
        }

        CXASTStmt::FunctionDefinition {
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
            let mut generated_body = None;
            let symbol = match template_prototype {
                Some(input) => {
                    let Some(body) = body else {
                        return Ok(());
                    };

                    CXSymbol::new(
                        visibility,
                        CXSymbolKind::FunctionTemplate {
                            template: input,
                            definition: prototype,
                            body,
                        },
                    )
                }
                None => {
                    generated_body = body.map(|body| (prototype.clone(), body));

                    CXSymbol::new(visibility, CXSymbolKind::FunctionReference(prototype))
                }
            };

            insert_symbol(env, &namespace, name.to_string(), symbol)?;

            if let Some((prototype, body)) = generated_body {
                env.stmts
                    .push(CXGenerationStmt::Function { prototype, body });
            }
        }

        CXASTStmt::GlobalVariableDefinition {
            visibility,
            variable,
        } => match &variable {
            CXGlobalVariable::EnumDefinition(def) => {
                let bucket = env.get_bucket_mut(base_namespace);
                let e_idx = bucket.insert_enum_block(def.clone());

                for (v_idx, variant) in def.variants.iter().enumerate() {
                    let symbol = CXSymbol::new(
                        visibility,
                        CXSymbolKind::EnumIdent {
                            enum_block_idx: e_idx,
                            variant_index: v_idx,
                        },
                    );

                    insert_symbol(env, base_namespace, variant.name.as_string(), symbol)?;
                }
            }

            CXGlobalVariable::Standard {
                name,
                _type,
                is_mutable: _,
                linkage,
                initializer,
            } => {
                let symbol = CXSymbol::new(
                    visibility,
                    CXSymbolKind::AddressableGlobal(name.clone(), _type.clone()),
                );

                insert_symbol(env, base_namespace, name.to_string(), symbol)?;

                env.stmts.push(CXGenerationStmt::AddressableGlobal {
                    name: name.clone(),
                    _type: _type.clone(),
                    linkage: *linkage,
                    initializer: initializer.clone(),
                });
            }
        },
    };

    Ok(())
}

fn convert_template_proto_to_args(prototype: CXTemplatePrototype) -> CXTemplateInput {
    let params = prototype
        .types
        .into_iter()
        .map(|name| {
            CXTypeKind::Identifier {
                name: QualifiedName::new_raw(name),
                predeclaration: PredeclarationType::None,
                template_input: None,
            }
            .to_type()
        })
        .collect::<Vec<CXType>>();

    CXTemplateInput { params }
}
