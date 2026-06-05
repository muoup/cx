use cx_ast::{
    ast::{global_var::CXGlobalVariable, CXASTStmt},
    decomposition::{CXGenerationAST, CXGenerationStmt},
    symbols::{CXSymbol, CXSymbolKind, SymbolNamespaceData},
};
use std::collections::HashMap;

use cx_util::namespace::{NamespacePath, QualifiedName};

pub struct DecompositionEnv<'a> {
    namespace: &'a NamespacePath,
    namespace_aliases: HashMap<NamespacePath, NamespacePath>,
    symbol_buckets: Vec<(NamespacePath, SymbolNamespaceData)>,
    stmts: Vec<CXGenerationStmt>,
}

impl<'a> DecompositionEnv<'a> {
    pub fn new(
        namespace: &'a NamespacePath,
        namespace_aliases: HashMap<NamespacePath, NamespacePath>,
    ) -> Self {
        Self {
            namespace,
            namespace_aliases,
            symbol_buckets: Vec::new(),
            stmts: Vec::new(),
        }
    }

    pub fn destructure(self) -> (Vec<(NamespacePath, SymbolNamespaceData)>, CXGenerationAST) {
        let ast = CXGenerationAST {
            namespace_aliases: self.namespace_aliases,
            generation_stmts: self.stmts,
        };

        (self.symbol_buckets, ast)
    }

    pub fn get_bucket_mut(&mut self, namespace: &NamespacePath) -> &mut SymbolNamespaceData {
        if let Some(idx) = self
            .symbol_buckets
            .iter_mut()
            .position(|(bucket_namespace, _)| bucket_namespace == namespace)
        {
            return &mut self.symbol_buckets[idx].1;
        };

        let data = SymbolNamespaceData::new();
        self.symbol_buckets.push((namespace.clone(), data));
        &mut self.symbol_buckets.last_mut().unwrap().1
    }

    pub fn decompose_stmt(&mut self, stmt: CXASTStmt) {
        decompose_stmt(self, stmt);
    }
}

fn decompose_stmt(env: &mut DecompositionEnv, stmt: CXASTStmt) {
    match stmt {
        CXASTStmt::TypeDefinition {
            name,
            visibility,
            template_prototype,
            _type,
        } => {
            let Some(name) = name else {
                return;
            };

            let symbol = match template_prototype {
                Some(input) => CXSymbol::new(
                    visibility,
                    CXSymbolKind::TypeTemplate {
                        template: input,
                        definition: _type,
                    },
                ),
                None => CXSymbol::new(visibility, CXSymbolKind::Type(_type)),
            };

            env.get_bucket_mut(env.namespace)
                .insert_symbol(name.to_string(), symbol);
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
            let namespace = env.namespace.join(&q_namespace);
            let symbol = match template_prototype {
                Some(input) => {
                    let Some(body) = body else {
                        return;
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
                    if let Some(body) = body {
                        env.stmts.push(CXGenerationStmt::Function {
                            prototype: prototype.clone(),
                            body,
                        })
                    }

                    CXSymbol::new(visibility, CXSymbolKind::FunctionReference(prototype))
                }
            };

            env.get_bucket_mut(&namespace)
                .insert_symbol(name.to_string(), symbol);
        }

        CXASTStmt::GlobalVariableDefinition {
            visibility,
            variable,
        } => match &variable {
            CXGlobalVariable::EnumDefinition(def) => {
                let bucket = env.get_bucket_mut(env.namespace);
                let e_idx = bucket.insert_enum_block(def.clone());

                for (v_idx, variant) in def.variants.iter().enumerate() {
                    let symbol = CXSymbol::new(
                        visibility,
                        CXSymbolKind::EnumIdent {
                            enum_block_idx: e_idx,
                            variant_index: v_idx,
                        },
                    );

                    bucket.insert_symbol(variant.name.as_string(), symbol);
                }
            }

            CXGlobalVariable::Standard { name, _type, is_mutable: _, linkage, initializer } => {
                let symbol = CXSymbol::new(
                    visibility,
                    CXSymbolKind::AddressableGlobal(name.clone(), _type.clone()),
                );

                env.get_bucket_mut(env.namespace)
                    .insert_symbol(name.to_string(), symbol);

                env.stmts.push(CXGenerationStmt::AddressableGlobal {
                    name: name.clone(),
                    _type: _type.clone(),
                    linkage: *linkage,
                    initializer: initializer.clone(),
                });
            }
        },
    }
}
