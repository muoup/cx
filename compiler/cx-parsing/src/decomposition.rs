use cx_ast::{
    ast::{CXASTStmt, global_var::CXGlobalVariable},
    decomposition::CXGenerationStmt,
    symbols::{SymbolNamespaceData, UntypedSymbol, UntypedSymbolKind},
};
use cx_util::{
    identifier::CXIdent,
    namespace::{NamespacePath, QualifiedName},
};

pub fn decompose_stmt(
    namespace: &NamespacePath,
    stmt: CXASTStmt,
    stmts: &mut Vec<CXGenerationStmt>,
    symbol_buckets: &mut Vec<(NamespacePath, SymbolNamespaceData)>,
) {
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
                Some(input) => UntypedSymbol::new(
                    visibility,
                    UntypedSymbolKind::TypeTemplate {
                        input,
                        definition: _type,
                    },
                ),
                None => UntypedSymbol::new(visibility, UntypedSymbolKind::Type(_type)),
            };

            insert_symbol(symbol_buckets, namespace.clone(), name, symbol);
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
            let full_namespace = namespace.join(&q_namespace);
            let symbol = match template_prototype {
                Some(input) => {
                    let Some(body) = body else {
                        return;
                    };

                    UntypedSymbol::new(
                        visibility,
                        UntypedSymbolKind::FunctionTemplate {
                            input,
                            definition: prototype,
                            body,
                        },
                    )
                }
                None => {
                    if let Some(body) = body {
                        stmts.push(CXGenerationStmt::Function {
                            prototype: prototype.clone(),
                            body,
                        })
                    }

                    UntypedSymbol::new(visibility, UntypedSymbolKind::Function(prototype))
                }
            };

            insert_symbol(symbol_buckets, full_namespace, name, symbol);
        }

        CXASTStmt::GlobalVariableDefinition {
            name,
            visibility,
            variable,
        } => {
            if let CXGlobalVariable::Standard {
                initializer,
                linkage,
                ..
            } = &variable
            {
                stmts.push(CXGenerationStmt::AddressableGlobal {
                    name: namespace.as_flat_name_with(&name),
                    initializer: initializer.clone(),
                    linkage: *linkage,
                });
            }

            insert_symbol(
                symbol_buckets,
                namespace.clone(),
                name,
                UntypedSymbol::new(visibility, UntypedSymbolKind::Global(variable)),
            );
        }
    }
}

fn insert_symbol(
    symbol_buckets: &mut Vec<(NamespacePath, SymbolNamespaceData)>,
    namespace: NamespacePath,
    name: CXIdent,
    symbol: UntypedSymbol,
) {
    if let Some((_, data)) = symbol_buckets
        .iter_mut()
        .find(|(bucket_namespace, _)| bucket_namespace == &namespace)
    {
        data.insert_symbol(name.as_string(), symbol);
        return;
    }

    let mut data = SymbolNamespaceData::new();
    data.insert_symbol(name.as_string(), symbol);
    symbol_buckets.push((namespace, data));
}
