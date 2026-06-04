use cx_ast::{
    ast::{CXASTStmt, global_var::CXGlobalVariable},
    decomposition::CXGenerationStmt,
    symbols::{SymbolNamespaceData, CXSymbol, CXSymbolKind},
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
                Some(input) => CXSymbol::new(
                    visibility,
                    CXSymbolKind::TypeTemplate {
                        input,
                        definition: _type,
                    },
                ),
                None => CXSymbol::new(visibility, CXSymbolKind::Type(_type)),
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

                    CXSymbol::new(
                        visibility,
                        CXSymbolKind::FunctionTemplate {
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

                    CXSymbol::new(visibility, CXSymbolKind::Function(prototype))
                }
            };

            insert_symbol(symbol_buckets, full_namespace, name, symbol);
        }

        CXASTStmt::GlobalVariableDefinition {
            name,
            visibility,
            variable,
        } => match variable {
            CXGlobalVariable::EnumDefinition { variants } => {
                let mut i = 0;
                
                for variant in variants.into_iter() {
                    let symbol = CXSymbol::new(
                        visibility,
                        CXSymbolKind::Expression {
                            expr: variant.value,
                            is_constexpr: true,
                        },
                    );

                    insert_symbol(symbol_buckets, namespace.clone(), variant.name.clone(), symbol);
                }
            },
        },
    }
}

fn insert_symbol(
    symbol_buckets: &mut Vec<(NamespacePath, SymbolNamespaceData)>,
    namespace: NamespacePath,
    name: CXIdent,
    symbol: CXSymbol,
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
