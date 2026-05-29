use cx_ast::{
    ast::CXASTStmt,
    decomposition::CXGenerationStmt,
    symbols::{SymbolNamespaceData, UntypedSymbol, UntypedSymbolKind},
};
use cx_util::namespace::{NamespacePath, QualifiedName};

pub fn decompose_stmt(
    namespace: &NamespacePath,
    stmt: CXASTStmt,
    stmts: &mut Vec<CXGenerationStmt>,
    global_data: &mut SymbolNamespaceData,
) {
    match stmt {
        CXASTStmt::FunctionDefinition {
            prototype,
            visibility,
            template_prototype: None,
            body,
        } => {
            let symbol =
                UntypedSymbol::new(visibility, UntypedSymbolKind::Function(prototype.clone()));

            if let Some(body) = body {
                stmts.push(CXGenerationStmt::Function { prototype: prototype.clone(), body })
            }

            let QualifiedName { name, namespace } = prototype.kind.into_key();
            let full_namespace = namespace.join(&namespace);

            
        }

        _ => {}
    }
}
