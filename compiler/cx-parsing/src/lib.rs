use cx_hir::ast::HIR;
use cx_hir::symbols::SymbolNamespaceData;
use cx_log::CXResult;
use cx_preparse_data::{PreparseContents, registry::GlobalPreparseRegistry};
use cx_tokens::TokenIter;
use cx_namespace::module::NamespacePath;

use crate::{
    extraction::ExtractionEnv,
    parse::{parse_global_stmt, parser::ParserData},
    preparse::{PreparseConfig, PreparseData, iterate_tokens},
};

pub(crate) mod extraction;
pub(crate) mod log;
pub(crate) mod macros;

pub struct HIRSymbolExtraction {
    pub symbol_buckets: Vec<(NamespacePath, SymbolNamespaceData)>,
    pub namespace_friends: Vec<(NamespacePath, NamespacePath)>,
}

pub mod parse;
pub mod preparse;

pub fn preparse(
    config: &PreparseConfig,
    tokens: TokenIter,
    module: String,
    path: NamespacePath,
) -> CXResult<PreparseContents> {
    let mut contents = PreparseContents::new(module, path);

    let mut data = PreparseData {
        contents: &mut contents,
        config,
        tokens,
        visibility_mode: cx_preparse_data::VisibilityMode::Private,
        include_states: Vec::new(),
    };

    while data.tokens.has_next() {
        iterate_tokens(&mut data)?;
    }

    Ok(contents)
}

pub fn parse_ast(
    iter: TokenIter,
    pp_contents: &PreparseContents,
    registry: &GlobalPreparseRegistry,
) -> CXResult<HIR> {
    let mut data = ParserData::new(iter, pp_contents, registry);

    while data.tokens.has_next() {
        parse_global_stmt(&mut data)?;
    }

    Ok(data.take_ast())
}

pub fn ast_extract_symbols(namespace: &NamespacePath, ast: &HIR) -> HIRSymbolExtraction {
    let namespace_aliases = ast.namespace_aliases.clone();
    let mut env = ExtractionEnv::new(namespace, namespace_aliases);

    for definition in &ast.definition_stmts {
        env.extract_stmt(definition);
    }

    let (symbol_buckets, namespace_friends) = env.destructure();
    HIRSymbolExtraction {
        symbol_buckets,
        namespace_friends,
    }
}
