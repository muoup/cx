use cx_ast::ast::CXAST;
use cx_log::CXResult;
use cx_preparse_data::{registry::GlobalPreparseRegistry, PreparseContents};
use cx_tokens::TokenIter;
use cx_util::namespace::NamespacePath;

use crate::{
    decomposition::DecompositionEnv,
    parse::{parse_global_stmt, parser::ParserData},
    preparse::{iterate_tokens, PreparseConfig, PreparseData},
};

pub(crate) mod decomposition;
pub(crate) mod log;
pub(crate) mod macros;

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
) -> CXResult<CXAST> {
    let mut data = ParserData::new(iter, pp_contents, registry);

    while data.tokens.has_next() {
        parse_global_stmt(&mut data)?;
    }

    Ok(data.take_ast())
}

pub fn decompose_ast<'a>(
    namespace: &'a NamespacePath,
    ast: CXAST,
) -> CXResult<DecompositionEnv<'a>> {
    let namespace_aliases = ast.namespace_aliases;
    let mut env = DecompositionEnv::new(namespace, namespace_aliases);

    for stmt in ast.definition_stmts {
        env.decompose_stmt(stmt)?;
    }
    Ok(env)
}
