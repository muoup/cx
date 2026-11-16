use chl_hir::ast::HIR;
use chl_log::CHLResult;
use chl_preparse_data::{PreparseContents, registry::GlobalPreparseRegistry};
use chl_tokens::TokenIter;
use chl_namespace::module::NamespacePath;

use crate::{
    parse::{parse_global_stmt, parser::ParserData},
    preparse::{PreparseConfig, PreparseData, iterate_tokens},
};

pub(crate) mod log;
pub(crate) mod macros;

pub mod parse;
pub mod preparse;

pub fn preparse(
    config: &PreparseConfig,
    tokens: TokenIter,
    module: String,
    path: NamespacePath,
) -> CHLResult<PreparseContents> {
    let mut contents = PreparseContents::new(module, path);
    for (alias, target) in config.scope.aliases() {
        contents.add_namespace_alias(alias, target);
    }

    let mut data = PreparseData {
        contents: &mut contents,
        config,
        tokens,
        visibility_mode: chl_preparse_data::VisibilityMode::Private,
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
) -> CHLResult<HIR> {
    let mut data = ParserData::new(iter, pp_contents, registry);

    while data.tokens.has_next() {
        parse_global_stmt(&mut data)?;
    }

    Ok(data.take_ast())
}
