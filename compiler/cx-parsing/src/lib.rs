#[macro_use]
mod log;

pub mod parse;
pub mod preparse;

pub use log::ParseErrorLog;

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

pub fn decompose_ast(ast: CXAST) -> ! {
    todo!()
}