use crate::log_error;
use crate::parse::ast::AST;
use crate::pipeline::CompilerPipeline;

pub(crate) fn import_file(ast: &mut AST, file: &str) -> Option<()> {
    let interface = get_interface(file)?;

    ast.statements.extend(interface.statements);

    Some(())
}

fn get_interface(file: &str) -> Option<AST> {
    let file = format!("{}.cx", file);

    let file_pipeline = CompilerPipeline::new(file.clone(), String::new());

    let Some(source) = file_pipeline.source_file() else {
        log_error!("File not found: {}", file);
    };
    let header = file_pipeline.find_previous_header();
    let object = file_pipeline.find_previous_object();

    if let Some(header) = header {
        if let Some(object) = object {
            let header_modified = header.metadata().unwrap().modified().unwrap();
            let object_modified = object.metadata().unwrap().modified().unwrap();
            let source_modified = source.metadata().unwrap().modified().unwrap();

            // The file has not been modified since the last compilation, so we can use the
            // previously generated interface
            if header_modified > source_modified || object_modified > source_modified {
                return CompilerPipeline::new(file.to_string(), String::new())
                    .preprocess()
                    .lex()
                    .parse_interface();
            }
        }
    }

    file_pipeline
        .preprocess()
        .lex()
        .parse()
        .codegen();

    CompilerPipeline::new(file.to_string(), String::new())
        .preprocess()
        .lex()
        .parse_interface()
}