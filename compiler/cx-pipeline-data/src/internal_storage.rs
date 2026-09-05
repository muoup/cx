use crate::{CompilationUnit, GlobalCompilationContext};
use speedy::{LittleEndian, Readable, Writable};
use std::path::PathBuf;

pub fn resource_path(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
) -> PathBuf {
    let diff = unit.module().as_path()
        .strip_prefix(&context.config.working_directory)
        .unwrap_or(unit.module().as_path())
        .with_extension(data_suffix);

    let complete_path = context.config.internal_directory.join(diff);
    return complete_path;
}

pub fn store_text(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
    text: &str,
) {
    let path = resource_path(context, unit, data_suffix);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("Failed to create parent directory for text data");
    }

    std::fs::write(path, text).expect("Failed to write text to file");
}

pub fn store_data<Data>(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
    data: Data,
) -> Option<()>
where
    Data: Writable<LittleEndian>,
{
    let path = resource_path(context, unit, data_suffix);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).ok()?;
    }

    data.write_to_file(path.as_path()).ok()
}

pub fn retrieve_text(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
) -> std::io::Result<String> {
    let path = resource_path(context, unit, data_suffix);

    std::fs::read_to_string(&path)
}

pub fn retrieve_data<'a, Data>(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
) -> Option<Data>
where
    Data: Readable<'a, LittleEndian>,
{
    let path = resource_path(context, unit, data_suffix);

    Data::read_from_file(path).ok()
}
