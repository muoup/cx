use crate::{CompilationUnit, GlobalCompilationContext};
use speedy::{LittleEndian, Readable, Writable};
use std::path::PathBuf;

pub fn resource_path(
    context: &GlobalCompilationContext,
    unit: &CompilationUnit,
    data_suffix: &str,
) -> PathBuf {
    let relative = if unit.namespace().is_root() {
        PathBuf::from(unit.module().as_path().file_name().expect("source file name"))
    } else {
        unit.namespace().segments().iter().map(|segment| segment.as_str()).collect()
    };
    context.config.internal_directory.join(relative).with_extension(data_suffix.trim_start_matches('.'))
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
