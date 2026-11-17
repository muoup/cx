use crate::pipeline_error;
use chl_log::CHLResult;
use chl_log::catalogue::driver as catalogue;
use chl_pipeline_data::config::LinkEntry;
use object::write::Object;
use object::{Object as _, ObjectSymbol, SectionFlags, SectionKind, elf};
use std::path::Path;

pub(crate) fn defined_globals(path: &Path) -> CHLResult<Vec<String>> {
    let data = read(path)?;
    let file = parse(path, &data)?;
    Ok(file
        .symbols()
        .filter(|symbol| symbol.is_global() && !symbol.is_undefined())
        .filter_map(|symbol| symbol.name().ok().filter(|name| !name.is_empty()).map(str::to_string))
        .collect())
}

pub(crate) fn write_dependent_libraries(output: &Path, like: &Path, links: &[LinkEntry]) -> CHLResult<()> {
    let data = read(like)?;
    let file = parse(like, &data)?;
    let mut object = Object::new(file.format(), file.architecture(), file.endianness());
    let section = object.add_section(Vec::new(), b".deplibs".to_vec(), SectionKind::Elf(elf::SHT_LLVM_DEPENDENT_LIBRARIES));
    object.section_mut(section).flags = SectionFlags::Elf { sh_flags: (elf::SHF_MERGE | elf::SHF_STRINGS) as u64 };
    let mut contents = Vec::new();
    for link in links {
        contents.extend(link.name.as_bytes());
        contents.push(0);
    }
    object.set_section_data(section, contents, 1);
    let bytes = object
        .write()
        .map_err(|error| failure("write", output, error.to_string()))?;
    std::fs::write(output, bytes).map_err(|error| failure("write", output, error.to_string()))
}

fn read(path: &Path) -> CHLResult<Vec<u8>> {
    std::fs::read(path).map_err(|error| failure("read", path, error.to_string()))
}

fn parse<'a>(path: &Path, data: &'a [u8]) -> CHLResult<object::File<'a>> {
    object::File::parse(data).map_err(|error| failure("parse", path, error.to_string()))
}

fn failure(operation: &str, path: &Path, error: String) -> chl_log::error::CHLError {
    pipeline_error(
        &catalogue::FILE_OPERATION,
        (operation.into(), "object file".into(), Some(format!("{}", path.display())), error),
    )
}
