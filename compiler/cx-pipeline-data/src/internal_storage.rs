use crate::directories::internal_directory;
use crate::{CompilationUnit, GlobalCompilationContext};
use speedy::{LittleEndian, Readable, Writable};
use std::path::PathBuf;

pub fn resource_path(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) -> PathBuf {
    let mut path = internal_directory(context, unit);
    path.push(data_suffix);
    path
}

pub fn store_text(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str, text: &str) {
    let path = resource_path(context, unit, data_suffix);

    std::fs::write(path, text).expect("Failed to write text to file");
}

pub fn store_data<'a, Data>(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str, data: Data)
    -> Option<()>
where 
    Data: Writable<LittleEndian>
{
    let path = resource_path(context, unit, data_suffix);
    
    data.write_to_file(path.as_path()).ok()
}

pub fn retrieve_text(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) -> std::io::Result<String> {
    let path = resource_path(context, unit, data_suffix);

    std::fs::read_to_string(&path)
}

pub fn retrieve_data<'a, Data>(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) 
    -> Option<Data>
where 
    Data: Readable<'a, LittleEndian>
{
    let path = resource_path(context, unit, data_suffix);

    Data::read_from_file(path).ok()
}