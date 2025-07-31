use std::fs::File;
use crate::{CompilationUnit, GlobalCompilationContext};
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::path::PathBuf;
use crate::directories::internal_directory;

pub fn resource_path(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) -> PathBuf {
    let mut path = internal_directory(context, unit.to_path());
    path.push(data_suffix);
    path
}

pub fn store_text(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str, text: &str) {
    let path = resource_path(context, unit, data_suffix);

    std::fs::write(path, text).expect("Failed to write text to file");
}

pub fn store_data<Data>(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str, data: Data)
    where Data: Serialize + DeserializeOwned
{
    let path = resource_path(context, unit, data_suffix);

    serde_json::to_writer_pretty(
        File::create(&path).expect("Failed to create file for writing"),
        &data
    ).expect("Failed to serialize data to JSON");
}

pub fn retrieve_text(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) -> std::io::Result<String> {
    let path = resource_path(context, unit, data_suffix);

    std::fs::read_to_string(&path)
}

pub fn retrieve_data<Data>(context: &GlobalCompilationContext, unit: &CompilationUnit, data_suffix: &str) -> Option<Data>
    where Data: Serialize + DeserializeOwned
{
    let path = resource_path(context, unit, data_suffix);
    let reader = File::open(&path)
        .expect("Failed to open file for reading");

    Some(
        serde_json::from_reader::<File, Data>(reader)
            .expect("Serialized data failure")
    )
}