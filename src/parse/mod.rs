pub mod parser;

// pub mod pass_bytecode;
pub mod pass_unverified;
pub mod pass_molded;
pub mod pass_typecheck;

mod macros;
mod format;
mod value_type;
mod interface_serializer;

pub struct FileInformation {
    pub file_name: String,
    pub file_path: String,
}