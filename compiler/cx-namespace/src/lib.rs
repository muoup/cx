pub mod lookup;
pub mod module;
pub mod mangling;

pub fn cx_library_directory(inner_path: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{manifest_dir}/../../lib/{inner_path}")
}