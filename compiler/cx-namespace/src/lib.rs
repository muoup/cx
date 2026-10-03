pub mod lookup;
pub mod module;
pub mod mangling;

use std::{
    path::{Path, PathBuf},
    sync::OnceLock,
};

// Searched in order: '$CX_HOME/lib', a 'lib' directory beside the executable or one level above
// it, then the source checkout the compiler was built from
fn library_root() -> &'static Path {
    static ROOT: OnceLock<PathBuf> = OnceLock::new();
    ROOT.get_or_init(|| {
        if let Some(home) = std::env::var_os("CX_HOME") {
            return PathBuf::from(home).join("lib");
        }
        let installed = std::env::current_exe().ok().and_then(|exe| {
            exe.parent()?
                .ancestors()
                .take(2)
                .map(|directory| directory.join("lib"))
                .find(|lib| lib.join("std").is_dir() && lib.join("libc").is_dir())
        });
        installed.unwrap_or_else(|| {
            let checkout = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../lib");
            checkout.canonicalize().unwrap_or(checkout)
        })
    })
}

pub fn cx_library_directory(inner_path: &str) -> String {
    library_root().join(inner_path).to_string_lossy().into_owned()
}
