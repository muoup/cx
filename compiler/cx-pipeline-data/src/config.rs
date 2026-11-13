mod lua;
mod schema;

use std::path::{Path, PathBuf};

use cx_log::{
    CXResult,
    catalogue::driver as catalogue,
    error::{CXError, context::CXNoSourceContext},
};

pub use schema::*;

pub const CONFIG_FILE: &str = "cx.lua";

pub fn find_project_root(start: &Path) -> Option<PathBuf> {
    start
        .ancestors()
        .find(|directory| directory.join(CONFIG_FILE).is_file())
        .map(Path::to_path_buf)
}

pub fn load_project(root: &Path) -> CXResult<ProjectConfig> {
    lua::evaluate(&root.join(CONFIG_FILE)).map_err(|message| {
        CXError::new(
            catalogue::INVALID_CONFIG.bind(message),
            CXNoSourceContext::error("project configuration could not be evaluated"),
        )
    })
}
