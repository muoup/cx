mod lua;
mod schema;

use std::path::{Path, PathBuf};

use chl_log::{
    CHLResult,
    catalogue::driver as catalogue,
    error::{CHLError, context::CHLNoSourceContext},
};

pub use schema::*;

pub const CONFIG_FILE: &str = "chl.lua";

pub fn find_project_root(start: &Path) -> Option<PathBuf> {
    start
        .ancestors()
        .find(|directory| directory.join(CONFIG_FILE).is_file())
        .map(Path::to_path_buf)
}

pub fn load_project(root: &Path) -> CHLResult<ProjectConfig> {
    lua::evaluate(&root.join(CONFIG_FILE)).map_err(|message| {
        CHLError::new(
            catalogue::INVALID_CONFIG.bind(message),
            CHLNoSourceContext::error("project configuration could not be evaluated"),
        )
    })
}
