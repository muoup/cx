use cx_pipeline::project;
use cx_pipeline_data::config::{load_project, Profile, CONFIG_FILE};
use std::path::{Path, PathBuf};

pub struct ProjectSettings {
    pub root: PathBuf,
    pub source_root: PathBuf,
    pub include_dirs: Vec<PathBuf>,
    pub predefined_macros: Vec<(String, String)>,
}

impl ProjectSettings {
    pub fn new(root: &Path) -> Self {
        Self {
            root: root.to_path_buf(),
            source_root: root.to_path_buf(),
            include_dirs: vec![],
            predefined_macros: vec![],
        }
    }
}

pub fn find_project_root(file_path: &Path) -> PathBuf {
    let mut current = file_path.parent().unwrap_or(file_path);

    let mut search = current;
    loop {
        if search.join(CONFIG_FILE).is_file() {
            return search.to_path_buf();
        }
        match search.parent() {
            Some(parent) if parent != search => search = parent,
            _ => break,
        }
    }

    loop {
        if current.join(".internal").is_dir() || current.join(".git").is_dir() {
            return current.to_path_buf();
        }

        match current.parent() {
            Some(parent) if parent != current => current = parent,
            _ => {
                return file_path
                    .parent()
                    .unwrap_or_else(|| Path::new("/"))
                    .to_path_buf();
            }
        }
    }
}

pub fn load_project_settings(project_root: &Path) -> Result<ProjectSettings, String> {
    let mut settings = ProjectSettings::new(project_root);
    if !project_root.join(CONFIG_FILE).is_file() {
        return Ok(settings);
    }

    let project = load_project(project_root).map_err(|error| error.message())?;
    let internal_directory = project_root.join(".internal");
    settings.source_root = project.source_root.clone();
    for target in &project.targets {
        let usage = project::target_usage(&project, &internal_directory, Profile::Debug, target)
            .map_err(|error| error.message())?;
        for include_dir in usage.include {
            if !settings.include_dirs.contains(&include_dir) {
                settings.include_dirs.push(include_dir);
            }
        }
        for define in usage.defines {
            if !settings.predefined_macros.iter().any(|(name, _)| *name == define.0) {
                settings.predefined_macros.push(define);
            }
        }
    }
    Ok(settings)
}
