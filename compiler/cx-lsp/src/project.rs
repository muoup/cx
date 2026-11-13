use cx_pipeline::project::ProjectPackages;
use cx_pipeline_data::config::{load_project, CONFIG_FILE, DEBUG_PROFILE};
use cx_pipeline_data::package::{Package, PackageId, Packages};
use cx_pipeline_data::CompilationUnit;
use std::path::{Path, PathBuf};

pub struct ProjectSettings {
    pub root: PathBuf,
    pub packages: Packages,
    fallback: PackageId,
}

impl ProjectSettings {
    pub fn new(root: &Path) -> Self {
        let (packages, fallback) = Packages::standalone(root.to_path_buf(), vec![], vec![]);
        Self {
            root: root.to_path_buf(),
            packages,
            fallback,
        }
    }

    pub fn package(&self, file: &Path) -> &Package {
        self.packages.get(self.owner(file))
    }

    pub fn unit(&self, file: &Path) -> (Packages, CompilationUnit) {
        if let Some(unit) = self.packages.unit(self.owner(file), file) {
            return (self.packages.clone(), unit);
        }
        let directory = file.parent().unwrap_or(file).to_path_buf();
        let (packages, package) = Packages::standalone(directory, vec![], vec![]);
        let unit = packages
            .unit(package, file)
            .expect("a file lies under its own directory");
        (packages, unit)
    }

    fn owner(&self, file: &Path) -> PackageId {
        self.packages.owner(file).unwrap_or(self.fallback)
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
    if !project_root.join(CONFIG_FILE).is_file() {
        return Ok(ProjectSettings::new(project_root));
    }

    let project = load_project(project_root).map_err(|error| error.message())?;
    let profile = project
        .profile(DEBUG_PROFILE)
        .expect("every project has the debug profile");
    let packages =
        ProjectPackages::new(&project, &profile.settings).map_err(|error| error.message())?;
    Ok(ProjectSettings {
        root: project_root.to_path_buf(),
        fallback: packages.id(0),
        packages: packages.packages,
    })
}
