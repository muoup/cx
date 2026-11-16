use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use mlua::{Lua, Result, Table};

use super::values::fail;
use crate::config::schema::{
    DEBUG_PROFILE, Mount, ProfileSettings, TargetKind, Usage,
};

pub(super) type Shared = Rc<RefCell<State>>;

#[derive(Default)]
pub(super) struct State {
    pub files: Vec<PathBuf>,
    pub stack: Vec<usize>,
    pub packages: Vec<PackageDraft>,
    pub targets: Vec<TargetDraft>,
}

pub(super) struct PackageDraft {
    pub script: PathBuf,
    pub name: Option<String>,
    pub source_root: Option<PathBuf>,
    pub mounts: Vec<Mount>,
    pub usage: Usage,
    pub c_units: Vec<String>,
    pub profiles: Vec<ProfileDraft>,
    pub selectable: Vec<usize>,
    pub exported: Vec<usize>,
    pub helpers: Table,
}

pub(super) struct ProfileDraft {
    pub name: String,
    pub base: Option<usize>,
    pub settings: ProfileSettings,
}

pub(super) struct TargetDraft {
    pub name: String,
    pub kind: TargetKind,
    pub package: usize,
    pub matches: Vec<String>,
    pub excludes: Vec<String>,
    pub headers: Vec<String>,
}

impl PackageDraft {
    pub fn new(script: PathBuf, helpers: Table) -> Self {
        Self {
            script,
            name: None,
            source_root: None,
            mounts: Vec::new(),
            usage: Usage::default(),
            c_units: Vec::new(),
            profiles: vec![ProfileDraft {
                name: DEBUG_PROFILE.into(),
                base: None,
                settings: ProfileSettings::default(),
            }],
            selectable: Vec::new(),
            exported: Vec::new(),
            helpers,
        }
    }

    pub fn label(&self) -> String {
        match &self.name {
            Some(name) => format!("package '{name}'"),
            None => format!("'{}'", self.script.display()),
        }
    }
}

impl State {
    pub fn directory(&self) -> PathBuf {
        self.files
            .last()
            .and_then(|file| file.parent())
            .map(Path::to_path_buf)
            .unwrap_or_default()
    }

    pub fn current(&self) -> usize {
        *self.stack.last().expect("a build script is being evaluated")
    }

    pub fn package(&mut self) -> &mut PackageDraft {
        let current = self.current();
        &mut self.packages[current]
    }

    pub fn owned_target(&self, lua: &Lua, index: usize) -> Result<&TargetDraft> {
        let target = &self.targets[index];
        if target.package != self.current() {
            return Err(fail(
                lua,
                format!("target '{}' belongs to another package and cannot be changed here", target.name),
            ));
        }
        Ok(target)
    }
}
