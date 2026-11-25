use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use mlua::{Lua, Result};

use super::values::fail;
use crate::config::schema::{
    DEBUG_PROFILE, IncludeSpec, LibraryConfig, LinkEntry, Mount, ProfileSettings, TargetKind,
};

pub(super) type Shared = Rc<RefCell<State>>;

#[derive(Default)]
pub(super) struct State {
    pub stack: Vec<usize>,
    pub packages: Vec<PackageDraft>,
    pub targets: Vec<TargetDraft>,
}

pub(super) struct PackageDraft {
    pub script: PathBuf,
    pub name: Option<String>,
    pub source_root: Option<PathBuf>,
    pub mounts: Vec<Mount>,
    pub deps: Vec<usize>,
    pub library: Option<LibraryConfig>,
    pub includes: Vec<IncludeSpec>,
    pub defines: Vec<(String, String)>,
    pub objects: Vec<PathBuf>,
    pub links: Vec<LinkEntry>,
    pub profiles: Vec<ProfileDraft>,
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
    pub ffi_includes: Vec<FfiIncludeDraft>,
}

pub(super) struct FfiIncludeDraft {
    pub package: usize,
    pub name: String,
    pub prefix: Option<String>,
}

impl PackageDraft {
    pub fn new(script: PathBuf) -> Self {
        Self {
            script,
            name: None,
            source_root: None,
            mounts: Vec::new(),
            deps: Vec::new(),
            library: None,
            includes: Vec::new(),
            defines: Vec::new(),
            objects: Vec::new(),
            links: Vec::new(),
            profiles: vec![ProfileDraft {
                name: DEBUG_PROFILE.into(),
                base: None,
                settings: ProfileSettings::default(),
            }],
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
        self.packages[self.current()]
            .script
            .parent()
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
