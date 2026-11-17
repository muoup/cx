use std::path::{Path, PathBuf};

use chl_lexer::IncludeRoot;
use chl_namespace::{
    chl_library_directory,
    module::{ModulePath, NamespacePath},
    scope::ImportScope,
};
use chl_util::identifier::CHLIdent;
use rustc_hash::{FxHashMap, FxHashSet};
use speedy::{Readable, Writable};

use crate::CompilationUnit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Readable, Writable)]
pub struct PackageId(u32);

impl PackageId {
    pub const STD: Self = Self(0);
}

#[derive(Debug, Clone)]
pub struct Package {
    pub name: Option<CHLIdent>,
    pub source_root: PathBuf,
    pub mounts: Vec<(CHLIdent, PackageId)>,
    pub includes: Vec<IncludeRoot>,
    pub unit_includes: FxHashMap<PathBuf, Vec<IncludeRoot>>,
    pub defines: Vec<(String, String)>,
    pub exports: Option<FxHashSet<PathBuf>>,
}

impl Package {
    pub fn new(name: Option<CHLIdent>, source_root: PathBuf) -> Self {
        Self {
            name,
            source_root,
            mounts: Vec::new(),
            includes: Vec::new(),
            unit_includes: FxHashMap::default(),
            defines: Vec::new(),
            exports: None,
        }
    }

    pub fn lexer_includes(&self, module: &Path) -> Vec<IncludeRoot> {
        let unit = self.unit_includes.get(module).into_iter().flatten();
        self.includes.iter().chain(unit).cloned().collect()
    }

    pub fn exports(&self, module: &Path) -> bool {
        let module = module.canonicalize().unwrap_or_else(|_| module.to_path_buf());
        self.exports.as_ref().is_none_or(|exports| exports.contains(&module))
    }
}

#[derive(Debug, Clone)]
pub struct Packages {
    packages: Vec<Package>,
}

impl Packages {
    pub fn new(defines: Vec<(String, String)>) -> Self {
        let std = CHLIdent::new("std");
        Self {
            packages: vec![Package {
                mounts: vec![(std.clone(), PackageId::STD)],
                defines,
                ..Package::new(Some(std), PathBuf::from(chl_library_directory("std")))
            }],
        }
    }

    pub fn standalone(
        source_root: PathBuf,
        includes: Vec<IncludeRoot>,
        defines: Vec<(String, String)>,
    ) -> (Self, PackageId) {
        let mut packages = Self::new(defines.clone());
        let root = packages.push(Package {
            mounts: vec![(CHLIdent::new("std"), PackageId::STD)],
            includes,
            defines,
            ..Package::new(None, source_root)
        });
        (packages, root)
    }

    pub fn push(&mut self, package: Package) -> PackageId {
        self.packages.push(package);
        PackageId(self.packages.len() as u32 - 1)
    }

    pub fn get(&self, id: PackageId) -> &Package {
        &self.packages[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: PackageId) -> &mut Package {
        &mut self.packages[id.0 as usize]
    }

    pub fn ids(&self) -> impl Iterator<Item = PackageId> {
        (0..self.packages.len() as u32).map(PackageId)
    }

    pub fn namespace(&self, id: PackageId) -> NamespacePath {
        NamespacePath::new(self.get(id).name.iter().cloned().collect())
    }

    pub fn scope(&self, id: PackageId) -> ImportScope {
        let mounts = self
            .get(id)
            .mounts
            .iter()
            .map(|(prefix, target)| (prefix.clone(), self.namespace(*target)))
            .collect();
        ImportScope::new(self.namespace(id), mounts)
    }

    pub fn owner(&self, source: &Path) -> Option<PackageId> {
        self.ids()
            .filter(|id| source.starts_with(&self.get(*id).source_root))
            .max_by_key(|id| self.get(*id).source_root.components().count())
    }

    pub fn unit(&self, id: PackageId, source: &Path) -> Option<CompilationUnit> {
        let module = ModulePath::new(source.to_path_buf());
        let relative = module
            .as_path()
            .strip_prefix(&self.get(id).source_root)
            .ok()?
            .with_extension("");
        let namespace = self.namespace(id).join(NamespacePath::new(
            relative
                .components()
                .map(|component| CHLIdent::new(component.as_os_str().to_string_lossy().as_ref()))
                .collect(),
        ));
        Some(CompilationUnit::new(id, module, namespace))
    }

    pub fn locate(&self, namespace: &NamespacePath) -> Option<CompilationUnit> {
        let named = namespace.segments().split_first().and_then(|(first, rest)| {
            self.ids()
                .find(|id| self.get(*id).name.as_ref() == Some(first))
                .map(|id| (id, rest))
        });
        let anonymous = || {
            self.ids()
                .find(|id| self.get(*id).name.is_none())
                .map(|id| (id, namespace.segments()))
        };
        let (id, rest) = named.or_else(anonymous)?;
        if rest.is_empty() {
            return None;
        }

        let stem = rest
            .iter()
            .fold(self.get(id).source_root.clone(), |path, segment| path.join(segment.as_str()));
        Some(CompilationUnit::new(
            id,
            ModulePath::new(stem.with_extension("chl")),
            namespace.clone(),
        ))
    }
}
