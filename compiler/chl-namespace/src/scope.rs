use chl_util::identifier::CHLIdent;

use crate::module::NamespacePath;

#[derive(Debug, Clone)]
pub struct ImportScope {
    local: NamespacePath,
    mounts: Vec<(CHLIdent, NamespacePath)>,
}

impl ImportScope {
    pub fn new(local: NamespacePath, mounts: Vec<(CHLIdent, NamespacePath)>) -> Self {
        Self { local, mounts }
    }

    pub fn resolve(&self, written: &NamespacePath) -> NamespacePath {
        let mounted = written.segments().split_first().and_then(|(first, rest)| {
            self.mounts
                .iter()
                .find(|(prefix, _)| prefix == first)
                .map(|(_, target)| target.clone().join(NamespacePath::new(rest.to_vec())))
        });
        mounted.unwrap_or_else(|| self.local.clone().join(written.clone()))
    }

    pub fn aliases(&self) -> impl Iterator<Item = (NamespacePath, NamespacePath)> + '_ {
        let local = (!self.local.is_root()).then(|| (NamespacePath::root(), self.local.clone()));
        let mounts = self
            .mounts
            .iter()
            .map(|(prefix, target)| (NamespacePath::new(vec![prefix.clone()]), target.clone()))
            .filter(|(prefix, target)| prefix != target);
        local.into_iter().chain(mounts)
    }
}
