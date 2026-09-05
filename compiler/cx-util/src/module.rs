use std::path::PathBuf;

use speedy::{Context, Readable, Reader, Writable, Writer};

use crate::identifier::CXIdent;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModulePath(PathBuf);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Namespace {
    module_path: ModulePath,
    project_base: PathBuf,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct QualifiedName {
    namespace: Namespace,
    name: CXIdent,
}

impl<'a, C: Context> Readable<'a, C> for ModulePath {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let path_str = String::read_from(reader)?;
        Ok(ModulePath(PathBuf::from(path_str)))
    }
}

impl <C: Context> Writable<C> for ModulePath {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + Writer<C>,
    {
        let path_str = self.0.to_string_lossy().to_string();
        path_str.write_to(writer)
    }
}

impl <'a, C: Context> Readable<'a, C> for Namespace {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let module_path = ModulePath::read_from(reader)?;
        let project_base = String::read_from(reader)?;
        Ok(Namespace {
            module_path,
            project_base: PathBuf::from(project_base),
        })
    }
}

impl <C: Context> Writable<C> for Namespace {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + Writer<C>,
    {
        self.module_path.write_to(writer)?;
        
        let project_base_str = self.project_base.to_string_lossy().to_string();
        project_base_str.write_to(writer)
    }
}