use std::collections::HashMap;

use speedy::{Readable, Writable};

use crate::data::{
    CXFunctionTemplate, CXNaivePrototype, CXNaiveType, CXTypeTemplate, ModuleResource,
    NaiveFnIdent, NaiveTypeIdent,
};

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXNaiveMap<Identifier: Eq + std::hash::Hash, Standard, Template> {
    standard: HashMap<Identifier, ModuleResource<Standard>>,
    templates: HashMap<Identifier, ModuleResource<Template>>,
}

pub type CXNaiveTypeMap = CXNaiveMap<NaiveTypeIdent, CXNaiveType, CXTypeTemplate>;
pub type CXNaiveFnMap = CXNaiveMap<NaiveFnIdent, CXNaivePrototype, CXFunctionTemplate>;

impl<Identifier, Standard, Template> Default for CXNaiveMap<Identifier, Standard, Template>
where
    Identifier: Eq + std::hash::Hash,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<Identifier, Standard, Template> CXNaiveMap<Identifier, Standard, Template>
where
    Identifier: Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            standard: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, name: Identifier, item: ModuleResource<Standard>) {
        self.standard.insert(name, item);
    }

    pub fn insert_template(&mut self, name: Identifier, item: ModuleResource<Template>) {
        self.templates.insert(name, item);
    }

    pub fn remove_standard(
        &mut self,
        name: &Identifier,
    ) -> Option<(Identifier, ModuleResource<Standard>)>
    where
        Identifier: Clone,
    {
        self.standard.remove_entry(name)
    }

    pub fn remove_template(
        &mut self,
        name: &Identifier,
    ) -> Option<(Identifier, ModuleResource<Template>)>
    where
        Identifier: Clone,
    {
        self.templates.remove_entry(name)
    }

    pub fn standard_iter(&self) -> impl Iterator<Item = (&Identifier, &ModuleResource<Standard>)> {
        self.standard.iter()
    }

    pub fn template_iter(&self) -> impl Iterator<Item = (&Identifier, &ModuleResource<Template>)> {
        self.templates.iter()
    }

    pub fn get_standard(&self, ident: &Identifier) -> Option<&ModuleResource<Standard>> {
        self.standard.get(ident)
    }

    pub fn get_template(&self, ident: &Identifier) -> Option<&ModuleResource<Template>> {
        self.templates.get(ident)
    }

    pub fn is_key_std(&self, ident: &Identifier) -> bool {
        self.standard.contains_key(ident)
    }

    pub fn is_key_template(&self, ident: &Identifier) -> bool {
        self.templates.contains_key(ident)
    }

    pub fn is_key_any(&self, ident: &Identifier) -> bool {
        self.is_key_std(ident) || self.is_key_template(ident)
    }
}
