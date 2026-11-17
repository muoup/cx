use mlua::{Lua, Result, UserData, UserDataMethods, Value, Variadic};

use super::global;
use super::state::Shared;
use super::values::{fail, string};
use crate::config::schema::LibraryConfig;

struct LibraryHandle {
    state: Shared,
    package: usize,
}

pub(super) fn register(lua: &Lua, state: &Shared) -> Result<()> {
    global(lua, state, "library", |lua, state, ()| {
        let mut borrowed = state.borrow_mut();
        let package = borrowed.current();
        let draft = borrowed.package();
        if draft.library.is_some() {
            return Err(fail(lua, "library() is declared more than once"));
        }
        draft.library = Some(LibraryConfig::default());
        Ok(LibraryHandle { state: state.clone(), package })
    })
}

impl LibraryHandle {
    fn update(&self, lua: &Lua, values: Variadic<Value>, what: &str, change: impl FnOnce(&mut LibraryConfig, Vec<String>)) -> Result<()> {
        let patterns = values
            .iter()
            .map(|value| string(lua, value, what))
            .collect::<Result<Vec<_>>>()?;
        let mut state = self.state.borrow_mut();
        if state.current() != self.package {
            return Err(fail(lua, "a library belongs to another package and cannot be changed here"));
        }
        let library = state.packages[self.package].library.as_mut().expect("library handles exist once declared");
        change(library, patterns);
        Ok(())
    }
}

impl UserData for LibraryHandle {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("match", |lua, this, values: Variadic<Value>| {
            this.update(lua, values, "match pattern", |library, patterns| library.matches.extend(patterns))
        });
        methods.add_method("exclude", |lua, this, values: Variadic<Value>| {
            this.update(lua, values, "exclude pattern", |library, patterns| library.excludes.extend(patterns))
        });
    }
}
