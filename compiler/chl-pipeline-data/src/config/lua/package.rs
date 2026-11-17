use std::path::Path;

use mlua::{Lua, Result, Value, Variadic};

use super::state::{PackageDraft, Shared};
use super::values::{define, fail, identifier, links, string};
use super::{global, run_file};
use crate::config::CONFIG_FILE;
use crate::config::schema::{IncludeSpec, IncludeSpecSource, Mount, MountTarget};

const RESERVED_PACKAGES: &[&str] = &["std"];
const GLOB_CHARACTERS: &[char] = &['*', '?', '['];

pub(super) fn register(lua: &Lua, state: &Shared) -> Result<()> {
    global(lua, state, "package", |lua, state, name: Value| declare_name(lua, state, &name))?;
    global(lua, state, "source_root", |lua, state, path: Value| {
        let path = state.borrow().directory().join(string(lua, &path, "source_root")?);
        let mut state = state.borrow_mut();
        let package = state.package();
        if package.source_root.is_some() {
            return Err(fail(lua, "source_root is declared more than once"));
        }
        package.source_root = Some(path);
        Ok(())
    })?;
    global(lua, state, "include", |lua, state, (source, prefix): (Value, Value)| {
        include(lua, state, &source, &prefix)
    })?;
    global(lua, state, "define", |lua, state, (name, value): (Value, Value)| {
        if let Some(definition) = define(lua, &name, &value)? {
            state.borrow_mut().package().defines.push(definition);
        }
        Ok(())
    })?;
    global(lua, state, "link", |lua, state, values: Variadic<Value>| {
        let mut entries = Vec::new();
        for value in values {
            entries.extend(links(lua, value)?);
        }
        state.borrow_mut().package().links.extend(entries);
        Ok(())
    })?;
    global(lua, state, "object", |lua, state, values: Variadic<Value>| {
        let directory = state.borrow().directory();
        let mut paths = Vec::new();
        for value in values {
            paths.push(directory.join(string(lua, &value, "object")?));
        }
        state.borrow_mut().package().objects.extend(paths);
        Ok(())
    })?;
    global(lua, state, "dep", |lua, state, (path, prefix): (Value, Value)| {
        dep(lua, state, &path, &prefix)
    })?;
    global(lua, state, "stdlib", |lua, state, prefix: Value| {
        mount(lua, state, &prefix, MountTarget::Std)
    })?;
    Ok(())
}

pub(super) fn load(lua: &Lua, state: &Shared, path: &Path) -> Result<usize> {
    let existing = state.borrow().packages.iter().position(|package| package.script == path);
    if let Some(index) = existing {
        if state.borrow().stack.contains(&index) {
            return Err(fail(lua, format!("dependency cycle through '{}'", path.display())));
        }
        return Ok(index);
    }

    let index = {
        let mut state = state.borrow_mut();
        state.packages.push(PackageDraft::new(path.to_path_buf()));
        let index = state.packages.len() - 1;
        state.stack.push(index);
        index
    };
    let result = run_file(lua, path);
    state.borrow_mut().stack.pop();
    result?;

    if state.borrow().packages[index].name.is_none() {
        return Err(fail(
            lua,
            format!("'{}' does not declare its package with package(\"name\")", path.display()),
        ));
    }
    Ok(index)
}

pub(super) fn resolve(lua: &Lua, state: &Shared, path: &Value, what: &str) -> Result<usize> {
    let relative = string(lua, path, what)?;
    let path = state.borrow().directory().join(&relative);
    let path = match path.is_dir() {
        true => path.join(CONFIG_FILE),
        false => path,
    };
    let path = path
        .canonicalize()
        .map_err(|error| fail(lua, format!("cannot resolve package '{relative}': {error}")))?;
    let current = state.borrow().current();
    let own = state.borrow().packages[current].script == path;
    match own {
        true => Ok(current),
        false => load(lua, state, &path),
    }
}

fn declare_name(lua: &Lua, state: &Shared, name: &Value) -> Result<()> {
    let name = identifier(lua, name, "package name")?;
    if RESERVED_PACKAGES.contains(&name.as_str()) {
        return Err(fail(lua, format!("package name '{name}' is reserved")));
    }
    let mut state = state.borrow_mut();
    if let Some(other) = state.packages.iter().find(|package| package.name.as_ref() == Some(&name)) {
        return Err(fail(
            lua,
            format!("package name '{name}' is already declared by '{}'", other.script.display()),
        ));
    }
    let package = state.package();
    if package.name.is_some() {
        return Err(fail(lua, "package is declared more than once"));
    }
    package.name = Some(name);
    Ok(())
}

fn dep(lua: &Lua, state: &Shared, path: &Value, prefix: &Value) -> Result<()> {
    let index = resolve(lua, state, path, "dep")?;
    {
        let mut borrowed = state.borrow_mut();
        let current = borrowed.current();
        let label = borrowed.packages[index].label();
        if index == current {
            return Err(fail(lua, "a package cannot depend on itself"));
        }
        if borrowed.packages[index].library.is_none() {
            return Err(fail(lua, format!("{label} declares no library() to depend on")));
        }
        let package = borrowed.package();
        if package.deps.contains(&index) {
            return Err(fail(lua, format!("{label} is a dependency more than once")));
        }
        package.deps.push(index);
    }
    match prefix {
        Value::Nil => Ok(()),
        prefix => mount(lua, state, prefix, MountTarget::Package(index)),
    }
}

fn mount(lua: &Lua, state: &Shared, prefix: &Value, target: MountTarget) -> Result<()> {
    let prefix = identifier(lua, prefix, "module prefix")?;
    let mut borrowed = state.borrow_mut();
    let package = borrowed.package();
    if package.mounts.iter().any(|mount| mount.prefix == prefix) {
        return Err(fail(lua, format!("module prefix '{prefix}' is mounted more than once")));
    }
    package.mounts.push(Mount { prefix, target });
    Ok(())
}

pub(super) fn prefix(lua: &Lua, value: &Value) -> Result<Option<String>> {
    match value {
        Value::Nil => Ok(None),
        other => Ok(Some(string(lua, other, "include prefix")?.trim_matches('/').to_string())
            .filter(|alias| !alias.is_empty())),
    }
}

fn include(lua: &Lua, state: &Shared, source: &Value, alias: &Value) -> Result<()> {
    let alias = prefix(lua, alias)?;
    let path = string(lua, source, "include")?;
    let directory = state.borrow().directory();
    let source = match path.contains(GLOB_CHARACTERS) {
        true => IncludeSpecSource::Glob { base: directory, pattern: path },
        false => IncludeSpecSource::Directory(directory.join(path)),
    };
    state.borrow_mut().package().includes.push(IncludeSpec { source, alias });
    Ok(())
}
