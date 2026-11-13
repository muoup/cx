use std::path::Path;

use mlua::{Lua, Result, UserData, UserDataMethods, Value, Variadic};

use super::state::{PackageDraft, Shared};
use super::target::TargetHandle;
use super::values::{define, fail, identifier, links, string};
use super::{global, run_file};
use crate::config::CONFIG_FILE;
use crate::config::schema::{IncludeSpec, IncludeSpecSource, Mount, MountTarget};

const RESERVED_PACKAGES: &[&str] = &["std"];

struct CUnitHandle {
    state: Shared,
    package: usize,
}

struct PackageHandle {
    state: Shared,
    target: MountTarget,
}

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
    global(lua, state, "include", |lua, state, (path, alias): (Value, Value)| {
        let current = state.borrow().current();
        add_include(lua, state, current, &path, &alias, false)
    })?;
    global(lua, state, "headers", |lua, state, (pattern, alias): (Value, Value)| {
        let current = state.borrow().current();
        add_include(lua, state, current, &pattern, &alias, true)
    })?;
    global(lua, state, "define", |lua, state, (name, value): (Value, Value)| {
        let current = state.borrow().current();
        add_define(lua, state, current, &name, &value)
    })?;
    global(lua, state, "link", |lua, state, entries: Variadic<Value>| {
        let current = state.borrow().current();
        add_links(lua, state, current, entries)
    })?;
    global(lua, state, "import_c", |lua, state, name: Value| {
        let name = string(lua, &name, "import_c")?;
        if name.is_empty() {
            return Err(fail(lua, "C unit names cannot be empty"));
        }
        let mut borrowed = state.borrow_mut();
        let package = borrowed.package();
        if package.c_units.contains(&name) {
            return Err(fail(lua, format!("C unit '{name}' is imported more than once")));
        }
        package.c_units.push(name);
        let package = borrowed.current();
        Ok(CUnitHandle { state: state.clone(), package })
    })?;
    global(lua, state, "import_cx", |lua, state, (path, prefix): (Value, Value)| {
        let relative = string(lua, &path, "import_cx")?;
        let path = state.borrow().directory().join(&relative);
        let path = match path.is_dir() {
            true => path.join(CONFIG_FILE),
            false => path,
        };
        let path = path
            .canonicalize()
            .map_err(|error| fail(lua, format!("cannot import '{relative}': {error}")))?;
        let index = load(lua, state, &path)?;
        mount(lua, state, &prefix, MountTarget::Package(index))
    })?;
    global(lua, state, "stdlib", |lua, state, prefix: Value| {
        mount(lua, state, &prefix, MountTarget::Std)
    })?;
    global(lua, state, "import", |lua, state, path: Value| import(lua, state, &path))?;
    Ok(())
}

pub(super) fn load(lua: &Lua, state: &Shared, path: &Path) -> Result<usize> {
    let existing = state.borrow().packages.iter().position(|package| package.script == path);
    if let Some(index) = existing {
        if state.borrow().stack.contains(&index) {
            return Err(fail(lua, format!("import cycle through '{}'", path.display())));
        }
        return Ok(index);
    }

    let helpers = lua.create_table()?;
    let index = {
        let mut state = state.borrow_mut();
        state.packages.push(PackageDraft::new(path.to_path_buf(), helpers));
        let index = state.packages.len() - 1;
        state.stack.push(index);
        index
    };
    let result = run_file(lua, state, path);
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

fn mount(lua: &Lua, state: &Shared, prefix: &Value, target: MountTarget) -> Result<PackageHandle> {
    let prefix = identifier(lua, prefix, "module prefix")?;
    let mut borrowed = state.borrow_mut();
    let package = borrowed.package();
    if package.mounts.iter().any(|mount| mount.prefix == prefix) {
        return Err(fail(lua, format!("module prefix '{prefix}' is mounted more than once")));
    }
    package.mounts.push(Mount { prefix, target });
    Ok(PackageHandle { state: state.clone(), target })
}

fn import(lua: &Lua, state: &Shared, path: &Value) -> Result<Value> {
    let relative = string(lua, path, "import")?;
    let path = state.borrow().directory().join(&relative);
    let path = path
        .canonicalize()
        .map_err(|error| fail(lua, format!("cannot import '{relative}': {error}")))?;
    let key = path.to_string_lossy().to_string();

    let helpers = state.borrow_mut().package().helpers.clone();
    if let Value::Table(cached) = helpers.raw_get::<Value>(key.as_str())? {
        return cached.raw_get(1);
    }
    if state.borrow().files.contains(&path) {
        return Err(fail(lua, format!("import cycle through '{relative}'")));
    }

    let value = run_file(lua, state, &path)?;
    let cached = lua.create_table()?;
    cached.raw_set(1, value.clone())?;
    helpers.raw_set(key, cached)?;
    Ok(value)
}

fn add_include(
    lua: &Lua,
    state: &Shared,
    package: usize,
    path: &Value,
    alias: &Value,
    glob: bool,
) -> Result<()> {
    let path = string(lua, path, if glob { "headers" } else { "include" })?;
    let alias = match alias {
        Value::Nil => None,
        other => Some(string(lua, other, "include alias")?.trim_matches('/').to_string())
            .filter(|alias| !alias.is_empty()),
    };
    let directory = state.borrow().directory();
    let source = match glob {
        true => IncludeSpecSource::Glob { base: directory, pattern: path },
        false => IncludeSpecSource::Directory(directory.join(path)),
    };
    state.borrow_mut().packages[package]
        .usage
        .includes
        .push(IncludeSpec { source, alias });
    Ok(())
}

fn add_define(lua: &Lua, state: &Shared, package: usize, name: &Value, value: &Value) -> Result<()> {
    if let Some(definition) = define(lua, name, value)? {
        state.borrow_mut().packages[package].usage.defines.push(definition);
    }
    Ok(())
}

fn add_links(lua: &Lua, state: &Shared, package: usize, entries: Variadic<Value>) -> Result<()> {
    for entry in entries {
        let entries = links(lua, entry)?;
        state.borrow_mut().packages[package].usage.link.extend(entries);
    }
    Ok(())
}

impl UserData for CUnitHandle {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("include", |lua, this, (path, alias): (Value, Value)| {
            add_include(lua, &this.state, this.package, &path, &alias, false)
        });
        methods.add_method("headers", |lua, this, (pattern, alias): (Value, Value)| {
            add_include(lua, &this.state, this.package, &pattern, &alias, true)
        });
        methods.add_method("object", |lua, this, paths: Variadic<Value>| {
            let directory = this.state.borrow().directory();
            for path in paths {
                let path = directory.join(string(lua, &path, "object")?);
                this.state.borrow_mut().packages[this.package].usage.objects.push(path);
            }
            Ok(())
        });
        methods.add_method("link", |lua, this, entries: Variadic<Value>| {
            add_links(lua, &this.state, this.package, entries)
        });
        methods.add_method("define", |lua, this, (name, value): (Value, Value)| {
            add_define(lua, &this.state, this.package, &name, &value)
        });
    }
}

impl UserData for PackageHandle {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("target", |lua, this, name: Value| {
            let name = string(lua, &name, "target")?;
            let MountTarget::Package(package) = this.target else {
                return Err(fail(lua, "the standard library has no targets"));
            };
            let mut state = this.state.borrow_mut();
            let exported = &state.packages[package];
            let Some(index) = exported
                .exported
                .iter()
                .copied()
                .find(|index| state.targets[*index].name == name)
            else {
                return Err(fail(
                    lua,
                    format!("target '{name}' is not exported by {}", exported.label()),
                ));
            };
            let current = state.current();
            if !state.packages[current].selectable.contains(&index) {
                let selectable = &state.packages[current].selectable;
                if selectable.iter().any(|other| state.targets[*other].name == name) {
                    return Err(fail(lua, format!("target '{name}' is declared more than once")));
                }
                state.packages[current].selectable.push(index);
            }
            Ok(TargetHandle::new(this.state.clone(), index))
        });
    }
}
