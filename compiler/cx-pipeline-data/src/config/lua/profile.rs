use mlua::{AnyUserData, Lua, Result, UserData, UserDataMethods, Value};

use super::global;
use super::state::{ProfileDraft, Shared};
use super::values::{boolean, define, describe, fail, string};
use crate::config::schema::{DEBUG_PROFILE, ProfileSettings};
use crate::{CompilerBackend, OptimizationLevel};

struct ProfileHandle {
    state: Shared,
    package: usize,
    index: usize,
}

pub(super) fn register(lua: &Lua, state: &Shared) -> Result<()> {
    global(lua, state, "profile", |lua, state, (name, base): (Value, Value)| {
        let name = string(lua, &name, "profile name")?;
        if name.is_empty() {
            return Err(fail(lua, "profile names cannot be empty"));
        }
        if name == DEBUG_PROFILE {
            return Err(fail(lua, "the built-in debug profile is changed through debug_profile()"));
        }
        let mut borrowed = state.borrow_mut();
        let current = borrowed.current();
        let base = match base {
            Value::Nil => None,
            other => Some(base_index(lua, &other, current)?),
        };
        let package = borrowed.package();
        if package.profiles.iter().any(|profile| profile.name == name) {
            return Err(fail(lua, format!("profile '{name}' is declared more than once")));
        }
        package.profiles.push(ProfileDraft { name, base, settings: ProfileSettings::default() });
        let index = package.profiles.len() - 1;
        Ok(ProfileHandle { state: state.clone(), package: current, index })
    })?;
    global(lua, state, "debug_profile", |_, state, ()| {
        let package = state.borrow().current();
        Ok(ProfileHandle { state: state.clone(), package, index: 0 })
    })?;
    Ok(())
}

fn base_index(lua: &Lua, value: &Value, package: usize) -> Result<usize> {
    let handle = value
        .as_userdata()
        .and_then(|userdata: &AnyUserData| userdata.borrow::<ProfileHandle>().ok())
        .map(|handle| (handle.package, handle.index));
    match handle {
        Some((owner, index)) if owner == package => Ok(index),
        Some(_) => Err(fail(lua, "a profile can only extend a profile of its own package")),
        None => Err(fail(lua, format!("expected a profile to extend, found {}", describe(value)))),
    }
}

fn flag(lua: &Lua, value: &Value, what: &str) -> Result<bool> {
    match value {
        Value::Nil => Ok(true),
        other => boolean(lua, other, what),
    }
}

impl ProfileHandle {
    fn update(&self, change: impl FnOnce(&mut ProfileSettings)) {
        let mut state = self.state.borrow_mut();
        change(&mut state.packages[self.package].profiles[self.index].settings);
    }
}

impl UserData for ProfileHandle {
    fn add_methods<M: UserDataMethods<Self>>(methods: &mut M) {
        methods.add_method("backend", |lua, this, value: Value| {
            let backend = match string(lua, &value, "backend")?.as_str() {
                "cranelift" => CompilerBackend::Cranelift,
                "llvm" => CompilerBackend::LLVM,
                other => {
                    return Err(fail(lua, format!("unknown backend '{other}', expected cranelift or llvm")));
                }
            };
            this.update(|settings| settings.backend = Some(backend));
            Ok(())
        });
        methods.add_method("optimize", |lua, this, value: Value| {
            let level = match string(lua, &value, "optimization")?.as_str() {
                "O0" => OptimizationLevel::O0,
                "O1" => OptimizationLevel::O1,
                "O2" => OptimizationLevel::O2,
                "O3" => OptimizationLevel::O3,
                "Osize" => OptimizationLevel::Osize,
                "Ofast" => OptimizationLevel::Ofast,
                other => return Err(fail(lua, format!("unknown optimization level '{other}'"))),
            };
            this.update(|settings| settings.optimization = Some(level));
            Ok(())
        });
        methods.add_method("no_assert", |lua, this, value: Value| {
            let enabled = flag(lua, &value, "no_assert")?;
            this.update(|settings| settings.no_assert = Some(enabled));
            Ok(())
        });
        methods.add_method("no_analysis", |lua, this, value: Value| {
            let enabled = flag(lua, &value, "no_analysis")?;
            this.update(|settings| settings.no_analysis = Some(enabled));
            Ok(())
        });
        methods.add_method("explicit_return", |lua, this, value: Value| {
            let enabled = flag(lua, &value, "explicit_return")?;
            this.update(|settings| settings.require_explicit_return = Some(enabled));
            Ok(())
        });
        methods.add_method("define", |lua, this, (name, value): (Value, Value)| {
            if let Some(definition) = define(lua, &name, &value)? {
                this.update(|settings| settings.defines.push(definition));
            }
            Ok(())
        });
    }
}
