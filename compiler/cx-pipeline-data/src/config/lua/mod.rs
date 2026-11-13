mod package;
mod profile;
mod state;
mod target;
mod values;

use std::cell::Cell;
use std::path::Path;

use mlua::chunk::ChunkMode;
use mlua::{
    Error, FromLuaMulti, HookTriggers, IntoLuaMulti, Lua, LuaOptions, Result, StdLib, Table,
    Value, VmState,
};

use self::state::{Shared, State};
use self::values::{error_message, fail};
use super::schema::{
    PackageConfig, ProfileConfig, ProjectConfig, TargetConfig,
};

const MEMORY_LIMIT: usize = 64 * 1024 * 1024;
const HOOK_INTERVAL: u32 = 10_000;
const INSTRUCTION_BUDGET: u32 = 1_000;

pub(super) fn evaluate(file: &Path) -> std::result::Result<ProjectConfig, String> {
    let file = file
        .canonicalize()
        .map_err(|error| format!("cannot read '{}': {error}", file.display()))?;
    let state = Shared::default();
    let run = || -> Result<()> {
        let lua = sandbox()?;
        package::register(&lua, &state)?;
        profile::register(&lua, &state)?;
        target::register(&lua, &state)?;
        package::load(&lua, &state, &file)?;
        Ok(())
    };
    run().map_err(|error| error_message(&error))?;

    assemble(state.take())
}

fn sandbox() -> Result<Lua> {
    let lua = Lua::new_with(
        StdLib::STRING | StdLib::TABLE | StdLib::MATH | StdLib::UTF8,
        LuaOptions::default(),
    )?;
    lua.set_memory_limit(MEMORY_LIMIT)?;

    let globals = lua.globals();
    for name in ["dofile", "loadfile", "load", "require", "collectgarbage", "print"] {
        globals.raw_set(name, Value::Nil)?;
    }
    let math: Table = globals.get("math")?;
    math.raw_set("random", Value::Nil)?;
    math.raw_set("randomseed", Value::Nil)?;
    let string: Table = globals.get("string")?;
    string.raw_set("dump", Value::Nil)?;

    let budget = Cell::new(0u32);
    lua.set_hook(
        HookTriggers::new().every_nth_instruction(HOOK_INTERVAL),
        move |_, debug| {
            budget.set(budget.get() + 1);
            if budget.get() <= INSTRUCTION_BUDGET {
                return Ok(VmState::Continue);
            }
            let source = debug.source().short_src.map(|source| source.into_owned());
            let location = source
                .zip(debug.current_line())
                .map(|(source, line)| format!("{source}:{line}: "))
                .unwrap_or_default();
            Err(Error::runtime(format!(
                "{location}configuration exceeded its instruction budget"
            )))
        },
    )?;
    Ok(lua)
}

fn global<A, R>(
    lua: &Lua,
    state: &Shared,
    name: &str,
    function: impl Fn(&Lua, &Shared, A) -> Result<R> + 'static,
) -> Result<()>
where
    A: FromLuaMulti,
    R: IntoLuaMulti,
{
    let state = state.clone();
    lua.globals().raw_set(
        name,
        lua.create_function(move |lua, arguments: A| function(lua, &state, arguments))?,
    )
}

fn run_file(lua: &Lua, state: &Shared, path: &Path) -> Result<Value> {
    let name = path.display().to_string();
    let source = std::fs::read_to_string(path)
        .map_err(|error| fail(lua, format!("cannot read '{name}': {error}")))?;

    let environment = lua.create_table()?;
    let metatable = lua.create_table()?;
    metatable.raw_set("__index", lua.globals())?;
    environment.set_metatable(Some(metatable))?;

    state.borrow_mut().files.push(path.to_path_buf());
    let result = lua
        .load(source)
        .set_name(format!("@{name}"))
        .set_mode(ChunkMode::Text)
        .set_environment(environment)
        .eval::<Value>();
    state.borrow_mut().files.pop();
    result
}

fn assemble(state: State) -> std::result::Result<ProjectConfig, String> {
    let mut packages = Vec::new();
    for draft in &state.packages {
        let name = draft.name.clone().expect("packages declare their name before assembly");
        let root = draft.script.parent().unwrap_or(Path::new("")).to_path_buf();
        let source_root = draft.source_root.clone().unwrap_or_else(|| root.clone());
        for mount in &draft.mounts {
            let clashes = [
                source_root.join(&mount.prefix),
                source_root.join(format!("{}.cx", mount.prefix)),
                source_root.join(format!("{}.cxc", mount.prefix)),
            ];
            if let Some(path) = clashes.iter().find(|path| path.exists()) {
                return Err(format!(
                    "package '{name}' mounts '{}', which clashes with '{}'",
                    mount.prefix,
                    path.display()
                ));
            }
        }
        packages.push(PackageConfig {
            name,
            root,
            source_root,
            mounts: draft.mounts.clone(),
            usage: draft.usage.clone(),
        });
    }

    let root = &state.packages[0];
    let mut profiles: Vec<ProfileConfig> = Vec::new();
    for draft in &root.profiles {
        let settings = match draft.base {
            Some(base) => draft.settings.clone().inherit(&profiles[base].settings),
            None => draft.settings.clone(),
        };
        profiles.push(ProfileConfig { name: draft.name.clone(), settings });
    }

    let mut targets = Vec::new();
    for &index in &root.selectable {
        let draft = &state.targets[index];
        if draft.matches.is_empty() {
            return Err(format!("target '{}' has no match patterns", draft.name));
        }
        targets.push(TargetConfig {
            name: draft.name.clone(),
            kind: draft.kind,
            package: draft.package,
            matches: draft.matches.clone(),
            excludes: draft.excludes.clone(),
            headers: draft.headers.clone(),
        });
    }

    Ok(ProjectConfig {
        root: packages[0].root.clone(),
        packages,
        profiles,
        targets,
    })
}

