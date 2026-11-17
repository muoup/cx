mod library;
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

use self::state::{FfiIncludeDraft, Shared, State};
use self::values::{error_message, fail};
use super::schema::{
    FfiInclude, PackageConfig, ProfileConfig, ProjectConfig, TargetConfig, TargetKind,
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
        library::register(&lua, &state)?;
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

fn run_file(lua: &Lua, path: &Path) -> Result<Value> {
    let name = path.display().to_string();
    let source = std::fs::read_to_string(path)
        .map_err(|error| fail(lua, format!("cannot read '{name}': {error}")))?;

    let environment = lua.create_table()?;
    let metatable = lua.create_table()?;
    metatable.raw_set("__index", lua.globals())?;
    environment.set_metatable(Some(metatable))?;

    lua.load(source)
        .set_name(format!("@{name}"))
        .set_mode(ChunkMode::Text)
        .set_environment(environment)
        .eval::<Value>()
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
                source_root.join(format!("{}.chl", mount.prefix)),
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
            deps: draft.deps.clone(),
            library: draft.library.clone(),
            includes: draft.includes.clone(),
            defines: draft.defines.clone(),
            objects: draft.objects.clone(),
            links: draft.links.clone(),
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
    for (index, draft) in state.targets.iter().enumerate() {
        let empty = match draft.kind {
            TargetKind::Executable => draft.matches.is_empty(),
            TargetKind::FfiLib => draft.matches.is_empty() && draft.headers.is_empty(),
        };
        if empty {
            return Err(format!("target '{}' has no match patterns", draft.name));
        }
        let ffi_includes = draft
            .ffi_includes
            .iter()
            .map(|include| ffi_include(&state, index, include))
            .collect::<std::result::Result<Vec<_>, _>>()?;
        targets.push(TargetConfig {
            name: draft.name.clone(),
            kind: draft.kind,
            package: draft.package,
            matches: draft.matches.clone(),
            excludes: draft.excludes.clone(),
            headers: draft.headers.clone(),
            ffi_includes,
        });
    }
    for target in 0..targets.len() {
        ffi_cycle(&targets, target, &mut Vec::new())?;
    }

    Ok(ProjectConfig {
        root: packages[0].root.clone(),
        packages,
        profiles,
        targets,
    })
}


fn ffi_include(
    state: &State,
    target: usize,
    include: &FfiIncludeDraft,
) -> std::result::Result<FfiInclude, String> {
    let label = state.packages[include.package].label();
    let found = state
        .targets
        .iter()
        .position(|other| other.package == include.package && other.name == include.name);
    let Some(index) = found else {
        return Err(format!("{label} declares no target '{}'", include.name));
    };
    if state.targets[index].kind != TargetKind::FfiLib {
        return Err(format!("target '{}' of {label} is not an ffi-lib", include.name));
    }
    if index == target {
        return Err(format!("ffi-lib '{}' cannot include its own headers", include.name));
    }
    Ok(FfiInclude { target: index, prefix: include.prefix.clone() })
}

fn ffi_cycle(targets: &[TargetConfig], target: usize, path: &mut Vec<usize>) -> std::result::Result<(), String> {
    if path.contains(&target) {
        return Err(format!("ffi_lib_c_include cycle through target '{}'", targets[target].name));
    }
    path.push(target);
    for include in &targets[target].ffi_includes {
        ffi_cycle(targets, include.target, path)?;
    }
    path.pop();
    Ok(())
}
