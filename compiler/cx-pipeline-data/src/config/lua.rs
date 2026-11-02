use std::cell::{Cell, RefCell};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use mlua::{Error, HookTriggers, Lua, LuaOptions, Result, StdLib, Table, Value, VmState};

use mlua::chunk::ChunkMode;
use super::schema::{
    DependencyConfig, DependencyRef, ProfileSettings, ProjectConfig, SourcePattern, TargetConfig,
    TargetKind, Usage,
};
use super::values::{
    Export, Handle, boolean, deps, entries, error_message, fail, string, table, unknown_key, usage,
};
use crate::{CompilerBackend, OptimizationLevel};

const MEMORY_LIMIT: usize = 64 * 1024 * 1024;
const HOOK_INTERVAL: u32 = 10_000;
const INSTRUCTION_BUDGET: u32 = 1_000;

#[derive(Default)]
struct ProjectDeclaration {
    name: Option<String>,
    source_root: Option<PathBuf>,
    backend: Option<CompilerBackend>,
    debug: ProfileSettings,
    release: ProfileSettings,
}

#[derive(Default)]
struct State {
    root: PathBuf,
    files: Vec<PathBuf>,
    project: Option<ProjectDeclaration>,
    targets: Vec<TargetConfig>,
    dependencies: Vec<DependencyConfig>,
}

type Shared = Rc<RefCell<State>>;

impl State {
    fn directory(&self) -> PathBuf {
        self.files
            .last()
            .and_then(|file| file.parent())
            .unwrap_or(&self.root)
            .to_path_buf()
    }

    fn in_main_file(&self) -> bool {
        self.files.len() == 1
    }
}

pub(super) fn evaluate(root: &Path, file: &Path) -> std::result::Result<ProjectConfig, String> {
    let state = Rc::new(RefCell::new(State { root: root.to_path_buf(), ..State::default() }));
    let run = || -> Result<()> {
        let lua = sandbox()?;
        register(&lua, &state)?;
        run_file(&lua, &state, file)?;
        Ok(())
    };
    run().map_err(|error| error_message(&error))?;

    Ok(assemble(state.take()))
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

fn run_file(lua: &Lua, state: &Shared, path: &Path) -> Result<Value> {
    let root = state.borrow().root.clone();
    let name = path.strip_prefix(&root).unwrap_or(path).display().to_string();
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

fn register(lua: &Lua, state: &Shared) -> Result<()> {
    let globals = lua.globals();

    let shared = state.clone();
    globals.raw_set(
        "project",
        lua.create_function(move |lua, value: Value| project(lua, &shared, value))?,
    )?;

    for (name, kind) in [("executable", TargetKind::Executable), ("library", TargetKind::Library)] {
        let shared = state.clone();
        globals.raw_set(
            name,
            lua.create_function(move |lua, value: Value| {
                let target_name = string(lua, &value, name)?;
                let shared = shared.clone();
                lua.create_function(move |lua, value: Value| {
                    target(lua, &shared, kind, target_name.clone(), value)
                })
            })?,
        )?;
    }

    let shared = state.clone();
    globals.raw_set(
        "dependency",
        lua.create_function(move |lua, value: Value| dependency(lua, &shared, value))?,
    )?;

    globals.raw_set(
        "exports",
        lua.create_function(|lua, value: Value| Ok(Export(string(lua, &value, "exports")?)))?,
    )?;

    let shared = state.clone();
    let loaded = lua.create_table()?;
    globals.raw_set(
        "import",
        lua.create_function(move |lua, value: Value| import(lua, &shared, &loaded, value))?,
    )?;
    Ok(())
}

fn project(lua: &Lua, state: &Shared, value: Value) -> Result<()> {
    if !state.borrow().in_main_file() {
        return Err(fail(lua, "project can only be declared in cx.lua"));
    }
    if state.borrow().project.is_some() {
        return Err(fail(lua, "project is declared more than once"));
    }
    let entries = entries(lua, &table(lua, value, "project")?, "project")?;
    if !entries.positional.is_empty() {
        return Err(fail(lua, "project only accepts named fields"));
    }

    let directory = state.borrow().directory();
    let mut declaration = ProjectDeclaration::default();
    for (key, value) in entries.keyed {
        match key.as_str() {
            "name" => declaration.name = Some(string(lua, &value, "name")?),
            "source_root" => {
                declaration.source_root = Some(directory.join(string(lua, &value, "source_root")?))
            }
            "backend" => declaration.backend = Some(backend(lua, &value)?),
            "profiles" => profiles(lua, value, &mut declaration)?,
            other => return Err(unknown_key(lua, other, "project")),
        }
    }
    state.borrow_mut().project = Some(declaration);
    Ok(())
}

fn profiles(lua: &Lua, value: Value, declaration: &mut ProjectDeclaration) -> Result<()> {
    let profiles = entries(lua, &table(lua, value, "profiles")?, "profiles")?;
    if !profiles.positional.is_empty() {
        return Err(fail(lua, "profiles only accepts debug and release"));
    }
    for (key, value) in profiles.keyed {
        let settings = match key.as_str() {
            "debug" => &mut declaration.debug,
            "release" => &mut declaration.release,
            other => return Err(fail(lua, format!("unknown profile '{other}', expected debug or release"))),
        };
        let what = format!("profile '{key}'");
        let fields = entries(lua, &table(lua, value, &what)?, &what)?;
        if !fields.positional.is_empty() {
            return Err(fail(lua, format!("{what} only accepts named fields")));
        }
        for (key, value) in fields.keyed {
            match key.as_str() {
                "optimization" => settings.optimization = Some(optimization(lua, &value)?),
                "backend" => settings.backend = Some(backend(lua, &value)?),
                "no_analysis" => settings.no_analysis = Some(boolean(lua, &value, &key)?),
                "no_assert" => settings.no_assert = Some(boolean(lua, &value, &key)?),
                "require_explicit_return" => {
                    settings.require_explicit_return = Some(boolean(lua, &value, &key)?)
                }
                other => return Err(unknown_key(lua, other, &what)),
            }
        }
    }
    Ok(())
}

fn optimization(lua: &Lua, value: &Value) -> Result<OptimizationLevel> {
    match string(lua, value, "optimization")?.as_str() {
        "O0" => Ok(OptimizationLevel::O0),
        "O1" => Ok(OptimizationLevel::O1),
        "O2" => Ok(OptimizationLevel::O2),
        "O3" => Ok(OptimizationLevel::O3),
        "Osize" => Ok(OptimizationLevel::Osize),
        "Ofast" => Ok(OptimizationLevel::Ofast),
        other => Err(fail(lua, format!("unknown optimization level '{other}'"))),
    }
}

fn backend(lua: &Lua, value: &Value) -> Result<CompilerBackend> {
    match string(lua, value, "backend")?.as_str() {
        "cranelift" => Ok(CompilerBackend::Cranelift),
        "llvm" => Ok(CompilerBackend::LLVM),
        other => Err(fail(lua, format!("unknown backend '{other}', expected cranelift or llvm"))),
    }
}

fn target(lua: &Lua, state: &Shared, kind: TargetKind, name: String, value: Value) -> Result<Handle> {
    let what = match kind {
        TargetKind::Executable => format!("executable '{name}'"),
        TargetKind::Library => format!("library '{name}'"),
    };
    if !state.borrow().in_main_file() {
        return Err(fail(lua, format!("{what} must be declared in cx.lua")));
    }
    if name.is_empty() {
        return Err(fail(lua, "target names cannot be empty"));
    }
    if state.borrow().targets.iter().any(|target| target.name == name) {
        return Err(fail(lua, format!("target '{name}' is declared more than once")));
    }

    let entries = entries(lua, &table(lua, value, &what)?, &what)?;
    let mut flattened = Vec::new();
    for value in entries.positional {
        super::values::flatten(lua, value, &what, &mut flattened)?;
    }
    let mut sources = Vec::new();
    for value in flattened {
        let export = match &value {
            Value::UserData(userdata) => userdata.borrow::<Export>().ok().map(|export| export.0.clone()),
            _ => None,
        };
        match export {
            Some(_) if kind == TargetKind::Executable => {
                return Err(fail(lua, "exports is only allowed in a library"));
            }
            Some(pattern) => sources.push(SourcePattern { pattern, exported: true }),
            None => sources.push(SourcePattern { pattern: string(lua, &value, "source pattern")?, exported: false }),
        }
    }
    if sources.is_empty() {
        return Err(fail(lua, format!("{what} has no source patterns")));
    }

    let directory = state.borrow().directory();
    let mut target = TargetConfig {
        name,
        kind,
        sources,
        usage: Usage::default(),
        deps: Vec::new(),
        default: true,
    };
    for (key, value) in entries.keyed {
        match key.as_str() {
            "default" => target.default = boolean(lua, &value, "default")?,
            "deps" => target.deps.extend(deps(lua, value)?),
            other if usage(lua, &directory, &mut target.usage, other, value)? => {}
            other => return Err(unknown_key(lua, other, &what)),
        }
    }

    let mut state = state.borrow_mut();
    state.targets.push(target);
    Ok(Handle {
        reference: DependencyRef::Target(state.targets.len() - 1),
        executable: kind == TargetKind::Executable,
    })
}

fn dependency(lua: &Lua, state: &Shared, value: Value) -> Result<Handle> {
    let entries = entries(lua, &table(lua, value, "dependency")?, "dependency")?;
    if !entries.positional.is_empty() {
        return Err(fail(lua, "dependency only accepts named fields"));
    }
    let directory = state.borrow().directory();
    let mut dependency = DependencyConfig { usage: Usage::default(), deps: Vec::new() };
    for (key, value) in entries.keyed {
        match key.as_str() {
            "deps" => dependency.deps.extend(deps(lua, value)?),
            other if usage(lua, &directory, &mut dependency.usage, other, value)? => {}
            other => return Err(unknown_key(lua, other, "dependency")),
        }
    }

    let mut state = state.borrow_mut();
    state.dependencies.push(dependency);
    Ok(Handle {
        reference: DependencyRef::Bundle(state.dependencies.len() - 1),
        executable: false,
    })
}

fn import(lua: &Lua, state: &Shared, loaded: &Table, value: Value) -> Result<Value> {
    let relative = string(lua, &value, "import")?;
    let path = state.borrow().directory().join(&relative);
    let path = path
        .canonicalize()
        .map_err(|error| fail(lua, format!("cannot import '{relative}': {error}")))?;
    let key = path.to_string_lossy().to_string();

    if let Value::Table(cached) = loaded.raw_get::<Value>(key.as_str())? {
        return cached.raw_get(1);
    }
    if state.borrow().files.contains(&path) {
        return Err(fail(lua, format!("import cycle through '{relative}'")));
    }

    let value = run_file(lua, state, &path)?;
    let cached = lua.create_table()?;
    cached.raw_set(1, value.clone())?;
    loaded.raw_set(key, cached)?;
    Ok(value)
}

fn assemble(state: State) -> ProjectConfig {
    let declaration = state.project.unwrap_or_default();
    let profile = |mut settings: ProfileSettings, optimization: OptimizationLevel| {
        settings.optimization.get_or_insert(optimization);
        if settings.backend.is_none() {
            settings.backend = declaration.backend;
        }
        settings
    };
    let debug = profile(declaration.debug.clone(), OptimizationLevel::O0);
    let release = profile(declaration.release.clone(), OptimizationLevel::O2);

    let name = declaration.name.clone().unwrap_or_else(|| {
        state
            .root
            .file_name()
            .map(|name| name.to_string_lossy().to_string())
            .unwrap_or_else(|| "project".into())
    });
    ProjectConfig {
        source_root: declaration.source_root.clone().unwrap_or_else(|| state.root.clone()),
        root: state.root,
        name,
        debug,
        release,
        targets: state.targets,
        dependencies: state.dependencies,
    }
}
