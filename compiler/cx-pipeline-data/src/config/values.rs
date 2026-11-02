use std::fmt::Display;
use std::path::{Path, PathBuf};

use mlua::{Error, Lua, Result, Table, UserData, Value};

use super::schema::{DependencyRef, LinkEntry, LinkKind, Usage};

#[derive(Debug, Clone, Copy)]
pub(super) struct Handle {
    pub reference: DependencyRef,
    pub executable: bool,
}

impl UserData for Handle {}

#[derive(Debug, Clone)]
pub(super) struct Export(pub String);

impl UserData for Export {}

pub(super) struct Entries {
    pub positional: Vec<Value>,
    pub keyed: Vec<(String, Value)>,
}

pub(super) fn fail(lua: &Lua, message: impl Display) -> Error {
    let location = lua
        .inspect_stack(1, |debug| {
            let source = debug.source().short_src.map(|source| source.into_owned());
            source.zip(debug.current_line())
        })
        .flatten();
    match location {
        Some((source, line)) => Error::runtime(format!("{source}:{line}: {message}")),
        None => Error::runtime(message.to_string()),
    }
}

pub(super) fn error_message(error: &Error) -> String {
    match error {
        Error::CallbackError { cause, .. } => error_message(cause),
        Error::RuntimeError(message) => message.clone(),
        Error::SyntaxError { message, .. } => message.clone(),
        Error::MemoryError(_) => "configuration exceeded its memory budget".into(),
        other => other.to_string(),
    }
}

pub(super) fn describe(value: &Value) -> &'static str {
    match value {
        Value::Nil => "nil",
        Value::Boolean(_) => "a boolean",
        Value::Integer(_) | Value::Number(_) => "a number",
        Value::String(_) => "a string",
        Value::Table(_) => "a table",
        Value::Function(_) => "a function",
        Value::UserData(userdata) if userdata.is::<Handle>() => "a target or dependency",
        Value::UserData(userdata) if userdata.is::<Export>() => "an exports entry",
        _ => "an unsupported value",
    }
}

pub(super) fn entries(lua: &Lua, table: &Table, what: &str) -> Result<Entries> {
    let length = table.raw_len();
    let positional = (1..=length)
        .map(|index| table.raw_get(index))
        .collect::<Result<Vec<Value>>>()?;
    let mut keyed = Vec::new();
    for pair in table.pairs::<Value, Value>() {
        let (key, value) = pair?;
        match key {
            Value::Integer(index) if index >= 1 && index as usize <= length => {}
            Value::String(key) => keyed.push((key.to_str()?.to_string(), value)),
            other => {
                return Err(fail(lua, format!("unexpected {} key in {what}", describe(&other))));
            }
        }
    }
    keyed.sort_by(|(left, _), (right, _)| left.cmp(right));
    Ok(Entries { positional, keyed })
}

pub(super) fn unknown_key(lua: &Lua, key: &str, what: &str) -> Error {
    fail(lua, format!("unknown key '{key}' in {what}"))
}

pub(super) fn string(lua: &Lua, value: &Value, what: &str) -> Result<String> {
    match value {
        Value::String(string) => Ok(string.to_str()?.to_string()),
        other => Err(fail(lua, format!("expected a string for {what}, found {}", describe(other)))),
    }
}

pub(super) fn boolean(lua: &Lua, value: &Value, what: &str) -> Result<bool> {
    match value {
        Value::Boolean(boolean) => Ok(*boolean),
        other => Err(fail(lua, format!("expected a boolean for {what}, found {}", describe(other)))),
    }
}

pub(super) fn table(lua: &Lua, value: Value, what: &str) -> Result<Table> {
    match value {
        Value::Table(table) => Ok(table),
        other => Err(fail(lua, format!("expected a table for {what}, found {}", describe(&other)))),
    }
}

pub(super) fn flatten(lua: &Lua, value: Value, what: &str, out: &mut Vec<Value>) -> Result<()> {
    let Value::Table(table) = value else {
        out.push(value);
        return Ok(());
    };
    let entries = entries(lua, &table, what)?;
    if let Some((key, _)) = entries.keyed.first() {
        return Err(unknown_key(lua, key, what));
    }
    for value in entries.positional {
        flatten(lua, value, what, out)?;
    }
    Ok(())
}

pub(super) fn strings(lua: &Lua, value: Value, what: &str) -> Result<Vec<String>> {
    let mut values = Vec::new();
    flatten(lua, value, what, &mut values)?;
    values.iter().map(|value| string(lua, value, what)).collect()
}

pub(super) fn paths(lua: &Lua, directory: &Path, value: Value, what: &str) -> Result<Vec<PathBuf>> {
    Ok(strings(lua, value, what)?
        .into_iter()
        .map(|path| directory.join(path))
        .collect())
}

pub(super) fn defines(lua: &Lua, value: Value) -> Result<Vec<(String, String)>> {
    let entries = match value {
        Value::Table(table) => entries(lua, &table, "defines")?,
        other => Entries { positional: vec![other], keyed: Vec::new() },
    };
    let mut defines = Vec::new();
    for value in entries.positional {
        let definition = string(lua, &value, "defines")?;
        let (name, value) = definition
            .split_once('=')
            .map(|(name, value)| (name.to_string(), value.to_string()))
            .unwrap_or_else(|| (definition.clone(), "1".into()));
        defines.push((macro_name(lua, name)?, value));
    }
    for (name, value) in entries.keyed {
        let value = match value {
            Value::String(string) => string.to_str()?.to_string(),
            Value::Integer(integer) => integer.to_string(),
            Value::Number(number) => number.to_string(),
            Value::Boolean(true) => "1".into(),
            Value::Boolean(false) => continue,
            other => {
                return Err(fail(lua, format!("expected a define value for '{name}', found {}", describe(&other))));
            }
        };
        defines.push((macro_name(lua, name)?, value));
    }
    Ok(defines)
}

fn macro_name(lua: &Lua, name: String) -> Result<String> {
    let mut characters = name.chars();
    let valid = characters
        .next()
        .is_some_and(|first| first == '_' || first.is_ascii_alphabetic())
        && characters.all(|character| character == '_' || character.is_ascii_alphanumeric());
    match valid {
        true => Ok(name),
        false => Err(fail(lua, format!("'{name}' is not a valid macro name"))),
    }
}

pub(super) fn links(lua: &Lua, value: Value) -> Result<Vec<LinkEntry>> {
    let table = match value {
        Value::String(_) => {
            return Ok(vec![LinkEntry { name: string(lua, &value, "link")?, kind: LinkKind::System }]);
        }
        Value::Table(table) => table,
        other => {
            return Err(fail(lua, format!("expected a string or table for link, found {}", describe(&other))));
        }
    };
    let entries = entries(lua, &table, "link")?;
    if entries.keyed.is_empty() {
        let mut links = Vec::new();
        for value in entries.positional {
            links.extend(links_entry(lua, value)?);
        }
        return Ok(links);
    }

    let mut kind = LinkKind::System;
    for (key, value) in &entries.keyed {
        match key.as_str() {
            "kind" => kind = link_kind(lua, &string(lua, value, "link kind")?)?,
            other => return Err(unknown_key(lua, other, "link entry")),
        }
    }
    let [name] = entries.positional.as_slice() else {
        return Err(fail(lua, "a link entry with a kind must name exactly one library"));
    };
    Ok(vec![LinkEntry { name: string(lua, name, "link")?, kind }])
}

fn links_entry(lua: &Lua, value: Value) -> Result<Vec<LinkEntry>> {
    match value {
        Value::String(_) | Value::Table(_) => links(lua, value),
        other => Err(fail(lua, format!("expected a string or table for link, found {}", describe(&other)))),
    }
}

fn link_kind(lua: &Lua, kind: &str) -> Result<LinkKind> {
    match kind {
        "system" => Ok(LinkKind::System),
        "static" => Ok(LinkKind::Static),
        "dynamic" => Ok(LinkKind::Dynamic),
        other => Err(fail(lua, format!("unknown link kind '{other}', expected system, static or dynamic"))),
    }
}

pub(super) fn deps(lua: &Lua, value: Value) -> Result<Vec<DependencyRef>> {
    let mut values = Vec::new();
    flatten(lua, value, "deps", &mut values)?;
    values
        .into_iter()
        .map(|value| {
            let handle = match &value {
                Value::UserData(userdata) => userdata.borrow::<Handle>().ok().map(|handle| *handle),
                _ => None,
            };
            match handle {
                Some(handle) if handle.executable => Err(fail(lua, "an executable cannot be a dependency")),
                Some(handle) => Ok(handle.reference),
                None => Err(fail(lua, format!("expected a target or dependency in deps, found {}", describe(&value)))),
            }
        })
        .collect()
}

pub(super) fn usage(lua: &Lua, directory: &Path, usage: &mut Usage, key: &str, value: Value) -> Result<bool> {
    match key {
        "include" => usage.include.extend(paths(lua, directory, value, "include")?),
        "objects" => usage.objects.extend(paths(lua, directory, value, "objects")?),
        "defines" => usage.defines.extend(defines(lua, value)?),
        "link" => usage.link.extend(links(lua, value)?),
        _ => return Ok(false),
    }
    Ok(true)
}
