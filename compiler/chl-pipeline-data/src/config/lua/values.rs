use std::fmt::Display;

use mlua::{Error, Lua, Result, Table, Value};

use crate::config::schema::{LinkEntry, LinkKind};

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
        Value::UserData(_) => "a handle",
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

pub(super) fn define(lua: &Lua, name: &Value, value: &Value) -> Result<Option<(String, String)>> {
    let name = macro_name(lua, string(lua, name, "define")?)?;
    let value = match value {
        Value::Nil | Value::Boolean(true) => "1".into(),
        Value::Boolean(false) => return Ok(None),
        Value::String(string) => string.to_str()?.to_string(),
        Value::Integer(integer) => integer.to_string(),
        Value::Number(number) => number.to_string(),
        other => {
            return Err(fail(lua, format!("expected a define value for '{name}', found {}", describe(other))));
        }
    };
    Ok(Some((name, value)))
}

pub(super) fn identifier(lua: &Lua, value: &Value, what: &str) -> Result<String> {
    let name = string(lua, value, what)?;
    let mut characters = name.chars();
    let valid = characters
        .next()
        .is_some_and(|first| first == '_' || first.is_ascii_alphabetic())
        && characters.all(|character| character == '_' || character.is_ascii_alphanumeric());
    match valid {
        true => Ok(name),
        false => Err(fail(lua, format!("{what} '{name}' is not a valid identifier"))),
    }
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

