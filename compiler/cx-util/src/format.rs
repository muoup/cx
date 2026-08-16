use std::cell::{Cell, RefCell};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Mutex;

thread_local! {
    static DUMP_DIRECTORY: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
    static DUMP_FILE: RefCell<Option<PathBuf>> = const { RefCell::new(None) };
    static DUMP_ENABLED: Cell<bool> = const { Cell::new(true) };
}

struct DumpDirectoryGuard(Option<PathBuf>);

impl Drop for DumpDirectoryGuard {
    fn drop(&mut self) {
        DUMP_DIRECTORY.with(|cell| {
            cell.replace(self.0.take());
        });
    }
}

struct DumpFileGuard(Option<PathBuf>);

impl Drop for DumpFileGuard {
    fn drop(&mut self) {
        DUMP_FILE.with(|cell| {
            cell.replace(self.0.take());
        });
    }
}

struct DumpEnabledGuard(bool);

impl Drop for DumpEnabledGuard {
    fn drop(&mut self) {
        DUMP_ENABLED.with(|cell| cell.set(self.0));
    }
}

pub fn with_dump_directory<T>(path: PathBuf, f: impl FnOnce() -> T) -> T {
    let previous = DUMP_DIRECTORY.with(|cell| cell.replace(Some(path)));
    let _guard = DumpDirectoryGuard(previous);
    f()
}

pub fn with_dump_file<T>(path: PathBuf, f: impl FnOnce() -> T) -> T {
    let previous = DUMP_FILE.with(|cell| cell.replace(Some(path)));
    let _guard = DumpFileGuard(previous);
    f()
}

pub fn without_dumps<T>(f: impl FnOnce() -> T) -> T {
    let previous = DUMP_ENABLED.with(|cell| cell.replace(false));
    let _guard = DumpEnabledGuard(previous);
    f()
}

fn dumps_enabled() -> bool {
    DUMP_ENABLED.with(Cell::get)
}

pub fn dump_data(data: &impl std::fmt::Display) {
    if !dumps_enabled() {
        return;
    }
    dump_write(&format!("{data}\n\n"));
}

pub fn dump_all(data: impl Iterator<Item = impl std::fmt::Display>) {
    if !dumps_enabled() {
        return;
    }
    let data = data
        .into_iter()
        .map(|d| format!("{d}\n"))
        .collect::<Vec<String>>()
        .join("\n");

    dump_write(&data);

    dump_write("\n\n\n//////////////\n\n\n\n");
}

pub fn dump_write(str: &str) {
    if !dumps_enabled() {
        return;
    }

    let dump_path = DUMP_FILE
        .with(|cell| cell.borrow().clone())
        .or_else(|| {
            DUMP_DIRECTORY.with(|cell| {
                cell.borrow()
                    .clone()
                    .map(|path| path.join("compiler-dump.data"))
            })
        })
        .unwrap_or_else(|| PathBuf::from(".internal/compiler-dump.data"));
    if let Some(parent) = dump_path.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }

    let mut dump_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(dump_path)
        .expect("Failed to open dump file");

    dump_file
        .write_all(str.as_bytes())
        .expect("Failed to write to dump file");
}

pub fn static_ident() -> &'static Mutex<usize> {
    static STATIC_IDENT: Mutex<usize> = Mutex::new(0);
    &STATIC_IDENT
}

pub fn indent() {
    let mut static_ident = static_ident().lock().unwrap();
    *static_ident += 1;
}

pub fn dedent() {
    let mut static_ident = static_ident().lock().unwrap();
    if *static_ident > 0 {
        *static_ident -= 1;
    }
}

#[macro_export]
macro_rules! fwrite {
    ($f:expr, $($args:expr),+) => {
        write!($f, $($args),*)
    };
}

#[macro_export]
macro_rules! fwriteln {
    ($f:expr, $($args:tt),+) => {
        {
            let val = writeln!($f, $($args),*);

            let static_ident = cx_util::format::static_ident().lock().unwrap();
            for _ in 0..*static_ident {
                fwrite!($f, "\t")?;
            }

            val
        }
    };
}
