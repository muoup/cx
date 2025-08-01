use std::fs::OpenOptions;
use std::io::Write;
use std::sync::Mutex;

pub fn dump_data(data: &impl std::fmt::Display) {
    dump_write(&format!("{data}\n\n"));
}

pub fn dump_all(data: impl Iterator<Item = impl std::fmt::Display>) {
    let data = data
        .into_iter()
        .map(|d| format!("{d}\n"))
        .collect::<Vec<String>>()
        .join("\n");

    dump_write(&data);

    dump_write("\n\n\n//////////////\n\n\n\n");
}


pub fn dump_write(str: &str) {
    const DUMP_PATH: &str = ".internal/compiler-dump.data";
    
    let path = std::path::Path::new(DUMP_PATH);
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    
    let mut dump_file =
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(DUMP_PATH)
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
                fwrite!($f, "\t");
            }

            val
        }
    };
}