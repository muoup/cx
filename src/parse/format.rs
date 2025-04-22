use std::sync::Mutex;

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
    ($f:expr, $($args:tt),+) => {
        write!($f, $($args),*)
    };
}

#[macro_export]
macro_rules! fwriteln {
    ($f:expr, $($args:tt),+) => {
        {
            let val = writeln!($f, $($args),*);

            let static_ident = crate::parse::format::static_ident().lock().unwrap();
            for _ in 0..*static_ident {
                fwrite!($f, "\t");
            }

            val
        }
    };
}