use std::cell::Cell;

// Modules are compiled single-threaded, but multiple modules can be compiled
// in parallel, so this counter is thread-local and cannot collide across modules.
thread_local! {
    static NUM: Cell<usize> = const { Cell::new(0) };
}

pub(crate) fn reset_num() {
    NUM.with(|num| num.set(0));
}

pub(crate) fn inst_num() -> String {
    format!(
        "inst_{}",
        NUM.with(|num| {
            let current = num.get();
            num.set(current + 1);
            current
        })
    )
}
