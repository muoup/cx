use rustc_hash::FxHashMap;
use std::mem;

use crate::expr::HMIRExprID;

use super::body::BodyPrinter;

pub(super) struct Emitter<'a> {
    pub(super) printer: BodyPrinter<'a>,
    out: String,
    depth: usize,
    temps: FxHashMap<HMIRExprID, String>,
    counts: [usize; 2],
    comptime: bool,
}

impl<'a> Emitter<'a> {
    pub(super) fn new(printer: BodyPrinter<'a>) -> Self {
        Self {
            printer,
            out: String::new(),
            depth: 0,
            temps: FxHashMap::default(),
            counts: [0; 2],
            comptime: false,
        }
    }

    pub(super) fn body(&mut self, root: HMIRExprID) -> String {
        let lines = self.nested(|emitter| emitter.region(root, false));
        format!("{{\n{lines}}}")
    }

    pub(super) fn value(&mut self, id: HMIRExprID) -> String {
        let mut atom = String::new();
        let lines = self.nested(|emitter| atom = emitter.atom(id));
        match lines.is_empty() {
            true => atom,
            false => format!("{{\n{lines}    yield {atom}\n}}"),
        }
    }

    pub(super) fn nested(&mut self, emit: impl FnOnce(&mut Self)) -> String {
        let outer = mem::take(&mut self.out);
        self.depth += 1;
        emit(self);
        self.depth -= 1;
        mem::replace(&mut self.out, outer)
    }

    pub(super) fn closing_indent(&self) -> String {
        "    ".repeat(self.depth)
    }

    pub(super) fn line(&mut self, text: impl AsRef<str>) {
        self.out.push_str(&self.closing_indent());
        self.out.push_str(text.as_ref());
        self.out.push('\n');
    }

    pub(super) fn open(&mut self, header: impl AsRef<str>) {
        self.line(header);
        self.depth += 1;
    }

    pub(super) fn reopen(&mut self, header: &str) {
        self.depth -= 1;
        self.line(header);
        self.depth += 1;
    }

    pub(super) fn close(&mut self) {
        self.depth -= 1;
        self.line("}");
    }

    pub(super) fn comptime_scope<R>(
        &mut self,
        enabled: bool,
        emit: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let outer = self.comptime;
        self.comptime |= enabled;
        let result = emit(self);
        self.comptime = outer;
        result
    }

    pub(super) fn fresh(&mut self, id: HMIRExprID) -> String {
        let count = &mut self.counts[self.comptime as usize];
        let temp = format!("{}t{count}", if self.comptime { "$" } else { "%" });
        *count += 1;
        self.temps.insert(id, temp.clone());
        temp
    }

    pub(super) fn memo(&self, id: HMIRExprID) -> Option<String> {
        self.temps.get(&id).cloned()
    }

    pub(super) fn assign_temp(&mut self, id: HMIRExprID, text: impl AsRef<str>) -> String {
        let temp = self.fresh(id);
        self.line(format!("{temp} = {}", text.as_ref()));
        temp
    }
}
