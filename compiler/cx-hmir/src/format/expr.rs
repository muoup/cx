use crate::expr::{
    HMIRExprID, HMIRExprKind,
    op::{HMIRBinaryOp, HMIRControlOp, HMIROp},
};

use super::{
    emit::Emitter,
    native::{Arg, render_args},
};

impl Emitter<'_> {
    pub(super) fn atom(&mut self, id: HMIRExprID) -> String {
        if let Some(temp) = self.memo(id) {
            return temp;
        }
        let printer = self.printer;
        match printer.body().expr(id).kind() {
            HMIRExprKind::Literal(_)
            | HMIRExprKind::Local(_)
            | HMIRExprKind::Def(_)
            | HMIRExprKind::Name { .. }
            | HMIRExprKind::Hole(_)
            | HMIRExprKind::Type(_)
            | HMIRExprKind::ScopeAccess { .. }
            | HMIRExprKind::Error(_) => printer.inline(id),

            HMIRExprKind::Comptime(inner) => {
                let inner = self.comptime_scope(true, |emitter| emitter.atom(*inner));
                format!("comptime({inner})")
            }
            HMIRExprKind::Quote { params, body } => {
                let decls = params
                    .iter()
                    .map(|param| printer.local_decl(*param))
                    .collect::<Vec<_>>();
                let params = match decls.is_empty() {
                    true => String::new(),
                    false => format!("({})", decls.join(", ")),
                };
                let lines = self.comptime_scope(true, |emitter| {
                    emitter.nested(|emitter| emitter.region(*body, !emitter.is_block(*body)))
                });
                format!("quote{params} {{\n{lines}{}}}", self.closing_indent())
            }
            HMIRExprKind::Splice { quote, args } => {
                let quote = self.atom(*quote);
                let args = args.iter().map(|arg| self.atom(*arg)).collect::<Vec<_>>();
                format!("splice({quote})({})", args.join(", "))
            }
            HMIRExprKind::Piped(value) => format!("piped({})", self.atom(*value)),

            HMIRExprKind::Intrinsic(intrinsic) => {
                let args = printer.intrinsic_args(intrinsic);
                let args = self.render(&args);
                self.assign_temp(id, format!("{}({})", intrinsic.path(), args.join(", ")))
            }
            HMIRExprKind::Op(op) => self.op(id, op),
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => {
                let callee = self.atom(*callee);
                let args = args.iter().map(|arg| Arg::Value(*arg));
                let named = named
                    .iter()
                    .map(|(name, value)| Arg::Named(name.clone(), *value));
                let args = self.render(&args.chain(named).collect::<Vec<_>>());
                self.assign_temp(id, format!("{callee}({})", args.join(", ")))
            }
            HMIRExprKind::Let { local, initializer } => {
                let initializer = initializer.map(|initializer| {
                    let comptime = printer.body().local(*local).comptime();
                    self.comptime_scope(comptime, |emitter| emitter.atom(initializer))
                });
                let initializer = initializer.map(|value| format!(" = {value}"));
                self.line(format!(
                    "local {}{}",
                    printer.local_decl(*local),
                    initializer.unwrap_or_default()
                ));
                printer.local(*local)
            }

            HMIRExprKind::Block { .. }
            | HMIRExprKind::If { .. }
            | HMIRExprKind::While { .. }
            | HMIRExprKind::For { .. }
            | HMIRExprKind::Switch { .. }
            | HMIRExprKind::Case { .. }
            | HMIRExprKind::Match { .. }
            | HMIRExprKind::Label { .. } => self.flow(id, true),
        }
    }

    pub(super) fn stmt(&mut self, id: HMIRExprID) {
        match self.printer.body().expr(id).kind() {
            HMIRExprKind::Literal(_)
            | HMIRExprKind::Local(_)
            | HMIRExprKind::Def(_)
            | HMIRExprKind::Name { .. }
            | HMIRExprKind::Hole(_) => {}
            HMIRExprKind::Comptime(inner) => {
                self.open("comptime {");
                self.comptime_scope(true, |emitter| emitter.region(*inner, false));
                self.close();
            }
            HMIRExprKind::Block { .. }
            | HMIRExprKind::If { .. }
            | HMIRExprKind::While { .. }
            | HMIRExprKind::For { .. }
            | HMIRExprKind::Switch { .. }
            | HMIRExprKind::Case { .. }
            | HMIRExprKind::Match { .. }
            | HMIRExprKind::Label { .. } => {
                self.flow(id, false);
            }
            HMIRExprKind::Op(HMIROp::Control(
                HMIRControlOp::Defer(_) | HMIRControlOp::Unsafe(_),
            )) => {
                self.flow(id, false);
            }
            _ => {
                self.atom(id);
            }
        }
    }

    pub(super) fn region(&mut self, id: HMIRExprID, valued: bool) {
        let body = self.printer.body();
        match body.expr(id).kind() {
            HMIRExprKind::Block {
                statements, yields, ..
            } => match (valued && !yields, statements.split_last()) {
                (true, Some((last, rest))) => {
                    rest.iter().for_each(|statement| self.stmt(*statement));
                    self.yield_atom(*last);
                }
                _ => statements
                    .iter()
                    .for_each(|statement| self.stmt(*statement)),
            },
            _ if valued => self.yield_atom(id),
            _ => self.stmt(id),
        }
    }

    pub(super) fn is_block(&self, id: HMIRExprID) -> bool {
        matches!(
            self.printer.body().expr(id).kind(),
            HMIRExprKind::Block { .. }
        )
    }

    pub(super) fn yield_atom(&mut self, id: HMIRExprID) {
        match self.printer.body().expr(id).kind() {
            HMIRExprKind::Op(HMIROp::Control(_)) => self.stmt(id),
            _ => {
                let atom = self.atom(id);
                self.line(format!("yield {atom}"));
            }
        }
    }

    fn render(&mut self, args: &[Arg]) -> Vec<String> {
        let printer = self.printer;
        render_args(args, &mut |id, place| match place {
            true => printer.place_with(id, &mut |id| self.atom(id)),
            false => self.atom(id),
        })
    }

    fn op(&mut self, id: HMIRExprID, op: &HMIROp) -> String {
        let printer = self.printer;
        match op {
            op if printer.is_place(op) => printer.place_with(id, &mut |id| self.atom(id)),
            HMIROp::BinOp {
                op: op @ (HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr),
                lhs,
                rhs,
            } => {
                let lhs = self.atom(*lhs);
                let temp = self.fresh(id);
                self.open(format!("{temp} = {} {lhs} {{", op.path()));
                self.yield_atom(*rhs);
                self.close();
                temp
            }
            HMIROp::Control(HMIRControlOp::Defer(_) | HMIRControlOp::Unsafe(_)) => {
                self.flow(id, true)
            }
            HMIROp::Control(HMIRControlOp::LabelAddress { .. }) => {
                let (head, args) = printer.native_parts(op);
                let args = self.render(&args);
                self.assign_temp(id, format!("{head} {}", args.join(", ")))
            }
            _ => {
                let (head, args) = printer.native_parts(op);
                let mut args = self.render(&args);
                let effect = matches!(op, HMIROp::Assign { .. } | HMIROp::Control(_));
                let text = match args.is_empty() {
                    true => head,
                    false => format!("{head} {}", args.join(", ")),
                };
                match (effect, op) {
                    (false, _) => self.assign_temp(id, text),
                    (true, HMIROp::Assign { .. }) => {
                        self.line(text);
                        args.swap_remove(0)
                    }
                    (true, _) => {
                        self.line(text);
                        "()".to_string()
                    }
                }
            }
        }
    }
}
