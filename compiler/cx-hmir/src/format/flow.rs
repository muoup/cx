use crate::expr::{
    HMIRExprID, HMIRExprKind,
    op::{HMIRControlOp, HMIROp},
};

use super::emit::Emitter;

const NONE: &str = "()";

impl Emitter<'_> {
    pub(super) fn flow(&mut self, id: HMIRExprID, valued: bool) -> String {
        let printer = self.printer;
        match printer.body().expr(id).kind() {
            HMIRExprKind::Block {
                statements,
                scoped,
                yields,
            } => {
                if !scoped && !yields {
                    return match (valued, statements.split_last()) {
                        (true, Some((last, rest))) => {
                            rest.iter().for_each(|statement| self.stmt(*statement));
                            self.atom(*last)
                        }
                        _ => {
                            statements
                                .iter()
                                .for_each(|statement| self.stmt(*statement));
                            NONE.to_string()
                        }
                    };
                }
                self.region_block(id, valued)
            }
            HMIRExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.atom(*condition);
                let temp = valued.then(|| self.fresh(id));
                self.open(format!("{}if {condition} {{", assignment(&temp)));
                self.region(*then_branch, valued);
                if let Some(else_branch) = else_branch {
                    self.reopen("} else {");
                    self.region(*else_branch, valued);
                }
                self.close();
                temp.unwrap_or_else(|| NONE.to_string())
            }
            HMIRExprKind::While {
                condition,
                body,
                pre_eval: true,
            } => {
                self.open("while {");
                self.yield_atom(*condition);
                self.reopen("} do {");
                self.region(*body, false);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::While {
                condition,
                body,
                pre_eval: false,
            } => {
                self.open("do {");
                self.region(*body, false);
                self.reopen("} while {");
                self.yield_atom(*condition);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                self.open("for {");
                self.region(*init, false);
                self.reopen("} while {");
                self.yield_atom(*condition);
                self.reopen("} step {");
                self.region(*increment, false);
                self.reopen("} do {");
                self.region(*body, false);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::Switch { condition, body } => {
                let condition = self.atom(*condition);
                self.open(format!("switch {condition} {{"));
                self.region(*body, false);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::Case { value, body } => {
                match value {
                    Some(value) => {
                        let value = self.atom(*value);
                        self.open(format!("case {value}: {{"));
                    }
                    None => self.open("default: {"),
                }
                self.region(*body, false);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::Label { name, body } => {
                self.open(format!("{name}: {{"));
                self.region(*body, false);
                self.close();
                NONE.to_string()
            }
            HMIRExprKind::Match {
                scrutinee,
                subject,
                arms,
            } => {
                let scrutinee = self.atom(*scrutinee);
                let temp = valued.then(|| self.fresh(id));
                self.open(format!(
                    "{}match {scrutinee} as {} {{",
                    assignment(&temp),
                    printer.local(*subject)
                ));
                for (pattern, body) in arms {
                    self.open(format!("{} => {{", printer.pattern(pattern)));
                    self.region(*body, valued);
                    self.close();
                }
                self.close();
                temp.unwrap_or_else(|| NONE.to_string())
            }
            HMIRExprKind::Op(HMIROp::Control(
                op @ (HMIRControlOp::Defer(body) | HMIRControlOp::Unsafe(body)),
            )) => {
                let keyword = op.path();
                let valued = valued && matches!(op, HMIRControlOp::Unsafe(_));
                let temp = valued.then(|| self.fresh(id));
                self.open(format!("{}{keyword} {{", assignment(&temp)));
                self.region(*body, valued);
                self.close();
                temp.unwrap_or_else(|| NONE.to_string())
            }
            _ => NONE.to_string(),
        }
    }

    fn region_block(&mut self, id: HMIRExprID, valued: bool) -> String {
        let temp = valued.then(|| self.fresh(id));
        self.open(format!("{}{{", assignment(&temp)));
        self.region(id, valued);
        self.close();
        temp.unwrap_or_else(|| NONE.to_string())
    }
}

fn assignment(temp: &Option<String>) -> String {
    temp.as_ref()
        .map(|temp| format!("{temp} = "))
        .unwrap_or_default()
}
