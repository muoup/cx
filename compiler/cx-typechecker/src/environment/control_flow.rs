use cx_log::error::{CXRawResult, message::CXStdErrMessage};
use cx_thir::thir::r#type::THIRType;
use cx_tokens::TokenRange;
use cx_util::dense_id;

dense_id!(ScopeId);

pub struct ControlFlow {
    scope_stack: Vec<Scope>,
}

#[derive(Clone)]
pub struct Scope {
    has_break_merge: bool,
    has_continue_merge: bool,

    anchor_range: TokenRange,

    breaks: bool,
    continues: bool,
    yields: Vec<YieldRecord>
}

#[derive(Clone)]
pub struct YieldRecord {
    ty: THIRType,
    range: TokenRange,
}

impl ControlFlow {
    pub fn new() -> Self {
        Self {
            scope_stack: vec![]
        }
    }

    pub fn push_scope(
        &mut self,
        has_break_merge: bool,
        has_continue_merge: bool,
        anchor_range: TokenRange,
    ) {
        self.scope_stack.push(Scope {
            has_break_merge,
            has_continue_merge,
            anchor_range,

            breaks: false,
            continues: false,
            yields: vec![]
        });
    }

    pub fn pop_scope(&mut self) -> CXRawResult<()> {
        let Some(old_scope) = self.scope_stack.pop() else {
            return CXStdErrMessage::result(
                "TYPE ERROR",
                "Attempted to pop a scope from an empty scope stack".to_string(),
            );
        };

        if let Some(new_top_scope) = self.scope_stack.last_mut() {
            new_top_scope.breaks |= old_scope.breaks;
            new_top_scope.continues |= old_scope.continues;
            new_top_scope.yields.extend(old_scope.yields);
        }

        Ok(())
    }

    pub fn record_break(&mut self) -> CXRawResult<()> {
        if !self.can_break() {
            return CXStdErrMessage::result(
                "TYPE ERROR",
                "Attempted to break outside of a loop".to_string(),
            );
        }

        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.breaks = true;
        }

        Ok(())
    }

    pub fn record_continue(&mut self) -> CXRawResult<()> {
        if !self.can_continue() {
            return CXStdErrMessage::result(
                "TYPE ERROR",
                "Attempted to continue outside of a loop".to_string(),
            );
        }

        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.continues = true;
        }

        Ok(())
    }

    pub fn record_yield(&mut self, tokens: TokenRange, ty: THIRType) -> CXRawResult<()> {
        if let Some(current_scope) = self.scope_stack.last_mut() {
            current_scope.yields.push(YieldRecord { ty, range: tokens });
        }

        Ok(())
    }

    pub fn current_scope_index(&self) -> ScopeId {
        ScopeId(self.scope_stack.len() - 1)
    }

    pub fn can_break(&self) -> bool {
        self.scope_stack
            .iter()
            .rev()
            .any(|scope| scope.has_break_merge)
    }

    pub fn can_continue(&self) -> bool {
        self.scope_stack
            .iter()
            .rev()
            .any(|scope| scope.has_continue_merge)
    }
}
