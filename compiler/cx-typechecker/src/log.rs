use cx_parsing_data::ast::CXExpr;
use cx_util::CXErrorTrait;

use crate::environment::TCEnvironment;

pub struct TypeError<'a> {
    pub env: &'a TCEnvironment<'a>,
    pub expr: &'a CXExpr,
    pub message: String,
}

impl CXErrorTrait for TypeError<'_> {
    fn pretty_print(&self) {
        cx_log::pretty_underline_error(&self.message, self.env.compilation_unit.as_path(), self.env.tokens, self.expr.start_index, self.expr.end_index);
    }
}

#[macro_export]
macro_rules! log_typecheck_error {
    ($env:expr, $expr:expr, $($arg:tt)*) => {
        {
            let message = format!("TYPE ERROR: {}", format!($($arg)*));

            Err(Box::new(crate::log::TypeError {
                expr: $expr,
                message: message,
                env: $env,
            }) as Box<dyn cx_util::CXErrorTrait>)
        }
    };
}
