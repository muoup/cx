use cx_data_ast::parse::ast::CXExpr;
use cx_util::log_error;

pub(crate) fn reformat_lvalue(expr: CXExpr) -> Option<CXExpr> {
    Some(
        match expr {
            // Identity LValues -> i.e. identical to how they are interpreted as RValues
            CXExpr::Identifier(_) => expr,

            _ => log_error!("Unsupported LValue: {:#?}", expr),
        }
    )
}