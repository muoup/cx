use crate::log_error;
use crate::parse::ast::ValueType;
use crate::parse::pass_molded::CXExpr;
use crate::parse::pass_typecheck::TypeEnvironment;

pub(crate) fn type_check_traverse(env: &mut TypeEnvironment, expr: &mut CXExpr) -> Option<ValueType> {
    match expr {
        CXExpr::Block { exprs } => {
            for expr in exprs {
                type_check_traverse(env, expr)?;
            }

            Some(ValueType::Unit)
        },

        CXExpr::Assignment { lhs, rhs, .. } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            let rhs_type = type_check_traverse(env, rhs)?;

            implicit_cast(rhs.as_mut(), &rhs_type, lhs_type.clone())?;

            Some(lhs_type)
        },
        CXExpr::BinOp { lhs, rhs, .. } => {
            let lhs_type = type_check_traverse(env, lhs)?;
            let rhs_type = type_check_traverse(env, rhs)?;

            implicit_cast(lhs, &lhs_type, lhs_type.clone())?;
            implicit_cast(rhs, &rhs_type, lhs_type.clone())?;

            Some(lhs_type)
        },

        CXExpr::VarDeclaration { name, type_, initializer } => {
            if let Some(initializer) = initializer {
                let initializer_type = type_check_traverse(env, initializer)?;
                implicit_cast(initializer, &initializer_type, type_.clone())?;

                env.symbol_table.insert(name.clone(), type_.clone());

                return Some(type_.clone());
            }

            Some(ValueType::Unit)
        },

        CXExpr::VarReference(name) => {
            env.symbol_table.get_symbol(name).cloned()
        },

        CXExpr::IntLiteral { bytes, .. } => {
            Some(ValueType::Integer { bytes: *bytes, signed: true })
        },
        CXExpr::FloatLiteral { bytes, .. } => {
            Some(ValueType::Float { bytes: *bytes })
        },
        CXExpr::StringLiteral { .. } => {
            Some(ValueType::PointerTo(Box::new(ValueType::Identifier("char".to_string()))))
        },

        CXExpr::Return { value } => {
            if let Some(value) = value {
                let value_type = type_check_traverse(env, value)?;
                implicit_cast(value, &value_type, env.return_type.clone())?;
            } else if env.return_type != ValueType::Unit {
                log_error!("TYPE ERROR: Function with empty return in non-void context");
            }

            Some(ValueType::Unit)
        },

        _ => Some(ValueType::Unit)
    }
}

pub(crate) fn implicit_cast(expr: &mut CXExpr, from_type: &ValueType, to_type: ValueType) -> Option<()> {
    if !matches!(expr, CXExpr::VarReference(_)) && from_type == &to_type {
        return Some(());
    }

    unsafe {
        // this feels like something that should be safe with the borrow checker, not my problem
        let old_expr = std::ptr::read(expr);
        let implicit_cast = CXExpr::ImplicitCast {
            expr: Box::new(old_expr),
            to_type,
        };

        let replaced = std::mem::replace(expr, implicit_cast);
        std::mem::forget(replaced);
    }

    Some(())
}