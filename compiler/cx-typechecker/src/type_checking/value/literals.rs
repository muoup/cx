use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};
use cx_ast::ast::modifiers::{CX_CONST, CXLinkageMode};
use cx_mir::mir::{
    data::{MIRFloatType, MIRIntegerType, MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
    program::{MIRGlobalVarKind, MIRGlobalVariable},
};
use cx_util::identifier::CXIdent;
use cx_util::unsafe_float::FloatWrapper;

fn anonymous_name_gen() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__anon_{id}")
}

pub(crate) fn typecheck_int_literal(val: i64, bytes: u8) -> TypecheckResult {
    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::IntLiteral(val, MIRIntegerType::from_bytes(bytes).unwrap(), true),
        _type: MIRType::from(MIRTypeKind::Integer {
            _type: MIRIntegerType::from_bytes(bytes).unwrap(),
            signed: true,
        }),
    })
}

pub(crate) fn typecheck_float_literal(val: FloatWrapper, bytes: u8) -> TypecheckResult {
    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::FloatLiteral(val, MIRFloatType::from_bytes(bytes).unwrap()),
        _type: MIRType::from(MIRTypeKind::Float {
            _type: MIRFloatType::from_bytes(bytes).unwrap(),
        }),
    })
}

pub(crate) fn typecheck_string_literal(env: &mut TypeEnvironment, val: &str) -> TypecheckResult {
    let anonymous_name = anonymous_name_gen();
    let name_ident = CXIdent::new(anonymous_name.clone());

    env.items.realized_globals.insert(
        anonymous_name,
        MIRGlobalVariable {
            kind: MIRGlobalVarKind::StringLiteral {
                name: name_ident.clone(),
                value: val.to_string(),
            },
            is_mutable: false,
            linkage: CXLinkageMode::Static,
        },
    );

    let str_ref_type = env
        .symbols
        .mem_ref_to(MIRType::from(MIRTypeKind::Str).add_specifier(CX_CONST));

    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::Variable {
            name: name_ident,
            location: SymbolValueOrigin::Global,
        },
        _type: str_ref_type,
    })
}

pub(crate) fn typecheck_unit() -> TypecheckResult {
    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::Unit,
        _type: MIRType::unit(),
    })
}
