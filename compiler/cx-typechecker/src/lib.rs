use cx_ast::ast::{CXAST, CXASTStmt, modifiers::VisibilityMode};
use cx_mir::mir::program::EnvironmentNamespace;
use cx_mir::mir::r#type::MIRType;
use cx_util::{
    CXResult,
};

pub mod log;

pub mod environment;
mod type_checking;

pub use type_checking::{
    complete_base_functions, complete_base_globals, realize_fn_implementation,
};

use crate::{environment::{MIRFunctionGenRequest, TypeEnvironment}, type_checking::functions::typecheck_function};

pub fn typecheck(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ast: &CXAST,
) -> CXResult<()> {
    complete_base_globals(env, namespace, ast)?;
    complete_base_functions(env, namespace)?;

    for stmt in ast.definition_stmts.iter() {
        if let CXASTStmt::FunctionDefinition { prototype, body, .. } = stmt {
            let prototype = env.complete_prototype(namespace, None, prototype)?;
            typecheck_function(env, namespace, prototype.clone(), body)?;
        }
    }

    Ok(())
}

pub fn fulfill_request(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    request: &MIRFunctionGenRequest,
) {
    todo!("Fulfill request routine -- ensure that we are double checking the request hasn't already been fulfilled");
}

fn realize_tagged_union_constructor(
    env: &mut TypeEnvironment,
    name: String,
    union_type: MIRType,
    variant_type: MIRType,
    variant_index: usize,
) {
    use cx_ast::ast::{
        function::{CXFunctionContract, CXFunctionKind},
        modifiers::CXLinkageMode,
    };
    use cx_mir::mir::{
        data::{MIRFunctionPrototype, MIRParameter},
        expression::{MIRExpression, MIRExpressionKind},
        program::MIRFunction,
    };
    use cx_tokens::TokenRange;
    use cx_util::{identifier::CXIdent, namespace::QualifiedName};

    let param_name = CXIdent::new("value");
    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(name),
        return_type: union_type.clone(),
        params: if variant_type.is_unit() {
            Vec::new()
        } else {
            vec![MIRParameter {
                name: Some(param_name.clone()),
                _type: variant_type.clone(),
            }]
        },
        var_args: false,
        contract: CXFunctionContract::default(),
        linkage: CXLinkageMode::Static,
    };

    let value = if variant_type.is_unit() {
        MIRExpression {
            token_range: None,
            _type: variant_type.clone(),
            kind: MIRExpressionKind::Unit,
        }
    } else {
        let param_ref = MIRExpression {
            token_range: None,
            _type: env.symbols.mem_ref_to(variant_type.clone()),
            kind: MIRExpressionKind::Variable(param_name),
        };

        MIRExpression {
            token_range: None,
            _type: variant_type.clone(),
            kind: MIRExpressionKind::RegionDuplicate {
                source: Box::new(param_ref),
            },
        }
    };
    let constructed = MIRExpression {
        token_range: None,
        _type: union_type.clone(),
        kind: MIRExpressionKind::ConstructTaggedUnion {
            variant_index,
            value: Box::new(value),
            sum_type: union_type,
        },
    };
    let body = MIRExpression {
        token_range: None,
        _type: prototype.return_type.clone(),
        kind: MIRExpressionKind::Return {
            value: Some(Box::new(constructed)),
            postcondition: None,
        },
    };

    env.push_generated_function(MIRFunction { prototype, body });
}