use crate::parse::ast::{Expression, GlobalStatement, AST};
use crate::parse::parser::VarTable;
use crate::parse::verify::context::{FnMap, FunctionPrototype, TypeMap, VerifyContext};
use crate::parse::verify::typeless_declarations::gen_declarations;
use crate::parse::verify::verify_expression::verify_expression;
use crate::parse::verify::verify_type::{verify_fn_prototype, verify_type};

pub mod context;
mod typeless_declarations;
mod verify_expression;
mod verify_type;

#[derive(Debug)]
pub struct VerifiedAST {
    pub funcs: Vec<(FunctionPrototype, Vec<Expression>)>,
}

pub fn verify_ast(ast: AST) -> Option<VerifiedAST> {
    let (mut type_map, mut fn_map) = gen_declarations(&ast)?;
    let function_bodies = ast.statements.into_iter()
        .filter_map(|stmt| {
            let GlobalStatement::Function { prototype, body } = stmt else {
                return None;
            };

            Some((prototype.name, body?))
        })
        .collect::<Vec<_>>();

    let type_map_iter = type_map.iter()
        .map(|(key, _type)| (key, verify_type(&type_map, _type.clone())));
    let mut type_map = TypeMap::new();

    for (_key, _type) in type_map_iter {
        type_map.insert(_key.clone(), _type?);
    }

    let fn_map_iter = fn_map.into_iter()
        .map(|(key, prototype)| (key, verify_fn_prototype(&type_map, prototype)));
    let mut fn_map = FnMap::new();

    for (_key, prototype) in fn_map_iter {
        fn_map.insert(_key, prototype?);
    }

    let mut context = VerifyContext {
        type_map,
        fn_map,

        variable_table: VarTable::new(),
        current_return_type: None,
        merge_stack: vec![],
    };

    let verified_funcs = function_bodies.into_iter()
        .map(|(name, mut body)| {
            for statement in body.iter_mut() {
                let _ = verify_expression(&mut context, statement)?;
            }

            Some((context.fn_map.get(&name).cloned()?, body))
        })
        .collect::<Option<Vec<_>>>()?;

    Some(VerifiedAST {
        funcs: verified_funcs
    })
}