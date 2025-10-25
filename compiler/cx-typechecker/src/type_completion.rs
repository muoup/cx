use std::collections::HashMap;

use cx_parsing_data::{
    parse::ast::{CXAST, CXGlobalStmt},
    preparse::{
        CXNaiveFnMap, CXNaiveTypeMap,
        naive_types::{CXNaivePrototype, CXNaiveType},
    },
};
use cx_pipeline_data::{CompilationUnit, db::ModuleData};
use cx_typechecker_data::{
    CXTypeData, CXTypeMap,
    ast::TCGlobalVariable,
    cx_types::{CXFunctionPrototype, CXType},
    function_map::CXFnData,
    intrinsic_types::INTRINSIC_TYPES,
};
use cx_util::{CXResult, identifier::CXIdent, log_error};

use crate::type_completion::types::{_complete_prototype, _complete_type, complete_fn_ident};

pub mod prototypes;
pub mod templates;
pub mod types;

pub fn complete_prototype(
    module_data: &ModuleData,
    unit: &CompilationUnit,
    external_module: Option<&String>,
    acc_type_map: &mut CXTypeMap,
    prototype: &CXNaivePrototype,
) -> Option<CXFunctionPrototype> {
    _complete_prototype(
        module_data,
        acc_type_map,
        base_type_map,
        external_module,
        prototype,
    )
}

pub fn complete_type(
    module_data: &ModuleData,
    unit: &CompilationUnit,
    external_module: Option<&String>,
    acc_type_data: &mut CXTypeMap,
    _type: &CXNaiveType,
) -> Option<CXType> {
    _complete_type(
        module_data,
        acc_type_data,
        base_type_map,
        external_module,
        _type,
    )
}

pub fn complete_type_map(
    module_data: &ModuleData,
    type_map: &CXNaiveTypeMap,
) -> CXResult<CXTypeData> {
    let mut cx_type_data = CXTypeData::new();

    for (name, template) in type_map.template_iter() {
        cx_type_data.insert_template(name.clone(), template.clone());
    }

    for (intrinsic_name, intrinsic) in INTRINSIC_TYPES.iter() {
        cx_type_data.insert_standard(intrinsic_name.to_string(), intrinsic.clone().into());
    }

    for (name, naive_type) in type_map.standard_iter() {
        let Some(cx_type) = _complete_type(
            module_data,
            &mut cx_type_data.standard,
            type_map,
            naive_type.external_module.as_ref(),
            &naive_type.resource,
        ) else {
            log_error!("Failed to contextualize type: {name}");
        };

        cx_type_data.insert_standard(name.clone(), cx_type);
    }

    Some(cx_type_data)
}

pub fn complete_fn_map(
    module_data: &ModuleData,
    fn_map: &CXNaiveFnMap,
    type_map: &mut CXTypeMap,
    naive_type_map: &CXNaiveTypeMap,
) -> CXResult<CXFnData> {
    let mut cx_fn_map = CXFnData::new();

    for (_, resource) in fn_map.standard_iter() {
        let Some(cx_prototype) = _complete_prototype(
            module_data,
            type_map,
            naive_type_map,
            resource.external_module.as_ref(),
            &resource.resource,
        ) else {
            log_error!(
                "Failed to contextualize function prototype: {:#?}",
                resource
            );
        };

        cx_fn_map.insert_standard(cx_prototype);
    }

    for (_, template) in fn_map.template_iter() {
        let ident = complete_fn_ident(
            module_data,
            type_map,
            naive_type_map,
            &template.resource.shell.name,
        )?;

        cx_fn_map.insert_template(ident, template.clone());
    }

    Some(cx_fn_map)
}

pub fn complete_globals(
    module_data: &ModuleData,
    type_map: &mut CXTypeMap,
    naive_map: &CXNaiveTypeMap,
    ast: &CXAST,
) -> Option<HashMap<String, TCGlobalVariable>> {
    let mut tc_globals = HashMap::new();

    for (name, constant) in ast.enum_constants.iter() {
        tc_globals.insert(
            name.clone(),
            TCGlobalVariable::UnaddressableConstant {
                name: CXIdent::from(name.clone()),
                val: *constant,
            },
        );
    }

    for global in ast.global_stmts.iter() {
        if let CXGlobalStmt::GlobalVariable {
            name,
            type_,
            initializer,
        } = global
        {
            if initializer.is_some() {
                todo!("Global variable with initializer")
            }

            let _type = _complete_type(module_data, type_map, naive_map, None, type_)?;

            tc_globals.insert(
                name.to_string(),
                TCGlobalVariable::Variable {
                    name: name.clone(),
                    _type,
                    initializer: None,
                },
            );
        }
    }

    Some(tc_globals)
}
