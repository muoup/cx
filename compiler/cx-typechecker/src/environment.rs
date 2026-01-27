use cx_tokens::token::Token;
use cx_ast::ast::CXExpr;
use cx_ast::data::{CXTemplateInput, CXFunctionKind, CXPrototype, CXType};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_mir::CXTypeMap;
use cx_mir::function_map::CXFnMap;
use cx_mir::intrinsic_types::INTRINSIC_TYPES;
use cx_mir::mir::expression::MIRExpression;
use cx_mir::mir::program::{MIRBaseMappings, MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::{CXError, CXResult};
use std::collections::HashMap;

use crate::environment::function_query::{
    query_deconstructor, query_destructor, query_member_function, query_standard_function,
};
use crate::type_completion::{complete_prototype_no_insert, complete_type};

pub(crate) mod function_query;
pub(crate) mod name_mangling;

pub enum MIRFunctionGenRequest {
    Template {
        module_origin: Option<String>,
        kind: CXFunctionKind,
        input: CXTemplateInput,
    },
}

pub struct Scope {
    pub has_break_merge: bool,
    pub has_continue_merge: bool,
}

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,

    pub module_data: &'a ModuleData,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, MIRGlobalVariable>,

    pub requests: Vec<MIRFunctionGenRequest>,
    pub current_function: Option<MIRFunctionPrototype>,
    pub arg_vals: Vec<MIRExpression>,

    pub symbol_table: ScopedMap<MIRExpression>,
    pub scope_stack: Vec<Scope>,

    pub in_external_templated_function: bool,

    pub generated_functions: Vec<MIRFunction>,
}

impl TypeEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        module_data: &'a ModuleData,
    ) -> TypeEnvironment<'a> {
        let intrinsic_types = INTRINSIC_TYPES
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone().into()))
            .collect::<HashMap<_, _>>();

        TypeEnvironment {
            tokens,
            compilation_unit,

            module_data,

            realized_types: intrinsic_types,
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            current_function: None,

            scope_stack: Vec::new(),
            requests: Vec::new(),
            symbol_table: ScopedMap::new(),

            arg_vals: Vec::new(),

            in_external_templated_function: false,
            generated_functions: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, has_break_merge: bool, has_continue_merge: bool) {
        self.symbol_table.push_scope();
        self.scope_stack.push(Scope {
            has_break_merge,
            has_continue_merge,
        });
    }

    pub fn pop_scope(&mut self) {
        self.symbol_table.pop_scope();
        self.scope_stack.pop().unwrap();
    }

    pub fn insert_symbol(&mut self, name: String, value: MIRExpression) {
        self.symbol_table.insert(name, value);
    }

    pub fn add_type(
        &mut self,
        name: String,
        _type: MIRType,
    ) -> Option<MIRType> {
        let old = self.realized_types.remove(&name);
        self.realized_types.insert(name.clone(), _type.clone());
        old
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRExpression> {
        self.symbol_table.get(name)
    }

    pub fn get_realized_func(&self, name: &str) -> Option<MIRFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }

    pub fn get_type(&mut self, base_data: &MIRBaseMappings, name: &str) -> CXResult<MIRType> {
        let Some(_ty) = base_data.type_data.get_standard(&name.to_string()) else {
            return CXError::create_result(format!("Type not found: {}", name));
        };

        self.complete_type(base_data, &_ty.resource)
    }

    pub fn get_realized_type(&self, name: &str) -> Option<MIRType> {
        self.realized_types.get(name).cloned()
    }

    pub fn get_deconstructor(&mut self, _type: &MIRType) -> Option<MIRFunctionPrototype> {
        query_deconstructor(self, _type)
    }

    pub fn get_destructor(
        &mut self,
        base_data: &MIRBaseMappings,
        _type: &MIRType,
    ) -> Option<MIRFunctionPrototype> {
        query_destructor(self, base_data, _type)
    }

    pub fn current_function(&self) -> &MIRFunctionPrototype {
        self.current_function.as_ref().unwrap()
    }

    pub fn complete_type(
        &mut self,
        base_data: &MIRBaseMappings,
        _type: &CXType,
    ) -> CXResult<MIRType> {
        complete_type(self, base_data, None, _type)
    }

    pub fn complete_prototype(
        &mut self,
        base_data: &MIRBaseMappings,
        external_module: Option<&String>,
        prototype: &CXPrototype,
    ) -> CXResult<MIRFunctionPrototype> {
        complete_prototype_no_insert(self, base_data, external_module, prototype).inspect(
            |prototype| {
                self.realized_fns
                    .insert(prototype.name.to_string(), prototype.clone());
            },
        )
    }

    pub fn is_copyable(&mut self, ty: &MIRType) -> bool {
        self.get_deconstructor(ty).is_none()
    }

    pub fn get_standard_function(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        key: &CXIdent,
        template_input: Option<&CXTemplateInput>,
    ) -> CXResult<MIRFunctionPrototype> {
        query_standard_function(self, base_data, expr, key, template_input)
    }

    pub fn get_member_function(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        member_type: &MIRType,
        name: &CXIdent,
        template_input: Option<&CXTemplateInput>,
    ) -> CXResult<MIRFunctionPrototype> {
        query_member_function(self, base_data, expr, member_type, name, template_input)
    }

    #[allow(dead_code)]
    fn start_defer(&mut self) {
        todo!()
    }

    pub fn in_defer<F, T>(&mut self, _: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        todo!()
    }

    pub fn finish_mir_unit(self) -> CXResult<MIRUnit> {
        Ok(MIRUnit {
            functions: self.generated_functions,
            prototypes: self.realized_fns.into_values().collect(),
            global_variables: self.realized_globals.into_values().collect(),
        })
    }
}
