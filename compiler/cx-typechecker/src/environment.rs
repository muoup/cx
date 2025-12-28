use cx_lexer_data::token::Token;
use cx_parsing_data::ast::CXExpr;
use cx_parsing_data::data::{CXFunctionKind, CXNaivePrototype, CXNaiveType};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::CXTypeMap;
use cx_typechecker_data::function_map::CXFnMap;
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use cx_typechecker_data::mir::expression::{MIRInstruction, MIRRegister, MIRValue};
use cx_typechecker_data::mir::program::{
    MIRBaseMappings, MIRBasicBlock, MIRGlobalVariable, MIRUnit,
};
use cx_typechecker_data::mir::types::{CXTemplateInput, MIRFunctionPrototype, MIRType};
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::{CXError, CXResult};
use std::collections::HashMap;

use crate::builder::{BlockPointer, MIRBuilder};
use crate::environment::deconstruction::process_new_type;
use crate::environment::function_query::{
    query_deconstructor, query_destructor, query_member_function, query_static_member_function, query_standard_function
};
use crate::type_completion::{complete_prototype_no_insert, complete_type};

pub(crate) mod deconstruction;
pub(crate) mod function_query;
pub(crate) mod name_mangling;

pub enum MIRFunctionGenRequest {
    Template {
        module_origin: Option<String>,
        kind: CXFunctionKind,
        input: CXTemplateInput,
    },

    Deconstruction {
        _type: MIRType,
    },
}

pub struct Scope {
    pub break_to: Option<CXIdent>,
    pub continue_to: Option<CXIdent>,
}

pub const DEFER_ACCUMULATION_REGISTER: &str = "__defer_accumulation_register";

pub struct TypeEnvironment<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,

    pub module_data: &'a ModuleData,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, MIRGlobalVariable>,

    pub(crate) builder: MIRBuilder,

    pub requests: Vec<MIRFunctionGenRequest>,
    pub current_function: Option<MIRFunctionPrototype>,
    pub arg_vals: Vec<MIRValue>,

    pub symbol_table: ScopedMap<MIRValue>,
    pub scope_stack: Vec<Scope>,

    pub in_external_templated_function: bool,
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
            builder: MIRBuilder::new(),

            realized_types: intrinsic_types,
            realized_fns: HashMap::new(),
            realized_globals: HashMap::new(),

            current_function: None,

            scope_stack: Vec::new(),
            requests: Vec::new(),
            symbol_table: ScopedMap::new(),

            arg_vals: Vec::new(),

            in_external_templated_function: false,
        }
    }

    pub fn push_scope(&mut self, continue_to: Option<CXIdent>, break_to: Option<CXIdent>) {
        self.symbol_table.push_scope();
        self.scope_stack.push(Scope {
            break_to,
            continue_to,
        });
        self.builder.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.symbol_table.pop_scope();
        self.scope_stack.pop().unwrap();
        self.builder.pop_scope();
    }
    
    pub fn insert_symbol(&mut self, name: String, value: MIRValue) {
        self.symbol_table.insert(name, value);
    }

    pub fn add_type(
        &mut self,
        base_data: &MIRBaseMappings,
        name: String,
        _type: MIRType,
    ) -> Option<MIRType> {
        let old = self.realized_types.remove(&name);
        
        self.realized_types.insert(name.clone(), _type.clone());
        process_new_type(self, base_data, _type);

        old
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRValue> {
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
        self.builder.current_prototype()
    }

    pub fn complete_type(
        &mut self,
        base_data: &MIRBaseMappings,
        _type: &CXNaiveType,
    ) -> CXResult<MIRType> {
        complete_type(self, base_data, None, _type)
    }

    pub fn complete_prototype(
        &mut self,
        base_data: &MIRBaseMappings,
        external_module: Option<&String>,
        prototype: &CXNaivePrototype,
    ) -> CXResult<MIRFunctionPrototype> {
        complete_prototype_no_insert(self, base_data, external_module, prototype)
            .inspect(|prototype| {
                self.realized_fns.insert(prototype.name.to_string(), prototype.clone());
            })
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

    pub fn get_static_member_function(
        &mut self,
        base_data: &MIRBaseMappings,
        expr: &CXExpr,
        member_type: &MIRType,
        name: &CXIdent,
        template_input: Option<&CXTemplateInput>,
    ) -> CXResult<MIRFunctionPrototype> {
        query_static_member_function(self, base_data, expr, member_type, name, template_input)
    }

    fn start_defer(&mut self) {
        let Some(func_ctx) = &mut self.builder.function_context else {
            unreachable!()
        };

        let mut instructions = Vec::new();
        let return_type = &func_ctx.current_prototype.return_type;
        
        if !return_type.is_unit() {
            let return_acc = MIRRegister {
                name: CXIdent::new(DEFER_ACCUMULATION_REGISTER),
            };

            instructions.push(MIRInstruction::Phi {
                result: return_acc.clone(),
                predecessors: vec![],
            });
         
            if return_type.is_memory_resident() {
                instructions.push(
                    MIRInstruction::LifetimeStart { 
                        name: DEFER_ACCUMULATION_REGISTER.to_owned(),
                        region: return_acc,
                        _type: return_type.clone(), 
                    }
                )
            }
        };

        func_ctx.defer_blocks.push(MIRBasicBlock {
            id: CXIdent::new("defer_entry"),
            instructions,
        });
    }

    pub fn in_defer<F, T>(&mut self, f: F) -> CXResult<T>
    where
        F: FnOnce(&mut Self) -> CXResult<T>,
    {
        if self.builder.in_defer() {
            return CXError::create_result("Cannot nest defer blocks");
        }

        let Some(func_ctx) = &mut self.builder.function_context else {
            unreachable!()
        };
        let previous_pointer = func_ctx.current_block.clone();

        if func_ctx.defer_blocks.is_empty() {
            self.start_defer();
        }

        self.builder
            .set_pointer(BlockPointer::Defer(self.builder.get_defer_end()));

        let result = f(self);

        let Some(func_ctx) = &mut self.builder.function_context else {
            unreachable!()
        };
        let BlockPointer::Defer(i) = func_ctx.current_block.clone() else {
            unreachable!()
        };

        self.builder.set_defer_end(i);
        self.builder.set_pointer(previous_pointer);

        result
    }

    pub fn finish_mir_unit(self) -> CXResult<MIRUnit> {
        Ok(MIRUnit {
            functions: self.builder.generated_functions,
            prototypes: self.realized_fns.into_values().collect(),
            global_variables: self.realized_globals.into_values().collect(),
        })
    }
}
