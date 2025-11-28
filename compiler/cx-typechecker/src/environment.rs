use cx_lexer_data::token::Token;
use cx_parsing_data::data::{CXNaivePrototype, CXNaiveType, NaiveFnIdent, NaiveFnKind};
use cx_pipeline_data::CompilationUnit;
use cx_pipeline_data::db::ModuleData;
use cx_typechecker_data::CXTypeMap;
use cx_typechecker_data::ast::{TCBaseMappings, TCGlobalVariable};
use cx_typechecker_data::function_map::{CXFnMap, CXFunctionIdentifier};
use cx_typechecker_data::intrinsic_types::INTRINSIC_TYPES;
use cx_typechecker_data::mir::expression::MIRValue;
use cx_typechecker_data::mir::program::MIRBasicBlock;
use cx_typechecker_data::mir::types::{CXFunctionPrototype, CXTemplateInput, CXType};
use cx_util::identifier::CXIdent;
use cx_util::scoped_map::ScopedMap;
use cx_util::{CXError, CXResult};
use std::collections::{HashMap, HashSet};

use crate::builder::{BlockPointer, MIRBuilder};
use crate::type_completion::templates::instantiate_function_template;
use crate::type_completion::{complete_fn_prototype, complete_type};

pub struct TCTemplateRequest {
    pub module_origin: Option<String>,
    pub name: NaiveFnKind,
    pub input: CXTemplateInput,
}

pub struct Scope {
    pub break_to: Option<CXIdent>,
    pub continue_to: Option<CXIdent>,
}

pub struct TCEnvironment<'a> {
    pub tokens: &'a [Token],
    pub compilation_unit: CompilationUnit,

    pub module_data: &'a ModuleData,

    pub realized_types: CXTypeMap,
    pub realized_fns: CXFnMap,
    pub realized_globals: HashMap<String, TCGlobalVariable>,

    pub(crate) builder: MIRBuilder,

    pub requests: Vec<TCTemplateRequest>,
    pub deconstructors: HashSet<CXType>,

    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<MIRValue>,
    pub scope_stack: Vec<Scope>,

    pub in_external_templated_function: bool,
}

impl TCEnvironment<'_> {
    pub fn new<'a>(
        tokens: &'a [Token],
        compilation_unit: CompilationUnit,
        module_data: &'a ModuleData,
    ) -> TCEnvironment<'a> {
        let intrinsic_types = INTRINSIC_TYPES
            .iter()
            .map(|(name, ty)| (name.to_string(), ty.clone().into()))
            .collect::<HashMap<_, _>>();

        TCEnvironment {
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
            deconstructors: HashSet::new(),
            symbol_table: ScopedMap::new(),

            in_external_templated_function: false,
        }
    }

    pub fn push_scope(&mut self, continue_to: Option<CXIdent>, break_to: Option<CXIdent>) {
        self.symbol_table.push_scope();
        self.scope_stack.push(Scope {
            break_to,
            continue_to,
        });
    }
    
    pub fn pop_scope(&mut self) {
        self.symbol_table.pop_scope();
        self.scope_stack.pop();
    }

    pub fn insert_symbol(&mut self, name: String, value: MIRValue) {
        self.symbol_table.insert(name, value);
    }

    pub fn add_type(&mut self, name: String, ty: CXType) {
        self.realized_types.insert(name, ty);
    }

    pub fn symbol_value(&self, name: &str) -> Option<&MIRValue> {
        self.symbol_table.get(name)
    }

    pub fn get_func(
        &mut self,
        base_data: &TCBaseMappings,
        name: &NaiveFnIdent,
    ) -> CXResult<CXFunctionPrototype> {
        let Some(base_fn) = base_data.fn_data.get_standard(name) else {
            return CXError::create_result(format!("Function not found: {:?}", name));
        };

        complete_fn_prototype(
            self,
            base_data,
            base_fn.external_module.as_ref(),
            &base_fn.resource,
        )
    }

    pub fn get_func_templated(
        &mut self,
        base_data: &TCBaseMappings,
        name: &NaiveFnKind,
        input: &CXTemplateInput,
    ) -> CXResult<CXFunctionPrototype> {
        instantiate_function_template(self, base_data, name, input)
    }

    pub fn get_realized_func(&self, name: &CXFunctionIdentifier) -> Option<CXFunctionPrototype> {
        self.realized_fns.get(name).cloned()
    }

    pub fn get_type(&mut self, base_data: &TCBaseMappings, name: &str) -> CXResult<CXType> {
        let Some(_ty) = base_data.type_data.get_standard(&name.to_string()) else {
            return CXError::create_result(format!("Type not found: {}", name));
        };

        complete_type(self, base_data, _ty.external_module.as_ref(), &_ty.resource)
    }

    pub fn get_realized_type(&self, name: &str) -> Option<CXType> {
        self.realized_types.get(name).cloned()
    }

    pub fn destructor_exists(&self, base_data: &TCBaseMappings, _type: &CXType) -> bool {
        let Some(type_name) = _type.get_identifier() else {
            return false;
        };

        base_data
            .fn_data
            .is_key_any(&NaiveFnIdent::Destructor(type_name.clone()))
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.builder.current_prototype()
    }

    pub fn complete_type(
        &mut self,
        base_data: &TCBaseMappings,
        _type: &CXNaiveType,
    ) -> CXResult<CXType> {
        complete_type(self, base_data, None, _type)
    }

    pub fn complete_prototype(
        &mut self,
        base_data: &TCBaseMappings,
        external_module: Option<&String>,
        prototype: &CXNaivePrototype,
    ) -> CXResult<CXFunctionPrototype> {
        complete_fn_prototype(self, base_data, external_module, prototype)
    }

    pub fn complete_fn_ident(
        &mut self,
        ident: &CXFunctionIdentifier,
    ) -> Option<CXFunctionPrototype> {
        if let Some(prototype) = self.get_realized_func(ident) {
            return Some(prototype);
        }

        None
    }

    pub fn in_defer<F, T>(&mut self, f: F) -> CXResult<T>
        where F: FnOnce(&mut Self) -> CXResult<T> {
            
        if self.builder.in_defer() {
            return CXError::create_result("Cannot nest defer blocks");
        }
             
        let Some(func_ctx) = &mut self.builder.function_context else {
            unreachable!()
        };
        
        if func_ctx.defer_blocks.is_empty() {
            func_ctx.defer_blocks.push(MIRBasicBlock {
                id: CXIdent::from("defer_entry"),
                expressions: Vec::new(),
            });
        }
        let previous_pointer = func_ctx.current_block.clone();
        
        self.builder.set_pointer(BlockPointer::Defer(self.builder.get_defer_end()));
        self.builder.set_block(CXIdent::from("defer_entry"));
        
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
}
