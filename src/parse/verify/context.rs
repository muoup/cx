use std::collections::HashMap;
use std::rc::Rc;
use crate::parse::ast::{VarInitialization, ValueType};
use crate::parse::verify::bytecode::{BytecodeBuilder, ElementID, ValueID, VirtualInstruction};
use crate::util::ScopedMap;

pub(crate) type TypeMap = HashMap<String, ValueType>;
pub(crate) type FnMap = HashMap<String, FunctionPrototype>;
pub(crate) type ConstMap = HashMap<String, i32>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionPrototype {
    pub(crate) name: String,
    pub(crate) return_type: ValueType,
    pub(crate) args: Vec<VarInitialization>
}

pub(crate) struct VerifyContext {
    pub(crate) type_map: TypeMap,
    pub(crate) fn_map: FnMap,
    pub(crate) constants_map: HashMap<String, i32>,
    pub(crate) var_map: ScopedMap<ValueID>,

    pub(crate) current_return_type: Option<ValueType>,
    pub(crate) merge_stack: Vec<ElementID>,
}

impl VerifyContext {
    pub(crate) fn get_function(&self, name: &str) -> Option<&FunctionPrototype> {
        self.fn_map.get(name)
    }

    pub(crate) fn get_type(&self, name: &str) -> Option<&ValueType> {
        self.type_map.get(name)
    }

    pub(crate) fn get_variable(&self, name: &str) -> Option<ValueID> {
        self.var_map.get(name).cloned()
    }

    pub(crate) fn get_constant(&mut self, name: &str) -> Option<i32> {
        self.constants_map.get(name).cloned()
    }

    pub(crate) fn get_value(&mut self, builder: &mut BytecodeBuilder, name: &str) -> Option<ValueID> {
        if let Some(val) = self.get_variable(name) {
            return Some(val);
        }

        let val = self.get_constant(name)?;

        builder.add_instruction(
            self,
            VirtualInstruction::Literal {
                val: val as u64
            },
            ValueType::Integer { bytes: 4, signed: true },
        )
    }

    pub(crate) fn insert_variable(&mut self, name: String, id: ValueID) {
        self.var_map.insert(name, id);
    }
}
