use std::collections::{BTreeMap, HashMap};

use cx_mir::{MIRConstant, MIRValue, expr::visit::successors};
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRFunctionID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRInstruction, MIRInstructionKind, MIRPlaceDecl, MIRPlaceID, MIRRegister, MIRRegisterDecl, MIRScopeDecl, MIRScopeID, MIRType, MIRTypeID, MIRTypeKind, MIRUnit, ty::{interface::MTRegistry, registry::MIRTypeRegistry},
};
use cx_target::ArchitectureConfig;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use index_vec::IndexVec;

///
/// Handles the internals of generating an MIRUnit via an API compatible with HMIR lowering. Used as an internal component
/// of the HMIREnvironment.
///
/// Responsible for generating MIR functions, globals, and the type registry.
///
#[derive(Debug)]
pub struct MIRModuleBuilder {
    architecture: ArchitectureConfig,

    types: IndexVec<MIRTypeID, MIRType>,
    interned: HashMap<MIRType, MIRTypeID>,
    debug_names: HashMap<MIRTypeID, String>,
    functions: BTreeMap<MIRFunctionID, MIRFunction>,
    globals: BTreeMap<MIRGlobalID, MIRGlobalVariable>,
    global_order: Vec<MIRGlobalID>,

    function_names: HashMap<String, MIRFunctionID>,
    global_names: HashMap<String, MIRGlobalID>,
}

impl MIRModuleBuilder {
    pub fn new(architecture: ArchitectureConfig) -> Self {
        Self {
            architecture,

            types: IndexVec::new(),
            interned: HashMap::new(),
            debug_names: HashMap::new(),
            functions: BTreeMap::new(),
            globals: BTreeMap::new(),
            global_order: Vec::new(),

            function_names: HashMap::new(),
            global_names: HashMap::new(),
        }
    }

    pub fn resolve_type(&self, ty: MIRTypeID) -> &MIRType {
        self.types.get(ty).expect("Type not found in registry")
    }

    pub fn intern_type(&mut self, ty: MIRType) -> MIRTypeID {
        if let Some(id) = self.find(&ty) {
            return id;
        }

        let id = self.types.push(ty.clone());
        self.interned.insert(ty, id);
        id
    }

    // An aggregate may refer to itself, so its ID exists before its definition does
    pub fn reserve_type(&mut self) -> MIRTypeID {
        self.types.push(MIRType::undefined())
    }

    pub fn define_type(&mut self, id: MIRTypeID, ty: MIRType, debug_name: Option<String>) {
        self.types[id] = ty.clone();
        self.interned.entry(ty).or_insert(id);

        if let Some(name) = debug_name {
            self.debug_names.insert(id, name);
        }
    }

    pub fn declare_function(&mut self, prototype: MIRFnPrototype) -> MIRFunctionID {
        let name = prototype.symbol_name.as_string();
        if let Some(id) = self.function_names.get(&name) {
            return *id;
        }

        let id = MIRFunctionID::new(self.functions.len());
        self.functions.insert(id, MIRFunction::new(prototype, None));
        self.function_names.insert(name, id);
        id
    }

    pub fn declared_function(&self, name: &str) -> Option<MIRFunctionID> {
        self.function_names.get(name).copied()
    }

    pub fn define_function(&mut self, id: MIRFunctionID, function: MIRFunction) {
        *self.functions.get_mut(&id).unwrap() = function;
    }

    pub fn function(&self, id: MIRFunctionID) -> &MIRFunction {
        &self.functions[&id]
    }

    pub fn declare_global(&mut self, global: MIRGlobalVariable) -> MIRGlobalID {
        let name = global.name().as_string();
        if let Some(id) = self.global_names.get(&name) {
            return *id;
        }

        let id = MIRGlobalID::new(self.globals.len());
        self.globals.insert(id, global);
        self.global_names.insert(name, id);
        self.global_order.push(id);
        id
    }

    pub fn define_global(&mut self, id: MIRGlobalID, state: MIRGlobalState) {
        let global = self.globals
            .get_mut(&id)
            .unwrap_or_else(|| panic!("Could not define unknown global id: {id}"));
        *global = MIRGlobalVariable::new(
            global.name().clone(), global.linkage(), global.ty(), state, global.is_mutable(),
        );
    }

    pub fn declared_global(&self, name: &str) -> Option<MIRGlobalID> {
        self.global_names.get(name).copied()
    }

    pub fn global(&self, id: MIRGlobalID) -> &MIRGlobalVariable {
        &self.globals[&id]
    }

    pub fn finish(self) -> MIRUnit {
        MIRUnit::new(
            MIRTypeRegistry::new(self.architecture, self.types, self.debug_names),
            self.functions,
            self.globals,
            self.global_order,
        )
    }
}

impl MTRegistry for MIRModuleBuilder {
    fn architecture(&self) -> &ArchitectureConfig {
        &self.architecture
    }

    fn definition(&self, id: MIRTypeID) -> Option<&MIRType> {
        self.types.get(id)
    }

    fn find(&self, ty: &MIRType) -> Option<MIRTypeID> {
        self.interned.get(ty).copied()
    }

    fn find_kind(&self, kind: &MIRTypeKind) -> Option<MIRTypeID> {
        self.find(&MIRType::new(kind.clone()))
    }

    fn debug_name(&self, id: MIRTypeID) -> Option<&str> {
        self.debug_names.get(&id).map(String::as_str)
    }
}

#[derive(Debug)]
pub struct MIRFunctionBuilder {
    prototype: MIRFnPrototype,

    parameters: Vec<MIRPlaceID>,
    blocks: IndexVec<MIRBasicBlockID, MIRBasicBlock>,
    registers: IndexVec<MIRRegister, MIRRegisterDecl>,
    places: IndexVec<MIRPlaceID, MIRPlaceDecl>,
    scopes: IndexVec<MIRScopeID, MIRScopeDecl>,
    address_labels: Vec<(String, MIRBasicBlockID)>,

    current_block: MIRBasicBlockID,
}

impl MIRFunctionBuilder {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        let entry = MIRBasicBlockID::new(0);
        let mut blocks = IndexVec::new();
        blocks.push(MIRBasicBlock::new(entry, None));

        Self {
            prototype,

            parameters: Vec::new(),
            blocks,
            registers: IndexVec::new(),
            places: IndexVec::new(),
            scopes: IndexVec::new(),
            address_labels: Vec::new(),

            current_block: entry,
        }
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn parameters(&self) -> &[MIRPlaceID] {
        &self.parameters
    }

    pub fn parameter(&self, index: usize) -> Option<MIRPlaceID> {
        self.parameters.get(index).copied()
    }

    pub fn add_parameter(&mut self, param: MIRPlaceID) {
        self.parameters.push(param);
    }

    pub fn blocks(&self) -> &[MIRBasicBlock] {
        self.blocks.as_raw_slice()
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock> {
        self.blocks.get(id)
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock> {
        self.blocks.get_mut(id)
    }

    pub fn add_block(&mut self, debug_name: Option<CXIdent>) -> MIRBasicBlockID {
        let id = self.blocks.next_idx();
        self.blocks.push(MIRBasicBlock::new(id, debug_name));
        id
    }

    pub fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub fn current_block_reachable(&self) -> bool {
        self.reachable()[self.current_block]
    }

    fn reachable(&self) -> IndexVec<MIRBasicBlockID, bool> {
        let mut visited = IndexVec::from_vec(vec![false; self.blocks.len()]);
        let mut pending = vec![MIRBasicBlockID::new(0)];
        while let Some(id) = pending.pop() {
            if visited[id] {
                continue;
            }
            visited[id] = true;
            let Some(instruction) = self.blocks[id].last_instruction() else {
                continue;
            };
            match &instruction.kind {
                MIRInstructionKind::Branch {
                    cond: MIRValue::Constant(MIRConstant::Integer { value, .. }),
                    true_target,
                    false_target,
                } => pending.push(if *value != 0 { true_target.block } else { false_target.block }),
                _ => pending.extend(successors(instruction).into_iter().map(|target| target.block)),
            }
        }
        visited
    }

    pub fn switch_to(&mut self, block: MIRBasicBlockID) {
        assert!(
            self.blocks.get(block).is_some(),
            "Attempt to switch to unknown block {block}"
        );

        self.current_block = block;
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        let block = &mut self.blocks[self.current_block];
        if block
            .last_instruction()
            .is_some_and(MIRInstruction::is_terminator)
        {
            return;
        }

        block.push_instruction(MIRInstruction::new(kind, span));
    }

    pub fn add_address_label(&mut self, label: String, block: MIRBasicBlockID) {
        self.address_labels.push((label, block));
    }

    pub fn address_labels(&self) -> &[(String, MIRBasicBlockID)] {
        &self.address_labels
    }

    pub fn add_scope(&mut self, span: TokenRange) -> MIRScopeID {
        self.scopes.push(MIRScopeDecl::new(span))
    }

    pub fn places(&self) -> &[MIRPlaceDecl] {
        self.places.as_raw_slice()
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places.get(id)
    }

    pub fn place_mut(&mut self, id: MIRPlaceID) -> Option<&mut MIRPlaceDecl> {
        self.places.get_mut(id)
    }

    pub fn allocate_place(&mut self, place: MIRPlaceDecl) -> MIRPlaceID {
        self.places.push(place)
    }

    pub fn scopes(&self) -> &[MIRScopeDecl] {
        self.scopes.as_raw_slice()
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers.get(id)
    }

    pub fn allocate_register(&mut self, register: MIRRegisterDecl) -> MIRRegister {
        self.registers.push(register)
    }

    pub fn finish(mut self) -> MIRFunction {
        let reachable = self.reachable();
        for (id, block) in self.blocks.iter_mut_enumerated() {
            if !reachable[id]
                && !block
                    .last_instruction()
                    .is_some_and(MIRInstruction::is_terminator)
            {
                let unreachable =
                    MIRInstruction::new(MIRInstructionKind::Unreachable, TokenRange::internal());
                block.push_instruction(unreachable);
            }
        }
        MIRFunction::new(
            self.prototype,
            Some(MIRBody::new(
                MIRBasicBlockID::new(0),
                self.blocks.raw,
                self.places.raw,
                self.parameters,
                self.registers.raw,
                self.scopes.raw,
                self.address_labels,
            )),
        )
    }
}
