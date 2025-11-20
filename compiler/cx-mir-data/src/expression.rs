use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXType};
use cx_util::identifier::CXIdent;

pub type MIRRegister = CXIdent;

#[derive(Clone, Debug)]
pub enum MIRValue {
    IntLiteral { value: i64, _type: CXType },
    FloatLiteral { value: f64, _type: CXType },
    FunctionReference { prototype: CXFunctionPrototype },
    Register(MIRRegister),
}

#[derive(Clone, Debug)]
pub enum MIRInstruction {
    Alias {
        result: MIRRegister,
        value: MIRValue,
    },

    CreateStackRegion {
        result: MIRRegister,
        _type: CXType,
    },

    LoadGlobal {
        result: MIRRegister,
        name: CXIdent,
    },

    MemoryRead {
        result: MIRRegister,
        source: MIRValue,
        _type: CXType,
    },

    MemoryWrite {
        target: MIRValue,
        value: MIRValue,
    },

    StructGet {
        result: MIRRegister,
        source: MIRValue,
        field_index: usize,
        struct_type: CXType,
    },

    ArrayGet {
        result: MIRRegister,
        source: MIRValue,
        index: MIRValue,
        element_type: CXType,
    },

    CallFunction {
        result: Option<MIRRegister>,
        function: MIRValue,
        arguments: Vec<MIRRegister>,
    },

    Loop {
        condition_precheck: bool,
        condition: MIRValue,
        loop_id: CXIdent,
        body: CXIdent,
    },

    LoopContinue {
        loop_id: CXIdent,
    },

    Branch {
        condition: MIRValue,
        true_block: CXIdent,
        false_block: CXIdent,
    },

    Jump {
        target: CXIdent,
    },

    Return {
        value: Option<MIRValue>,
    },

    BinOp {
        result: MIRRegister,
        lhs: MIRValue,
        rhs: MIRValue,
        op: MIRBinOp,
    },

    UnOp {
        result: MIRRegister,
        operand: MIRValue,
        op: MIRUnOp,
    },

    Coercion {
        result: MIRRegister,
        operand: MIRValue,
        cast_type: MIRCoercion,
    },
    
    // ---- Verification Nodes ----
    
    Assert {
        value: MIRValue,
        message: String,
    },

    Assume {
        value: MIRValue,
    },

    Havoc {
        target: MIRRegister,
    },
}

#[derive(Clone, Debug)]
pub enum MIRBinOp {
    ADD, SUB, MUL, DIV,
    IMUL, IDIV,
       
    FADD, FSUB, FMUL, FDIV,
 
    AND, OR, XOR, SHL, SHR,
    
    EQ, NEQ, 
    
    LT, LE, GT, GE,
    ILT, ILE, IGT, IGE,
    FLT, FLE, FGT, FGE,   
}

#[derive(Clone, Debug)]
pub enum MIRUnOp {
    NEG,
    INEG,
    FNEG,
    BNOT,
    LNOT
}

#[derive(Clone, Debug)]
pub enum MIRCoercion {
    Integral,
    FloatingPoint,
    IntToFloat,
    FloatToInt,
    ReinterpretBits
}