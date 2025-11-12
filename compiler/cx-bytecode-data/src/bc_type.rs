use cx_mir_data::LinkageType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BCFunctionPrototype {
    pub name: String,
    pub return_type: BCType,
    pub parameter_types: Vec<BCType>,
    pub var_args: bool,
    pub linkage: LinkageType,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BCType {
    /**
     *  Opaque pointer type. Holds no information as to what it points to,
     *  but allows marking hints for compiler optimizations. 
     */
    Pointer {
        dereferenceable: usize,
        nonnull: bool,
    },
    
    /** 
     *  A structured type padded to alignment boundaries.
     */
    Structured {
        name: String,
        fields: Vec<(String, BCType)>,
    },
    
    /**
     *  An array type with fixed size.
     */
    Array {
        element: Box<BCType>,
        size: usize,
    },
    
    Integer(IntegerType),
    Float(FloatType),
       
    Bool,
    
    /**
     *  Fallback type; no type information, just some N-sized string of data.
     */
    Opaque {
        size: usize,
    },
    
    /** 
     *  Void type; represents absence of a value.
     */
    Unit
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum IntegerType {
    I8,
    I16,
    I32,
    I64,
    I128
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum FloatType {
    F32,
    F64,
}

impl BCType {
    pub fn size_in_bytes(&self) -> usize {
        match self {
            BCType::Pointer { .. } => std::mem::size_of::<usize>(),
            BCType::Structured { fields, .. } => {
                fields.iter().map(|(_, t)| t.size_in_bytes()).sum()
            }
            BCType::Array { element, size } => element.size_in_bytes() * size,
            BCType::Integer(int_type) => match int_type {
                IntegerType::I8 => 1,
                IntegerType::I16 => 2,
                IntegerType::I32 => 4,
                IntegerType::I64 => 8,
                IntegerType::I128 => 16,
            },
            BCType::Float(float_type) => match float_type {
                FloatType::F32 => 4,
                FloatType::F64 => 8,
            },
            BCType::Opaque { size } => *size,
            BCType::Bool => 1,
            BCType::Unit => 0
        }
    }
    
    pub fn default_pointer() -> Self {
        BCType::Pointer {
            dereferenceable: 0,
            nonnull: false,
        }
    }
 
    pub fn is_pointer(&self) -> bool {
        matches!(self, BCType::Pointer { .. })
    }
    
    pub fn is_unit(&self) -> bool {
        matches!(self, BCType::Unit)
    }
    
    pub fn is_opaque(&self) -> bool {
        matches!(self, BCType::Opaque { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, BCType::Integer(_))
    }
    
    pub fn is_float(&self) -> bool {
        matches!(self, BCType::Float(_))
    }
    
    pub fn is_structured(&self) -> bool {
        matches!(self, BCType::Structured { .. })
    }

    pub fn get_integer_type(&self) -> Option<IntegerType> {
        if let BCType::Integer(int_type) = self {
            Some(int_type.clone())
        } else {
            None
        }
    }
    
    pub fn get_float_type(&self) -> Option<FloatType> {
        if let BCType::Float(float_type) = self {
            Some(float_type.clone())
        } else {
            None
        }
    }
}