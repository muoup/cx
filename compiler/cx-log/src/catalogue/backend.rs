use super::define_errors;

define_errors! {
    CRANELIFT_PARAM_INDEX: String = "B0033" => |index| format!("Function parameter index out of bounds: {index}");
    CRANELIFT_FUNCTION: String = "B0034" => |name| format!("Function not found: {name}");
    CRANELIFT_FLOAT: String = "B0035" => |ty| format!("Float immediate has non-float type: {ty}");
    CRANELIFT_GLOBAL: String = "B0036" => |id| format!("Global not found: {id}");
    CRANELIFT_VARIABLE: String = "B0037" => |value| format!("Variable not found in variable table: {value}");
    CRANELIFT_EMIT: String = "B0038" => |error| format!("Failed to emit object file: {error}");
    CRANELIFT_VECTOR: String = "B0039" => |ty| format!("Unsupported vector type for codegen: {ty}");
    CRANELIFT_TYPE: String = "B0040" => |ty| format!("Unsupported type for codegen: {ty}");
    VARIADIC_UNIMPLEMENTED: () = "B0041" => |()| "Cranelift lowering of variadic builtins is not implemented".into();
    
    NULL_RUNTIME_VALUE: () = "B0042" => |()| "LMIR attempted to store a value with no runtime representation".into();
    TARGET_POINTER_SIZE: (usize, usize) = "B0043" => |(lmir, cranelift)| format!("LMIR target uses pointer size {lmir}, but Cranelift target uses {cranelift}");
    FUNCTION_MAP_NOT_FOUND: String = "B0044" => |name| format!("Function not found in function map: {name}");
    PARAMETER_INDEX_OUT_OF_BOUNDS_FOR_FUNCTION: (String, String) = "B0057" => |(arg0, arg1)| format!("Parameter index {arg0} out of bounds for function {arg1}");
    FUNCTION_REFERENCE_WAS_USED_WHERE_A_GENERATED_VALUE_WAS_EXPECTED: String = "B0058" => |arg0| format!("Function reference {arg0} was used where a generated value was expected");
    VALUE_WAS_NOT_GENERATED: String = "B0059" => |arg0| format!("Value {arg0} was not generated");
    BLOCK_WITH_ID_WAS_NOT_GENERATED: String = "B0060" => |arg0| format!("Block with ID {arg0} was not generated");
    BLOCK_PARAMETERS_FOR_WERE_NOT_GENERATED: String = "B0061" => |arg0| format!("Block parameters for {arg0} were not generated");
    LMIR_EDGE_TO_HAS_ARGUMENTS_FOR_PARAMETERS: (String, String, String) = "B0062" => |(arg0, arg1, arg2)| format!("LMIR edge to {arg0} has {arg1} arguments for {arg2} parameters");
    EXPECTED_A_SCALAR_LLVM_VALUE_FOUND: String = "B0063" => |arg0| format!("Expected a scalar LLVM value, found: {arg0}");
    EXPECTED_A_BASIC_LLVM_VALUE_FOUND: String = "B0064" => |arg0| format!("Expected a basic LLVM value, found: {arg0}");
    FAILED_TO_CREATE_LLVM_TARGET_MACHINE: () = "B0065" => |()| "Failed to create LLVM target machine".into();
    FUNCTION_WAS_NOT_DECLARED_IN_THE_LLVM_MODULE: String = "B0066" => |arg0| format!("Function {arg0} was not declared in the LLVM module");
    FUNCTION_HAS_NO_LMIR_BLOCKS: String = "B0067" => |arg0| format!("Function {arg0} has no LMIR blocks");
    EXPECTED_A_BASIC_LLVM_TYPE_FOUND: String = "B0068" => |arg0| format!("Expected a basic LLVM type, found {arg0}");
    EXPECTED_A_BASIC_LLVM_VALUE_FOUND_DIAGNOSTIC: String = "B0069" => |arg0| format!("Expected a basic LLVM value, found {arg0}");
    INVALID_LLVM_FUNCTION_RETURN_TYPE: String = "B0070" => |arg0| format!("Invalid LLVM function return type: {arg0}");
    INVALID_GLOBAL_DEFINITION_INDEX: String = "B0071" => |arg0| format!("Invalid global definition index {arg0}");
    INVALID_FIELD_INDEX_IN_LLVM_STRUCT_INITIALIZER: String = "B0072" => |arg0| format!("Invalid field index {arg0} in LLVM struct initializer");
    AGGREGATE_INITIALIZER_USED_WITH_NON_AGGREGATE_LLVM_TYPE: () = "B0073" => |()| "Aggregate initializer used with non-aggregate LLVM type".into();
    INVALID_GLOBAL_INITIALIZER_REFERENCE: String = "B0074" => |arg0| format!("Invalid global initializer reference {arg0}");
    INVALID_FUNCTION_INITIALIZER_REFERENCE: String = "B0075" => |arg0| format!("Invalid function initializer reference {arg0}");
    INVALID_FLOATING_POINT_COMPARISON_OPERATION: () = "B0076" => |()| "Invalid floating-point comparison operation".into();
    LLVM_LOAD_DID_NOT_PRODUCE_AN_INSTRUCTION: () = "B0077" => |()| "LLVM load did not produce an instruction".into();
    FUNCTION_WAS_NOT_DECLARED: String = "B0078" => |arg0| format!("Function {arg0} was not declared");
    NO_LLVM_INSERTION_BLOCK_FOR_JUMP: () = "B0079" => |()| "No LLVM insertion block for jump".into();
    LLVM_BRANCH_CONDITION_IS_NOT_AN_INTEGER_OR_POINTER: () = "B0080" => |()| "LLVM branch condition is not an integer or pointer".into();
    LLVM_INTRINSIC_WAS_NOT_FOUND: String = "B0081" => |arg0| format!("LLVM intrinsic {arg0} was not found");
    LLVM_INTRINSIC_WAS_NOT_DECLARED: String = "B0082" => |arg0| format!("LLVM intrinsic {arg0} was not declared");
    NO_LLVM_INSERTION_BLOCK_WHILE_ALLOCATING_MEMORY: () = "B0083" => |()| "No LLVM insertion block while allocating memory".into();
    LLVM_ENTRY_BLOCK_HAS_NO_INSTRUCTION: () = "B0084" => |()| "LLVM entry block has no instruction".into();
    LLVM_ALLOCA_DID_NOT_PRODUCE_AN_INSTRUCTION: () = "B0085" => |()| "LLVM alloca did not produce an instruction".into();
    EXPECTED_STRUCT_TYPE_FOR_STRUCT_ACCESS_FOUND: String = "B0086" => |arg0| format!("Expected struct type for struct access, found {arg0}");
    LLVM_OPERATION_FAILED: String = "B0087" => |error| format!("LLVM operation failed: {error}");
    LLVM_TARGET_LAYOUT: (usize, usize, usize, usize) = "B0088" => |(size, alignment, llvm_size, llvm_alignment)| format!("LMIR target uses pointer size/alignment {size}/{alignment}, but LLVM target uses {llvm_size}/{llvm_alignment}");
}
