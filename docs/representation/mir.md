# Mid-level Intermediate Representation

The mid-level intermediate representation (MIR) is a flat SSA-style IR modeled to be higher-level than the code generation LMIR which is intended as a backend agnostic IR that is easy to translate to both LLVM IR and Cranelift IR. MIR is also designed and kept to be easily-analyzable for control-flow related analysis. Safety mechanisms such as the following are implemented via MIR analysis:

 - Liveness tracking
 - Const value propogation for tautological assertion failures
 - Ghost variable value tracking (to be implemented)

MIR uses virtual SSA registers and abstracts storage through 'places'. Places are virtual regions containing contiguous memory.

MIR is the result of staging HMIR in `cx-hmir-lowering`. The lowering evaluator handles comptime expressions, types, function specialization, and staged quotes before emitting runtime MIR. MIR bodies and basic blocks contain only `MIRInstruction`; they have no comptime parameters, registers, or operands. Evaluated data can appear as ordinary `MIRConstant` values and global initializers, while type and quote values remain within HMIR lowering.
