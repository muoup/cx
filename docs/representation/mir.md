# Mid-level Intermediate Representation

The mid-level intermediate representation (MIR) is a flat SSA-style IR modeled to be higher-level than the code generation LMIR which is intended as a backend agnostic IR that is easy to translate to both LLVM IR and Cranelift IR. MIR is also designed and kept to be easily-analyzable for control-flow related analysis. Safety mechanisms such as the following are implemented via MIR analysis:

 - Liveness tracking
 - Const value propogation for tautological assertion failures
 - Ghost variable value tracking (to be implemented)

MIR intentionally avoids any notion of memory or registers, we carry over the term 'register' to represent virtual registers in the SSA sense, and abstract away the concept of memory via 'places'. Places are virtual regions containing contiguous memory. 