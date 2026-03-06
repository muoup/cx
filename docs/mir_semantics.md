# MIR Semantics

## High-Level

Source -> Preprocessed -> AST -> MIR -> Bytecode -> LLVM IR / Cranelift -> Machine Code
                                 ^
MIR (Mid-level intermediate representation) is a flattened, typechecked representation of a CX AST. It can be thought of as a higher-level version of LLVM IR or similar that does not erase high-level constructs and language specific semantics to power formal verification (and optimizations in the future) specific to CX. 

The main struggle of implementating formal verification for a language like CX is that unlike more traditional formally-verified functional languages, C and by extension CX allows for unrestricted use of mutation and direct  memory access, removing many of the guarantees like referential transparency that comes with a pure language. Any  C program should be valid CX code, meaning we cannot simply disallow these inconvenient features, and as such, MIR attempts to separate mutative and pure constructs as much as possible, while certain language features and idioms will allow developers to assist the compiler to be able to make more strong deductions about their code.

## Containerizing Mutation

C semantics guarantee that, in virtually all cases, code cannot reason in a defined manner the relationship between two separately allocated memory regions. Even on the stack, we cannot guarantee any particular layout of separate variables, as the compiler can reorder, or completely eliminate / inline variables as it sees fit. This allows us to containerize mutation in the context of a "region". In the context of MIR, we have no notion of memory, and have no need for a unified memory space, instead we can think of memory as separate boxed regions, which one can have a  reference to either the whole region, or some sub-region of it. This does not necessarily fix all issues mentioned  before, but given that mutation cannot exist in the context of temporaries, we can mandate that mutation may thus  only exist in the context of a region given MIR's SSA semantics.

## Optimizing

--- Please note that MIR optimizations and formal verification in general are not yet implemented, this section is purely for planning and fleshing out ideas. ---

Optimizations in MIR are not inherently done for the sake of performance, but rather for strengthening the guarantees the compiler can make about the code, thus allowing for more powerful formal verification.

A simple example of an optimization worth implementing that demonstrates this difference would be a modified Mem2Reg pass. Consider the following code.

```c

int function() where
    post(ret): (ret > 0)
{
    const int x = 0;
    
    ...
    
    return x;
}

```

In a naive code generator, since 'x' still has a referenceable address, the 'x' variable would be created as a region, and then loaded when returning. While it is still possible to create reasoning logic in the verifier to handle this, this is a perfect example of how regions can overcomplicate reasoning about code. Since we know 'x' is constant, and thus cannot be mutated, we can inline any reference to the value of 'x' directly with 0, and as long as there is no request for the address of 'x', we can completely eliminate the region for 'x', simplifying reasoning about the code.

If we envision the difference here in approximately how the MIR looks before and after optimization, we can see the following:

```

fn function_before() -> int:
.entry:
  %1 = create_region int
  region_write i32& %1, i32 0
  
  ...
  
  %2 = region_read i32& %1 as i32
  %3 = binop(gt) i32 %2, 0
  assert %3
  return %2

fn function_after() -> int:
.entry:
  %1 = create_region int   # if no address-of x is taken, this can be eliminated
  
  ...
  
  %3 = binop(gt) i32 0
  assert %3
  return 0

```

In this example, it becomes pretty clear the difference when we compare the assertion code here. Before this 'optimization', the verifier would have to spend time attempting to deduce the value of the region read from %1, which could pose problems with something such as an ill-placed havoc instruction, while after the optimization, the  verifier can immediately see that %3 is always false, and thus the postcondition is violated.

It is worth noting that the type system of MIR still contains const correctness, and thus a verifier, if given   directly the first example, could realize that the region is of a const type, and cannot be mutated, but the  reasoning to verify the contract assertion would be essentially the reasoning we did in this optimization, but likely requiring redudant reasoning and being generally less efficient.