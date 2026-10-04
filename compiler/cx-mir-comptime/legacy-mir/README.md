# Legacy MIR comptime reference

These files retain the old THIR-backed MIR comptime instructions, functions,
staged expressions, and types for source parity comparisons with
`cx-thir-lowering` and `cx-mir-comptime`. They were moved from `cx-mir/src`,
preserving their original relative paths and contents.

This directory is not a Rust module or a buildable copy of MIR. Its imports
refer to the former MIR module layout and APIs, including generic instruction
bodies and comptime operands, which have been removed from active MIR. The
legacy THIR crates and MIR evaluator also remain outside the active workspace.

The active compiler evaluates comptime expressions and splices staged code in
`cx-hmir-lowering`. It emits MIR containing concrete runtime types, instructions,
and constants; MIR has no comptime registers, parameters, or staged values.
