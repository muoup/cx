# Test suites

The test workspace is split into separate crates so fast correctness tests and deliberate benchmark workloads have independent entry points.

`integration` owns the generated Cargo test suite. Its fixtures retain the existing compile-only, diagnostic, and end-to-end categories under `integration/fixtures`.

`benchmarks` owns the custom two-stage benchmark runner. It measures compilation in fresh temporary directories, then measures repeated execution of a separately compiled artifact. Run it with:

```text
cargo run -p cx-benchmarks -- --format pretty
```

Use `--format json` for machine-readable results and `--format github` for a Markdown summary suitable for `GITHUB_STEP_SUMMARY`. LLVM cases require `--features backend-llvm`.

Short output expectations can live beside the source. `CX-STDOUT` starts an exact stdout sequence and each `CX-STDOUT-NEXT` directive adds the immediately following line:

```c
/* CX-STDOUT: Hello, World! */
/* CX-STDOUT-NEXT: A second line */
```

The existing `.cx-output` sidecar remains supported while fixtures are migrated. The inline format always adds a final newline to the expected sequence.
