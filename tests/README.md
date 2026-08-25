# Test suites

The test workspace is split into separate crates so fast correctness tests and deliberate benchmark workloads have independent entry points.

`integration` owns the generated Cargo test suite. Its fixtures retain the existing compile-only, diagnostic, and end-to-end categories under `integration/fixtures`.

`benchmarks` owns the custom two-stage benchmark runner. It measures compilation in fresh temporary directories, then measures repeated execution of a separately compiled artifact. Run it with:

```text
cargo run -p cx-benchmarks -- --format pretty
```

Use `--format json` for machine-readable results and `--format github` for a Markdown summary suitable for `GITHUB_STEP_SUMMARY`. `--json-output PATH` writes the machine-readable report alongside the selected display format, so CI can publish one measurement without running the benchmark twice. LLVM cases require `--features backend-llvm`.

Human-readable timing cells show the mean with a 95% margin of error. Values of one second or longer are displayed in seconds, while shorter values remain in milliseconds; a single sample reports an unavailable margin of error.

CI runs benchmarks on pull requests and on `main`/`dev` pushes. Pull-request runs upload their JSON report, and the trusted report workflow updates one pinned comment with job results, benchmark timings, and percentage deltas against the latest successful baseline artifact for the target branch.

Short output expectations can live beside the source. `CX-STDOUT` starts an exact stdout sequence and each `CX-STDOUT-NEXT` directive adds the immediately following line:

```c
/* CX-STDOUT: Hello, World! */
/* CX-STDOUT-NEXT: A second line */
```

The legacy `.cx-output` sidecar remains supported for fixtures that have not been migrated. Non-empty inline sequences add a final newline by default; append `[no-final-newline]` to the final directive when the program intentionally leaves stdout unterminated. An empty `CX-STDOUT:` directive expects no stdout.
