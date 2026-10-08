# Test suites

The test workspace is split into separate crates so fast correctness tests and deliberate benchmark workloads have independent entry points.

`integration` owns the generated Cargo test suite. Its fixtures retain the existing compile-only, diagnostic, and end-to-end categories under `integration/fixtures`.

`benchmarks` owns the custom two-stage benchmark runner. It measures each build in a fresh temporary directory, then measures repeated runs of the last of those builds. Run it with:

```text
cargo run --release -p cx-benchmarks --features backend-llvm -- --reference clang
```

A case is either a single source file or a `cx.toml` project:

- Every `.c` and `.cx` file under `benchmarks/fixtures` is a case, built at `O2` and run once without arguments. Its stdout expectation lives beside the source, as described below.
- The projects are the C parity examples under `examples/c-parity`, listed in the `PROJECTS` table of `benchmarks/src/case.rs`. They are built from their `cx.toml` where they live, at the optimization level it names, so the upstream submodules must be checked out (`git submodule update --init`). A project is compiled without linking, run alone, or run once for each script in `benchmarks/workloads/<project>`, whose stdout is checked against the `.cx-output` file beside the script.

Stdout is verified on every run, so a case that computes the wrong answer fails the benchmark instead of reporting a time.

`--case` selects cases by project name or by source path and may be repeated. `--backend` takes `available`, `cranelift`, `llvm` or `both`; LLVM requires `--features backend-llvm`, and a project only runs on the backends its table entry lists. `--reference COMMAND` also builds every C case with that C compiler, at the same optimization level, and adds a column that says how many times slower or faster cx was than the reference.

Use `--format json` for machine-readable results and `--format github` for a Markdown summary suitable for `GITHUB_STEP_SUMMARY`. `--json-output PATH` writes the machine-readable report alongside the selected display format, so CI can publish one measurement without running the benchmark twice.

The report (schema 2) holds one row per case, workload and toolchain. A row carries `case`, `backend`, and whichever of `compile` and `execute` apply: a case with several workloads has a build row followed by a `case: workload` row for each. Rows from the reference compiler set `reference: true` and name its command as their backend, and the top-level `reference` records the command and version.

Human-readable timing cells show the mean with a 95% margin of error. Values of one second or longer are displayed in seconds, while shorter values remain in milliseconds; a single sample reports an unavailable margin of error.

CI runs benchmarks on pull requests and on `main`/`dev` pushes. Pull-request runs upload their JSON report, and the trusted report workflow updates one pinned comment with job results, benchmark timings, and percentage deltas against the latest successful baseline artifact for the target branch, alongside the comparison with `clang`.

Short output expectations can live beside the source. `CX-STDOUT` starts an exact stdout sequence and each `CX-STDOUT-NEXT` directive adds the immediately following line:

```c
/* CX-STDOUT: Hello, World! */
/* CX-STDOUT-NEXT: A second line */
```

The legacy `.cx-output` sidecar remains supported for fixtures that have not been migrated. Non-empty inline sequences add a final newline by default; append `[no-final-newline]` to the final directive when the program intentionally leaves stdout unterminated. An empty `CX-STDOUT:` directive expects no stdout.
