# End-to-end tests

Black-box tests that drive the real **`coolc`** executable: they compile COOL
source files, link against the runtime, run the produced native `./a.out`, and
assert on its output. This covers the *entire* pipeline
(lex → parse → typecheck → lower → emit → link → run) — something the in-process
Rust tests (e.g. `crates/cool_codegen/tests/codegen_smoke.rs`, which stops at IR
verification) deliberately don't.

## Requirements

- **Apple Silicon macOS** — the emitted code targets `aarch64-apple-darwin`. On any
  other host the suite is **skipped**, not failed.
- **Xcode Command Line Tools** (`clang`) — used by `coolc` as the linker driver.
- **cargo** — the suite auto-builds `cool_runtime` and `coolc` before running.
- **[uv](https://docs.astral.sh/uv/)** — manages the Python env and runs pytest.

## Running

```bash
cd tests/e2e
uv run pytest -v
```

`uv run` creates/syncs an isolated environment (pytest is declared in
`pyproject.toml`) and runs the tests. A session fixture builds the Rust binaries
(`cargo build -p cool_runtime -p coolc`) automatically, so you don't need to build
them first.

## Layout

| File | Purpose |
| --- | --- |
| `conftest.py` | Fixtures: Apple-Silicon skip, one-time Rust build, and a `Coolc` driver that compiles/runs programs in an isolated temp dir. |
| `test_examples.py` | Tests over the top-level `examples/*.cl` files. |
| `test_arithmetic.py` | Compiles/runs every `examples/arithmetic/*.cl` and checks its output against the sibling `.expected` file. |

## Adding an arithmetic case

Drop two files into `examples/arithmetic/`: `<name>.cl` (a
`class Main { main() : Int { ... } }` using only literals and `+ - * /`) and
`<name>.expected` (the exact stdout it should print). The suite discovers the pair
automatically — no test code changes needed. Programs outside the codegen subset go
in `examples/arithmetic/unsupported/` and are asserted to fail compilation.

## Notes

- Each test runs in its own pytest `tmp_path`, so no `a.out`/`a.out.o` artifacts are
  left in the repo. The runtime archive is reached via a symlink the fixture places
  at `<tmp>/target/debug/libcool_runtime.a` (because `coolc` resolves that path
  relative to its working directory).
- Today's codegen only supports a `Main.main` whose body is an integer expression
  using literals and `+ - * /`. Accordingly, `examples/test.cl` runs and prints `7`,
  while `examples/hello.cl` and `examples/bad.cl` are expected to fail compilation.
  When the backend grows (blocks, parentheses, more types), update the
  expected-failure tests in `test_examples.py` / `test_arithmetic.py`.
