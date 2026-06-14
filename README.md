# COOL Compiler in Rust

A **from-scratch implementation of a full COOL (Classroom Object-Oriented Language) compiler**, written in **Rust**, following the original COOL language specification.

This project implements a complete **frontend** (lexer, parser, AST, and full static type checker) plus a **runtime** with a garbage collector, and is actively growing its **LLVM backend**. It is explicitly designed to be a **complete end-to-end compiler** that:

- Compiles COOL source code to **LLVM IR**
- Lowers LLVM IR to **native Apple Silicon (arm64)** code
- Includes a **custom runtime** for object layout and method dispatch
- Implements a **basic stop-the-world mark-and-sweep garbage collector**
- Produces native macOS executables on Apple Silicon

> 📐 **New here?** See **[ARCHITECTURE.md](ARCHITECTURE.md)** for a guided tour that
> connects COOL language concepts, classic compiler theory, and LLVM/runtime
> internals to the actual modules, functions, and structs in this repo (with
> diagrams).

---

## COOL Language Background

**COOL (Classroom Object-Oriented Language)** was **invented at the University of California, Berkeley** as a teaching language for compiler construction.

The language specification used here is the widely distributed version authored by **Alex Aiken**, later adopted by Stanford and other universities for compiler courses.

Official specification (PDF):

- https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

This project respects the original academic intent of COOL while reimplementing it using **modern systems programming tools**.

---

## Academic Integrity Notice

This repository is **not intended to be used for classroom submissions**.

- The compiler is written in **Rust**, not in the languages typically required by COOL coursework.
- The backend targets **LLVM IR and Apple Silicon (arm64)**, which differs significantly from most instructional COOL compilers.
- The architecture, runtime, and garbage collector go far beyond standard course requirements.

**Students should not submit this code (or derivatives of it) for academic credit.**  
Doing so would likely violate course academic integrity policies.

This project is intended for:
- Learning
- Research
- Personal experimentation and growth (Learn and Improve Rust and Compiler Programming Skills)
- Reference-quality compiler engineering

---

### Lexer — Complete
- Implemented using **Logos**
- Handles keywords, identifiers, literals, operators, and comments
- Produces a clean token stream for parsing

### Parser — Complete
- Implemented using **Chumsky**
- Builds a fully-typed **Abstract Syntax Tree (AST)**
- Uses a **recursive-descent + Pratt parsing** hybrid approach
- Correctly encodes COOL operator precedence and associativity

### Static Type Checker — Complete
- Implements the **static semantic rules from Section 12 of the COOL specification**
- Performs:
  - Class table construction
  - Inheritance validation and cycle detection
  - Attribute and method signature validation
  - Method override checking
  - Full expression type checking
  - `SELF_TYPE` handling
  - Least-upper-bound (LUB) computation
  - Conformance (`≤`) checks
- Detects and reports **multiple type errors in a single run**

### LLVM IR Code Generation — In Progress
- Uses **Inkwell** targeting **LLVM 21.1.x**
- Infrastructure in place for:
  - Class layout and tagging
  - Object allocation via runtime calls
  - Vtable-based dynamic dispatch
  - Emission of native arm64 object files
- Lowering from COOL AST to LLVM IR is actively under development

### COOL Runtime (Apple Silicon arm64) — In Progress
- Custom runtime crate with:
  - Stable object header layout
  - Explicit vtable structures
  - C-compatible ABI for generated code
- Runtime functions exposed via `extern "C"` for LLVM-generated calls

### Garbage Collection — Implemented (Evolving)
- **Stop-the-world, mark-and-sweep GC**
- **Precise root tracking via a shadow stack**
- GC metadata derived from compiler-emitted pointer maps
- Single-threaded, deterministic design
- Unit-tested at the runtime level

### Overall Status

At this stage, the compiler can:
- Parse one or more `.cl` files as a single program
- Build a complete AST
- Perform full semantic validation
- Reject ill-typed COOL programs with meaningful diagnostics
- In Progress: Emit native arm64 object files linked against a custom runtime

---

## 🗂 Repository Structure

```text
cool-compiler-rust-monorepo/
├── crates/
│   ├── cool_frontend/
│   │   ├── src/
│   │   │   ├── ast.rs          # AST definitions
│   │   │   ├── lexer.rs        # Logos-based lexer
│   │   │   ├── parser.rs       # Chumsky-based parser with Pratt parsing
│   │   │   ├── typechecker.rs  # COOL static type checker (Section 12)
│   │   │   └── lib.rs          # Frontend public API
│   │   └── Cargo.toml
│   │
│   ├── cool_codegen/
│   │   ├── src/
│   │   │   ├── abi.rs          # Runtime ABI definitions
│   │   │   ├── emit.rs         # LLVM object emission (arm64)
│   │   │   ├── lowering.rs     # AST → LLVM IR lowering (in progress)
│   │   │   └── lib.rs
│   │   └── Cargo.toml
│   │
│   ├── cool_runtime/
│   │   ├── src/
│   │   │   ├── gc.rs           # Mark-and-sweep garbage collector
│   │   │   ├── rt_print.rs     # Runtime I/O helpers
│   │   │   └── lib.rs
│   │   └── Cargo.toml
│   │
│   ├── cool_parse_cli/
│   │   └── src/main.rs         # Frontend-only CLI (AST + type checking)
│   │
│   └── coolc/
│       └── src/main.rs         # Full compiler driver
│
├── examples/                   # Sample .cl programs
├── tests/
│   └── e2e/                    # Python (uv + pytest) end-to-end tests of ./coolc
│
├── ARCHITECTURE.md             # Guided tour: concepts ↔ code
├── LICENSE.md                  # Apache 2.0 license
├── NOTICE.md                   # Attribution notice
└── README.md
```

## 🚀 Getting Started

### Prerequisites

- **Rust** (2024 edition; install via [rustup](https://rustup.rs))
- **LLVM 21.1.x** — required by the `inkwell` backend (`cool_codegen`, `coolc`).
  On macOS: `brew install llvm@21`, then point the build at it:
  ```bash
  export LLVM_SYS_211_PREFIX="$(brew --prefix llvm@21)"
  ```
- **clang** — used by `coolc` as the linker driver (ships with Xcode CLT).
- **Apple Silicon (arm64) macOS** — the emitted object/executable targets
  `aarch64-apple-darwin`.

> The frontend-only crates (`cool_frontend`, `cool_parse_cli`) do **not** need
> LLVM, so you can explore the lexer/parser/type checker without it.

### Build & test

```bash
cargo build --workspace      # build everything
cargo test  --workspace      # run the full test suite
cargo clippy --workspace     # lint
```

### Inspect the frontend (no LLVM needed)

`cool_parse_cli` lexes + parses + type-checks and dumps the AST:

```bash
cargo run -p cool_parse_cli -- examples/test.cl     # prints the AST, then "OK"
cargo run -p cool_parse_cli -- examples/bad.cl      # reports a type error
```

### Compile a program to a native executable

```bash
# IMPORTANT: build the runtime static lib first. coolc links against
# target/debug/libcool_runtime.a at link time, and `cargo run -p coolc` does NOT
# rebuild it automatically (it isn't a Cargo dependency of coolc).
cargo build -p cool_runtime

cargo run -p coolc -- examples/test.cl              # writes a.out.o and ./a.out
./a.out                                             # prints: 7
```

`examples/test.cl` is `class Main { main() : Int { 1 + 2 * 3 } }`, so the program
prints **`7`** (note `*` binds tighter than `+`). You can pass multiple `.cl`
files; they are concatenated into one program.

> **Backend scope today:** code generation currently handles a demonstrative
> subset — a `Main.main` whose body is an integer arithmetic expression — exercising
> the whole pipeline (object model, vtable dispatch, allocation, GC, linking) end
> to end. Generalizing lowering to all classes/expressions is the active frontier.
> The frontend and type checker handle the full language.

### End-to-end tests (Python)

In addition to the Rust unit/integration tests (`cargo test --workspace`), there is a
black-box e2e suite that actually compiles `.cl` files with `coolc`, runs the resulting
native binary, and checks its output. It uses **[uv](https://docs.astral.sh/uv/)** +
pytest and auto-builds the Rust binaries:

```bash
cd tests/e2e
uv run pytest -v
```

The suite runs on Apple Silicon macOS and is skipped elsewhere. See
[`tests/e2e/README.md`](tests/e2e/README.md) for details.

---

## Parsing Strategy

This compiler uses a **recursive-descent Pratt parser** approach for parsing.

### Why Pratt Parsing and what is it?

COOL’s expression grammar contains many operators with different precedence and associativity (arithmetic, comparison, assignment, dispatch, etc.). A Pratt parser (also known as *Top-Down Operator Precedence parsing*) provides a clean and extensible way to handle this without complex grammar rewrites or parser generators.

The benefits of this approach include:

- **Explicit precedence handling** without left-recursive grammar rules
- **Simple, readable implementation** in idiomatic Rust
- **Easy extensibility** for adding new operators or expression forms
- Natural integration with a **hand-written recursive-descent parser** for non-expression constructs (classes, features, formals, etc.)

These two resources give a clear explanation of Pratt Parsing:
- Matklad’s article *“Simple but Powerful Pratt Parsing”*  
  https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- The Pratt parsing walkthrough by Bob Nystrom (author of *Crafting Interpreters*):  
  https://www.youtube.com/watch?v=0c8b7YfsBKs

### High-Level Approach

- Non-expression constructs (programs, classes, features, methods, attributes) are parsed using **standard recursive descent**.
- Expressions are parsed using a **Pratt parser**, driven by:
  - *null denotation (nud)* functions for prefix expressions
  - *left denotation (led)* functions for infix and postfix expressions
  - A precedence table that encodes COOL operator precedence and associativity

This hybrid strategy keeps the parser:

- Predictable and debuggable
- Free of external parser generators
- Faithful to the original COOL specification while remaining maintainable
