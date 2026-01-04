# COOL Compiler in Rust

A **from-scratch implementation of a full COOL (Classroom Object-Oriented Language) compiler**, written in **Rust**, following the original COOL language specification.

This project currently implements the **frontend** (lexer, parser, AST) and is explicitly designed to evolve into a **complete end-to-end compiler** that:

- Compiles COOL source code to **LLVM IR**
- Lowers LLVM IR to **native Apple Silicon (arm64)** code
- Includes a **custom runtime** for object layout and method dispatch
- Implements a **basic stop-the-world mark-and-sweep garbage collector**
- Produces native macOS executables on Apple Silicon

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
├── LICENSE.md                  # Apache 2.0 license
├── NOTICE.md                   # Attribution notice
└── README.md
```

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
