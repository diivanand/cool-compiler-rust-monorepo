# COOL Compiler in Rust

A **from-scratch implementation of a full COOL (Classroom Object-Oriented Language) compiler**, written in **Rust**, following the original COOL language specification.

This project currently implements the **frontend** (lexer, parser, AST) and is explicitly designed to evolve into a **complete end-to-end compiler** that:

- Compiles COOL source code to **LLVM IR**
- Lowers LLVM IR to **native Apple Silicon (arm64)** code
- Includes a **custom runtime** for object layout and method dispatch
- Implements a **basic stop-the-world mark-and-sweep garbage collector**
- Produces native macOS executables on Apple Silicon

---

## 📚 COOL Language Background

**COOL (Classroom Object-Oriented Language)** was **invented at the University of California, Berkeley** as a teaching language for compiler construction.

The language specification used here is the widely distributed version authored by **Alex Aiken**, later adopted by Stanford and other universities for compiler courses.

Official specification (PDF):

- https://theory.stanford.edu/~aiken/software/cool/cool-manual.pdf

This project respects the original academic intent of COOL while reimplementing it using **modern systems programming tools**.

---

## ⚠️ Academic Integrity Notice

This repository is **not intended to be used for classroom submissions**.

- The compiler is written in **Rust**, not in the languages typically required by COOL coursework.
- The backend targets **LLVM IR and Apple Silicon (arm64)**, which differs significantly from most instructional COOL compilers.
- The architecture, runtime, and garbage collector go far beyond standard course requirements.

**Students should not submit this code (or derivatives of it) for academic credit.**  
Doing so would likely violate course academic integrity policies.

This project is intended for:
- Learning
- Research
- Personal experimentation
- Reference-quality compiler engineering

---

## 🗂 Repository Structure

```text
cool-compiler-rust-monorepo/
├── crates/
│   ├── cool_frontend/
│   │   ├── src/
│   │   │   ├── ast.rs        # AST definitions
│   │   │   ├── lexer.rs      # Logos-based lexer (Rust)
│   │   │   ├── parser.rs    # Chumsky-based parser (Rust)
│   │   │   └── lib.rs
│   │   └── Cargo.toml
│   │
│   └── cool_parse_cli/
│       ├── src/
│       │   └── main.rs       # CLI: parse .cl files → AST
│       └── Cargo.toml
│
├── LICENSE.md                # Apache 2.0 license
├── NOTICE.md                 # Attribution notice
└── README.md
