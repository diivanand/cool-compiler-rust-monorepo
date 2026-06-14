// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! `cool_codegen` — the backend that lowers a type-checked AST to LLVM IR and
//! emits a native object file.
//!
//! It builds on [`inkwell`], a safe Rust wrapper around the LLVM C API. The
//! moving parts:
//!
//! - [`mod@abi`] — constants describing the runtime object layout and the names
//!   of the runtime's C entry points. These MUST stay in sync with `cool_runtime`.
//! - [`mod@lowering`] — walks the AST and emits LLVM IR (the interesting part).
//! - [`mod@emit`] — runs the LLVM target machine to write an `arm64` object file.
//!
//! > **Status:** lowering currently handles a small, hardcoded subset — a `Main`
//! > class whose `main` returns an integer arithmetic expression — enough to
//! > demonstrate the object model (boxed `Int`), vtables, and dynamic dispatch
//! > end-to-end. It is the active frontier of the project.

pub mod abi;
pub mod emit;
pub mod lowering;

use cool_frontend::Program;
use inkwell::{builder::BuilderError, context::Context, module::Module};

/// Everything that can go wrong while generating code.
#[derive(Debug)]
pub enum CodegenError {
    /// A path that is planned but not yet written.
    NotImplemented(&'static str),
    /// An error bubbled up from LLVM/inkwell (e.g. a failed IR builder call).
    Llvm(String),
    MissingMainClass,
    MissingMainMethod,
    /// A language construct the current (subset) backend doesn't accept yet.
    Unsupported(&'static str),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::NotImplemented(s) => write!(f, "codegen not implemented: {s}"),
            CodegenError::Llvm(s) => write!(f, "llvm error: {s}"),
            CodegenError::MissingMainClass => write!(f, "missing class Main"),
            CodegenError::MissingMainMethod => write!(f, "missing method Main.main"),
            CodegenError::Unsupported(s) => write!(f, "unsupported feature in codegen: {s}"),
        }
    }
}

impl std::error::Error for CodegenError {}

/// Bridge inkwell's IR-builder error into ours. Implementing `From` lets the
/// lowering code use the `?` operator on every `builder.build_*` call instead of
/// wrapping each one in an explicit `.map_err(...)` — a big readability win given
/// how many builder calls codegen makes.
impl From<BuilderError> for CodegenError {
    fn from(e: BuilderError) -> Self {
        CodegenError::Llvm(format!("{e:?}"))
    }
}

/// Owns the LLVM `Context` borrow and the `Module` we're building into.
///
/// The `'ctx` lifetime ties every IR value we create back to the `Context` that
/// owns it; LLVM objects are only valid as long as their context lives, and the
/// borrow checker enforces exactly that for us here.
pub struct Codegen<'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(ctx: &'ctx Context, module_name: &str) -> Self {
        let module = ctx.create_module(module_name);
        Self { ctx, module }
    }

    /// Lower a whole (already type-checked) program into this module's IR.
    pub fn compile_program(&mut self, p: &Program) -> Result<(), CodegenError> {
        lowering::lower_program(self.ctx, &self.module, p)
    }
}
