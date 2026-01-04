// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

pub mod abi;
pub mod emit;
pub mod lowering;

use cool_frontend::Program;
use inkwell::{context::Context, module::Module};

#[derive(Debug)]
pub enum CodegenError {
    NotImplemented(&'static str),
    Llvm(String),
}

impl std::fmt::Display for CodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodegenError::NotImplemented(s) => write!(f, "codegen not implemented: {s}"),
            CodegenError::Llvm(s) => write!(f, "llvm error: {s}"),
        }
    }
}

impl std::error::Error for CodegenError {}

pub struct Codegen<'ctx> {
    pub ctx: &'ctx Context,
    pub module: Module<'ctx>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(ctx: &'ctx Context, module_name: &str) -> Self {
        let module = ctx.create_module(module_name);
        Self { ctx, module }
    }

    pub fn compile_program(&mut self, _p: &Program) -> Result<(), CodegenError> {
        // Path 2 lowering goes here
        Err(CodegenError::NotImplemented(
            "Program → LLVM lowering pass",
        ))
    }
}
