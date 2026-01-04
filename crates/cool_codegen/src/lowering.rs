// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! Lowering from COOL AST to LLVM IR.
//!
//! - Build class layout and inheritance graph
//! - Assign class tags
//! - Compute field layouts
//! - Emit pointer-offset tables for GC
//! - Emit vtables
//! - Emit method bodies
//! - Lower expressions with dispatch via vtables

use crate::CodegenError;

pub fn lower_program() -> Result<(), CodegenError> {
    Err(CodegenError::NotImplemented(
        "lower_program not implemented",
    ))
}
