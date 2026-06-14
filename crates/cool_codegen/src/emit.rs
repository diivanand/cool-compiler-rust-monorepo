// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! Object-file emission: hand the finished LLVM module to a target machine and
//! write a native `.o`.
//!
//! This is where the platform-independent IR becomes machine code for a specific
//! CPU. We hardcode Apple Silicon (`aarch64-apple-darwin`) since that's the
//! project's stated target; making this configurable is a natural extension.

use inkwell::{
    OptimizationLevel,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
};
use std::path::Path;

/// Compile `module` to a native object file at `out` for `arm64` macOS.
pub fn emit_object(module: &Module, out: &Path) -> Result<(), String> {
    // Register LLVM's target backends so `from_triple` can find AArch64.
    Target::initialize_all(&InitializationConfig::default());

    let triple = TargetTriple::create("aarch64-apple-darwin");
    let target = Target::from_triple(&triple).map_err(|e| format!("{e:?}"))?;

    // A "target machine" bundles the triple with codegen knobs: CPU model,
    // feature flags, optimization level, relocation model (PIC for position-
    // independent code, required on macOS), and code model.
    let cpu = "apple-m1"; // reasonable default for Apple Silicon
    let features = "";

    let tm = target
        .create_target_machine(
            &triple,
            cpu,
            features,
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| "Failed to create TargetMachine".to_string())?;

    tm.write_to_file(module, FileType::Object, out)
        .map_err(|e| format!("{e:?}"))?;

    Ok(())
}
