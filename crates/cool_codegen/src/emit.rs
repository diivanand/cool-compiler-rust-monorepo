// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use inkwell::{
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    OptimizationLevel,
};
use std::path::Path;

pub fn emit_object(module: &Module, out: &Path) -> Result<(), String> {
    Target::initialize_all(&InitializationConfig::default());

    let triple = TargetTriple::create("aarch64-apple-darwin");
    let target = Target::from_triple(&triple).map_err(|e| format!("{e:?}"))?;

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
