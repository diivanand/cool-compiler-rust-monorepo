// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! `coolc` — the full compiler driver. It wires the stages together end to end:
//!
//! ```text
//!   .cl files ─▶ frontend (lex/parse/typecheck) ─▶ codegen (LLVM IR)
//!             ─▶ emit object (.o) ─▶ link with runtime ─▶ ./a.out
//! ```
//!
//! For a frontend-only view (AST dump + type check, no codegen), see the
//! sibling `cool_parse_cli` binary.

use cool_codegen::{Codegen, emit::emit_object};
use cool_frontend::{combine_sources, lex, parse_program, type_check_program};
use inkwell::context::Context;
use std::{env, path::PathBuf, process::Command};

/// Thin wrapper so the real work can use `?` and we report errors in one place.
fn main() {
    if let Err(e) = run() {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        return Err("Usage: coolc <file1.cl> <file2.cl> ...".to_string());
    }

    // --- Frontend: text -> tokens -> AST -> type-checked AST ---
    let combined = combine_sources(&args)?;
    let toks = lex(&combined).map_err(|e| format!("lexing failed: {e}"))?;
    let prog = parse_program(&toks).map_err(|errs| format!("parse errors:\n{errs:?}"))?;

    if let Err(type_errors) = type_check_program(&prog) {
        // Surface *all* type errors at once (see the type checker's design notes).
        let mut msg = String::from("type check errors:\n");
        for e in type_errors {
            msg.push_str(&format!("  {}\n", e.msg));
        }
        return Err(msg);
    }

    // --- Codegen: AST -> LLVM IR, held in `cg.module` ---
    // The `Context` owns all LLVM data and must outlive the module/builder, so
    // it's created here and borrowed by `Codegen`.
    let ctx = Context::create();
    let mut cg = Codegen::new(&ctx, "cool_module");
    cg.compile_program(&prog)
        .map_err(|e| format!("codegen failed: {e}"))?;

    // --- Emit: LLVM IR -> native arm64 object file ---
    let out_o = PathBuf::from("a.out.o");
    emit_object(&cg.module, &out_o).map_err(|e| format!("failed to emit object: {e}"))?;

    // --- Link: object + runtime static lib -> executable ---
    // We shell out to `clang` as the linker driver. The runtime archive must be
    // built first (`cargo build -p cool_runtime`); `cargo run -p coolc` does NOT
    // rebuild it automatically because coolc depends on it only at link time,
    // not as a Cargo dependency.
    let runtime_a = PathBuf::from("target/debug/libcool_runtime.a");

    let status = Command::new("clang")
        .args(["-target", "aarch64-apple-darwin"])
        .arg(&out_o)
        .arg(&runtime_a)
        .args(["-o", "a.out"])
        .status()
        .map_err(|e| format!("failed to invoke clang: {e}"))?;

    if !status.success() {
        return Err("clang failed during link".to_string());
    }

    println!("Built ./a.out");
    Ok(())
}
