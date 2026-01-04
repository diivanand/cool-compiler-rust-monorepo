// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use cool_frontend::{
    lex, parse_program, strip_comments, type_check_program,
};
use cool_codegen::{emit::emit_object, Codegen};
use inkwell::context::Context;
use std::{
    env,
    fs,
    path::PathBuf,
    process::Command,
};

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

    // Combine input files
    let mut combined = String::new();
    for (i, path) in args.iter().enumerate() {
        let src = fs::read_to_string(path)
            .map_err(|e| format!("failed to read {path}: {e}"))?;
        if i > 0 {
            combined.push_str("\n\n");
        }
        combined.push_str(&src);
    }

    // Frontend
    let combined =
        strip_comments(&combined).map_err(|e| format!("comment stripping failed: {e}"))?;
    let toks = lex(&combined).map_err(|e| format!("lexing failed: {e}"))?;
    let prog = parse_program(&toks)
        .map_err(|errs| format!("parse errors:\n{errs:?}"))?;

    if let Err(type_errors) = type_check_program(&prog) {
        let mut msg = String::from("type check errors:\n");
        for e in type_errors {
            msg.push_str(&format!("  {}\n", e.msg));
        }
        return Err(msg);
    }

    // Codegen
    let ctx = Context::create();
    let mut cg = Codegen::new(&ctx, "cool_module");
    cg.compile_program(&prog)
        .map_err(|e| format!("codegen failed: {e}"))?;

    // Emit object
    let out_o = PathBuf::from("a.out.o");
    emit_object(&cg.module, &out_o)
        .map_err(|e| format!("failed to emit object: {e}"))?;

    // Link with runtime
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
