// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! `cool_parse_cli` — a frontend-only tool for inspecting how the compiler sees
//! your program. It lexes, parses, pretty-prints the AST, then type-checks —
//! stopping before code generation. Handy for learning and for debugging the
//! parser/type checker without needing the LLVM toolchain.

use std::{env, process};

use cool_frontend::{combine_sources, lex, parse_program, type_check_program};

/// Print usage and exit with a non-zero status.
fn usage_and_exit() -> ! {
    eprintln!("Usage: cool_parse_cli <file1.cl> <file2.cl> ...");
    eprintln!("Example: cool_parse_cli examples/Main.cl examples/IO.cl");
    process::exit(2);
}

fn main() {
    let paths: Vec<String> = env::args().skip(1).collect();
    if paths.is_empty() {
        usage_and_exit();
    }

    // Read and concatenate all inputs into one logical program (shared with coolc).
    let combined = combine_sources(&paths).unwrap_or_else(|e| {
        eprintln!("{e}");
        process::exit(2);
    });

    // Stage 1: lexing (comment stripping happens inside `lex`).
    let toks = lex(&combined).unwrap_or_else(|e| {
        eprintln!("Lex error: {e}");
        process::exit(1);
    });

    // Stage 2: parsing into an AST, which we dump with the pretty `{:#?}` format.
    let prog = parse_program(&toks).unwrap_or_else(|errs| {
        eprintln!("Parse errors:");
        for e in errs {
            eprintln!("  {e:?}");
        }
        process::exit(1);
    });

    println!("=== AST ===");
    println!("{prog:#?}");

    // Stage 3: static type checking.
    match type_check_program(&prog) {
        Ok(()) => {
            println!("=== Type Check ===");
            println!("OK");
        }
        Err(errs) => {
            eprintln!("=== Type Check Errors ===");
            for e in errs {
                eprintln!("  {}", e.msg);
            }
            process::exit(1);
        }
    }
}
