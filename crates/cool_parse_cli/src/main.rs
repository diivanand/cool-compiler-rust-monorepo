// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0
use std::{env, fs, process};

use cool_frontend::{lex, parse_program, strip_comments, type_check_program};

fn usage_and_exit() -> ! {
    eprintln!("Usage: cool_parse_cli <file1.cl> <file2.cl> ...");
    eprintln!("Example: cool_parse_cli examples/Main.cl examples/IO.cl");
    process::exit(2);
}

fn main() {
    let mut args = env::args();
    let _bin = args.next();

    let paths: Vec<String> = args.collect();
    if paths.is_empty() {
        usage_and_exit();
    }

    // Read & concatenate all files into one logical "program".
    // Add separators to avoid accidental token gluing at boundaries.
    let mut combined = String::new();
    for (i, path) in paths.iter().enumerate() {
        let src = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Failed to read {path}: {e}");
            process::exit(2);
        });

        if i > 0 {
            combined.push('\n');
            combined.push('\n');
        }
        combined.push_str(&src);
    }

    // Strip COOL comments before lexing (your function returns Result<String, String>)
    let combined = strip_comments(&combined).unwrap_or_else(|e| {
        eprintln!("Comment stripping error: {e}");
        process::exit(1);
    });

    let toks = lex(combined.as_str()).unwrap_or_else(|e| {
        eprintln!("Lex error: {e}");
        process::exit(1);
    });

    let prog = parse_program(&toks).unwrap_or_else(|errs| {
        eprintln!("Parse errors:");
        for e in errs {
            eprintln!("  {e:?}");
        }
        process::exit(1);
    });

    println!("=== AST ===");
    println!("{:#?}", prog);

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
