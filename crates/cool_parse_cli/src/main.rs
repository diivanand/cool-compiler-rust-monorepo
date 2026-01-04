// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use std::{env, fs, process};

use cool_frontend::{lex, parse_program};

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

    // Read and concatenate all files into one source string.
    // The separators help avoid accidental token gluing between files.
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

    let toks = lex(&combined).unwrap_or_else(|e| {
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

    println!("{:#?}", prog);
}
