// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! `cool_frontend` — the language-understanding half of the compiler.
//!
//! It takes COOL source text through the classic three frontend stages and
//! hands a validated AST to the backend (`cool_codegen`):
//!
//! ```text
//!   source ──[lexer]──▶ tokens ──[parser]──▶ AST ──[typechecker]──▶ checked AST
//! ```
//!
//! - [`mod@lexer`] — tokenize text (strip comments, recognize keywords/literals).
//! - [`mod@parser`] — build the [`ast`] tree (recursive descent + Pratt).
//! - [`mod@typechecker`] — enforce COOL's static semantics (manual §12).
//!
//! The most useful items are re-exported below so downstream crates can write
//! `use cool_frontend::{lex, parse_program, type_check_program};`.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod typechecker;

pub use ast::*;
pub use lexer::{Tok, lex, strip_comments};
pub use parser::{ParseError, parse_program};
pub use typechecker::{CoolType, TypeError, type_check_program};

/// Read several source files and concatenate them into one logical program.
///
/// COOL programs may be split across files (e.g. one class per file); the
/// compiler treats them as a single unit. We insert a blank line between files
/// so two files can't accidentally glue tokens together at the boundary (e.g.
/// a trailing identifier in one file and a leading one in the next).
///
/// Shared by both CLI front-ends (`coolc` and `cool_parse_cli`) so they read
/// input identically.
pub fn combine_sources(paths: &[String]) -> Result<String, String> {
    let mut combined = String::new();
    for (i, path) in paths.iter().enumerate() {
        let src =
            std::fs::read_to_string(path).map_err(|e| format!("failed to read {path}: {e}"))?;
        if i > 0 {
            combined.push_str("\n\n");
        }
        combined.push_str(&src);
    }
    Ok(combined)
}
