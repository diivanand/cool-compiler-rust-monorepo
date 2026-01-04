// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

pub mod ast;
pub mod lexer;
pub mod parser;

pub use ast::*;
pub use lexer::{lex, strip_comments, Tok};
pub use parser::parse_program;

// Re-export the lifetime error alias
pub use parser::ParseError;
