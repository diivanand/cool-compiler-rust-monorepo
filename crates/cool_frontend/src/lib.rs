// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod typechecker;

pub use ast::*;
pub use lexer::{lex, strip_comments, Tok};
pub use parser::{parse_program, ParseError};

// Re-export current typechecker API (matches the file we fixed)
pub use typechecker::{type_check_program, CoolType, TypeError};
