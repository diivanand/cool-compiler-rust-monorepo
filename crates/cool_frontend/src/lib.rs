// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod typechecker;

pub use ast::*;
pub use lexer::{Tok, lex, strip_comments};
pub use parser::{ParseError, parse_program};

// Re-export current typechecker API (matches the file we fixed)
pub use typechecker::{CoolType, TypeError, type_check_program};
