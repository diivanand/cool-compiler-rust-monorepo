// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\r\n\f\v]+")]
pub enum Tok {
    // Keywords (case-insensitive except true/false first letter must be lowercase)
    #[regex(r"(?i:class)")]
    KwClass,
    #[regex(r"(?i:inherits)")]
    KwInherits,
    #[regex(r"(?i:if)")]
    KwIf,
    #[regex(r"(?i:then)")]
    KwThen,
    #[regex(r"(?i:else)")]
    KwElse,
    #[regex(r"(?i:fi)")]
    KwFi,
    #[regex(r"(?i:while)")]
    KwWhile,
    #[regex(r"(?i:loop)")]
    KwLoop,
    #[regex(r"(?i:pool)")]
    KwPool,
    #[regex(r"(?i:let)")]
    KwLet,
    #[regex(r"(?i:in)")]
    KwIn,
    #[regex(r"(?i:case)")]
    KwCase,
    #[regex(r"(?i:of)")]
    KwOf,
    #[regex(r"(?i:esac)")]
    KwEsac,
    #[regex(r"(?i:new)")]
    KwNew,
    #[regex(r"(?i:isvoid)")]
    KwIsVoid,
    #[regex(r"(?i:not)")]
    KwNot,

    // true/false special casing rule (first char lowercase)
    #[regex(r"t[rR][uU][eE]")]
    KwTrue,
    #[regex(r"f[aA][lL][sS][eE]")]
    KwFalse,

    // Special identifiers
    #[token("self")]
    SelfId,
    #[token("SELF_TYPE")]
    SelfType,

    // Symbols / operators
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("@")]
    At,

    #[token("<-")]
    Assign,
    #[token("=>")]
    Darrow,
    #[token("<=")]
    Le,
    #[token("<")]
    Lt,
    #[token("=")]
    Eq,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,

    #[token("~")]
    Tilde,

    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap())]
    Int(i64),

    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    Str(String),

    // Identifiers
    #[regex(r"[A-Z][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    TypeId(String),

    #[regex(r"[a-z][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    ObjId(String),
}

fn parse_string(lex: &mut logos::Lexer<Tok>) -> String {
    let raw = lex.slice();
    let inner = &raw[1..raw.len() - 1];
    let mut out = String::new();
    let mut chars = inner.chars();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('b') => out.push('\u{0008}'),
                Some('t') => out.push('\t'),
                Some('n') => out.push('\n'),
                Some('f') => out.push('\u{000C}'),
                Some('\\') => out.push('\\'),
                Some('"') => out.push('"'),
                Some(other) => out.push(other),
                None => break,
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Strip COOL comments:
///  - line comments: -- ... \n
///  - block comments: (* ... *) nested
pub fn strip_comments(input: &str) -> Result<String, String> {
    let bytes = input.as_bytes();
    let mut i = 0usize;
    let mut out = String::with_capacity(input.len());
    let mut depth = 0usize;

    while i < bytes.len() {
        if depth > 0 {
            if i + 1 < bytes.len() && bytes[i] == b'(' && bytes[i + 1] == b'*' {
                depth += 1;
                i += 2;
                continue;
            }
            if i + 1 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b')' {
                depth -= 1;
                i += 2;
                continue;
            }
            i += 1;
            continue;
        }

        // line comment "--"
        if i + 1 < bytes.len() && bytes[i] == b'-' && bytes[i + 1] == b'-' {
            i += 2;
            while i < bytes.len() && bytes[i] != b'\n' {
                i += 1;
            }
            continue;
        }

        // block comment "(*"
        if i + 1 < bytes.len() && bytes[i] == b'(' && bytes[i + 1] == b'*' {
            depth = 1;
            i += 2;
            continue;
        }

        out.push(bytes[i] as char);
        i += 1;
    }

    if depth != 0 {
        return Err("unterminated block comment".to_string());
    }
    Ok(out)
}

/// Lex COOL input into tokens.
/// Logos returns `Err(())` for invalid/unmatched input. We surface a string error with context.
pub fn lex(input: &str) -> Result<Vec<Tok>, String> {
    let cleaned = strip_comments(input)?;
    let mut toks = Vec::new();
    let mut lx = Tok::lexer(&cleaned);

    while let Some(res) = lx.next() {
        match res {
            Ok(tok) => toks.push(tok),
            Err(_) => return Err(format!("lexer error near: {:?}", lx.slice())),
        }
    }

    Ok(toks)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_line_comments() {
        let s = "class Main { -- hi\n x:Int; };";
        let cleaned = strip_comments(s).unwrap();
        assert!(cleaned.contains("class Main"));
        assert!(!cleaned.contains("-- hi"));
    }

    #[test]
    fn strips_nested_block_comments() {
        let s = "(* a (* b *) c *) class Main { };";
        let cleaned = strip_comments(s).unwrap();
        assert!(cleaned.contains("class Main"));
    }

    #[test]
    fn lex_keywords_case_insensitive_but_true_false_special() {
        let toks = lex("ClAsS Main { x:Bool <- tRuE; };").unwrap();
        assert!(toks.contains(&Tok::KwClass));
        assert!(toks.contains(&Tok::KwTrue));

        // "True" should NOT be KwTrue
        let toks2 = lex("class Main { x:Bool <- True; };").unwrap();
        assert!(!toks2.contains(&Tok::KwTrue));
    }

    #[test]
    fn lex_basic_class_tokens() {
        let toks = lex("class Main inherits Object { x : Int <- 1; };").unwrap();
        assert!(toks.contains(&Tok::KwClass));
        assert!(toks.contains(&Tok::KwInherits));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Main")));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "x")));
        assert!(toks.iter().any(|t| matches!(t, Tok::Int(1))));
    }
}
