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

    #[test]
    fn lex_all_operators_and_symbols() {
        let toks = lex("{ } ( ) : ; , . @ <- => <= < = + - * / ~").unwrap();
        assert!(toks.contains(&Tok::LBrace));
        assert!(toks.contains(&Tok::RBrace));
        assert!(toks.contains(&Tok::LParen));
        assert!(toks.contains(&Tok::RParen));
        assert!(toks.contains(&Tok::Colon));
        assert!(toks.contains(&Tok::Semi));
        assert!(toks.contains(&Tok::Comma));
        assert!(toks.contains(&Tok::Dot));
        assert!(toks.contains(&Tok::At));
        assert!(toks.contains(&Tok::Assign));
        assert!(toks.contains(&Tok::Darrow));
        assert!(toks.contains(&Tok::Le));
        assert!(toks.contains(&Tok::Lt));
        assert!(toks.contains(&Tok::Eq));
        assert!(toks.contains(&Tok::Plus));
        assert!(toks.contains(&Tok::Minus));
        assert!(toks.contains(&Tok::Star));
        assert!(toks.contains(&Tok::Slash));
        assert!(toks.contains(&Tok::Tilde));
    }

    #[test]
    fn lex_string_with_escape_sequences() {
        let toks = lex(r#""hello\n\t\b\f\\\"world""#).unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::Str(s) if s == "hello\n\t\u{0008}\u{000C}\\\"world")));
    }

    #[test]
    fn lex_string_basic() {
        let toks = lex(r#""simple string""#).unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::Str(s) if s == "simple string")));
    }

    #[test]
    fn lex_integer_literals() {
        let toks = lex("0 42 123456789").unwrap();
        assert!(toks.contains(&Tok::Int(0)));
        assert!(toks.contains(&Tok::Int(42)));
        assert!(toks.contains(&Tok::Int(123456789)));
    }

    #[test]
    fn lex_type_id_vs_obj_id() {
        let toks = lex("MyClass myObject AnotherType another_var").unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "MyClass")));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "myObject")));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "AnotherType")));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "another_var")));
    }

    #[test]
    fn lex_special_identifiers() {
        let toks = lex("self SELF_TYPE").unwrap();
        assert!(toks.contains(&Tok::SelfId));
        assert!(toks.contains(&Tok::SelfType));
    }

    #[test]
    fn lex_all_keywords_various_cases() {
        let src = "ClAsS iNhErItS IF tHeN eLsE fI wHiLe LoOp PoOl LeT iN CaSe Of EsAc NeW iSvOiD nOt";
        let toks = lex(src).unwrap();
        assert!(toks.contains(&Tok::KwClass));
        assert!(toks.contains(&Tok::KwInherits));
        assert!(toks.contains(&Tok::KwIf));
        assert!(toks.contains(&Tok::KwThen));
        assert!(toks.contains(&Tok::KwElse));
        assert!(toks.contains(&Tok::KwFi));
        assert!(toks.contains(&Tok::KwWhile));
        assert!(toks.contains(&Tok::KwLoop));
        assert!(toks.contains(&Tok::KwPool));
        assert!(toks.contains(&Tok::KwLet));
        assert!(toks.contains(&Tok::KwIn));
        assert!(toks.contains(&Tok::KwCase));
        assert!(toks.contains(&Tok::KwOf));
        assert!(toks.contains(&Tok::KwEsac));
        assert!(toks.contains(&Tok::KwNew));
        assert!(toks.contains(&Tok::KwIsVoid));
        assert!(toks.contains(&Tok::KwNot));
    }

    #[test]
    fn lex_false_variations() {
        assert!(lex("false").unwrap().contains(&Tok::KwFalse));
        assert!(lex("fAlSe").unwrap().contains(&Tok::KwFalse));
        assert!(lex("faLsE").unwrap().contains(&Tok::KwFalse));

        // False (capital F) should NOT be KwFalse, should be TypeId
        let toks = lex("False").unwrap();
        assert!(!toks.contains(&Tok::KwFalse));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "False")));
    }

    #[test]
    fn lex_true_variations() {
        assert!(lex("true").unwrap().contains(&Tok::KwTrue));
        assert!(lex("tRuE").unwrap().contains(&Tok::KwTrue));
        assert!(lex("trUe").unwrap().contains(&Tok::KwTrue));

        // True (capital T) should NOT be KwTrue, should be TypeId
        let toks = lex("True").unwrap();
        assert!(!toks.contains(&Tok::KwTrue));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "True")));
    }

    #[test]
    fn strip_multiple_line_comments() {
        let s = "class -- comment 1\n Main -- comment 2\n { };";
        let cleaned = strip_comments(s).unwrap();
        assert!(cleaned.contains("class"));
        assert!(cleaned.contains("Main"));
        assert!(!cleaned.contains("comment 1"));
        assert!(!cleaned.contains("comment 2"));
    }

    #[test]
    fn strip_comment_at_end_of_file() {
        let s = "class Main { }; -- final comment";
        let cleaned = strip_comments(s).unwrap();
        assert!(cleaned.contains("class Main"));
        assert!(!cleaned.contains("final comment"));
    }

    #[test]
    fn strip_deeply_nested_block_comments() {
        let s = "(* level1 (* level2 (* level3 *) back2 *) back1 *) code";
        let cleaned = strip_comments(s).unwrap();
        assert!(cleaned.contains("code"));
        assert!(!cleaned.contains("level"));
    }

    #[test]
    fn error_unterminated_block_comment() {
        let s = "(* this comment never closes class Main { };";
        let result = strip_comments(s);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("unterminated"));
    }

    #[test]
    fn error_unmatched_close_comment() {
        let s = "class Main *) { };";
        let cleaned = strip_comments(s).unwrap();
        // This should actually be fine - the *) is just treated as tokens
        assert!(cleaned.contains("*)"));
    }

    #[test]
    fn lex_assignment_vs_arrow() {
        let toks = lex("x <- 5 case y of a : Int => a; esac").unwrap();
        assert!(toks.contains(&Tok::Assign));
        assert!(toks.contains(&Tok::Darrow));
    }

    #[test]
    fn lex_less_than_vs_less_equal() {
        let toks = lex("x < 5 y <= 10").unwrap();
        assert!(toks.contains(&Tok::Lt));
        assert!(toks.contains(&Tok::Le));
        // Make sure we have both, not confused
        assert_eq!(toks.iter().filter(|t| **t == Tok::Lt).count(), 1);
        assert_eq!(toks.iter().filter(|t| **t == Tok::Le).count(), 1);
    }

    #[test]
    fn lex_identifiers_with_underscores_and_numbers() {
        let toks = lex("my_var_123 MyClass_456 x1 Y2").unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "my_var_123")));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "MyClass_456")));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "x1")));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Y2")));
    }

    #[test]
    fn lex_whitespace_handling() {
        let toks = lex("  class  \t\n Main\r\n  {  }  ;  ").unwrap();
        assert!(toks.contains(&Tok::KwClass));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Main")));
        assert!(toks.contains(&Tok::LBrace));
        assert!(toks.contains(&Tok::RBrace));
        assert!(toks.contains(&Tok::Semi));
    }

    #[test]
    fn lex_complex_expression() {
        let toks = lex("x <- 1 + 2 * 3 - 4 / 5").unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "x")));
        assert!(toks.contains(&Tok::Assign));
        assert!(toks.contains(&Tok::Int(1)));
        assert!(toks.contains(&Tok::Plus));
        assert!(toks.contains(&Tok::Int(2)));
        assert!(toks.contains(&Tok::Star));
        assert!(toks.contains(&Tok::Int(3)));
        assert!(toks.contains(&Tok::Minus));
        assert!(toks.contains(&Tok::Int(4)));
        assert!(toks.contains(&Tok::Slash));
        assert!(toks.contains(&Tok::Int(5)));
    }

    #[test]
    fn lex_dispatch_expression() {
        let toks = lex("obj.method(arg1, arg2)").unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "obj")));
        assert!(toks.contains(&Tok::Dot));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "method")));
        assert!(toks.contains(&Tok::LParen));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "arg1")));
        assert!(toks.contains(&Tok::Comma));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "arg2")));
        assert!(toks.contains(&Tok::RParen));
    }

    #[test]
    fn lex_static_dispatch_expression() {
        let toks = lex("obj@Type.method()").unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "obj")));
        assert!(toks.contains(&Tok::At));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Type")));
        assert!(toks.contains(&Tok::Dot));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "method")));
    }

    #[test]
    fn lex_empty_string() {
        let toks = lex(r#""""#).unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::Str(s) if s.is_empty())));
    }

    #[test]
    fn lex_string_with_spaces() {
        let toks = lex(r#""hello world foo bar""#).unwrap();
        assert!(toks.iter().any(|t| matches!(t, Tok::Str(s) if s == "hello world foo bar")));
    }

    #[test]
    fn lex_mixed_comments_and_code() {
        let src = r#"
            class Main { -- line comment
                (* block comment *)
                x : Int; -- another line comment
                (* nested (* comment *) here *)
            };
        "#;
        let toks = lex(src).unwrap();
        assert!(toks.contains(&Tok::KwClass));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Main")));
        assert!(toks.iter().any(|t| matches!(t, Tok::ObjId(s) if s == "x")));
        assert!(toks.iter().any(|t| matches!(t, Tok::TypeId(s) if s == "Int")));
    }
}
