// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! Abstract Syntax Tree (AST) for COOL.
//!
//! The AST is the parser's output and every later stage's input: the type
//! checker walks it, and the code generator lowers it to LLVM IR. It mirrors
//! COOL's grammar (manual §10) fairly directly — one enum variant per syntactic
//! form — rather than being a desugared or "core" representation. Keeping it
//! close to the source makes type-error messages and teaching easier.
//!
//! ## Design notes for readers new to writing compilers
//!
//! - **Types are just `String`s here.** At parse time we don't yet know which
//!   class names are valid, so `Class::parent`, `Formal::ty`, etc. hold the raw
//!   identifier text. The type checker (`typechecker.rs`) is what resolves these
//!   strings against the class table and turns them into a richer `CoolType`.
//! - **Recursion goes through `Box`.** `Expr` is a recursive type (an `If`
//!   contains more `Expr`s), and a Rust enum cannot contain itself by value —
//!   that would be infinitely sized. `Box<Expr>` puts the child on the heap so
//!   each variant has a known, finite size.
//! - **`derive(PartialEq, Eq)`** lets tests compare whole subtrees with `==`,
//!   which is how the parser tests assert on tree *shape* (e.g. that
//!   `1 + 2 * 3` nests the multiply under the add).

/// A whole COOL program: an unordered collection of class definitions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub classes: Vec<Class>,
}

/// A single class. `parent` is `None` when the class has no explicit
/// `inherits` clause; the type checker treats that as inheriting from `Object`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: String,           // TYPE identifier, e.g. "Main"
    pub parent: Option<String>, // TYPE identifier from `inherits`, if any
    pub features: Vec<Feature>,
}

/// A *feature* is a member of a class: either a method or an attribute (field).
/// COOL uses one keyword-free syntax for both, distinguished by whether a
/// parameter list / body follows, so they share this enum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Feature {
    /// `name(formals) : ret_type { body }`
    Method {
        name: String, // ID
        formals: Vec<Formal>,
        ret_type: String, // TYPE
        body: Expr,
    },
    /// `name : ty [ <- init ]`
    Attr {
        name: String, // ID
        ty: String,   // TYPE
        init: Option<Expr>,
    },
}

/// A formal parameter: `name : ty` in a method signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Formal {
    pub name: String, // ID
    pub ty: String,   // TYPE
}

/// COOL expressions. *Everything* in a method body is an expression — there are
/// no statements — so this single enum captures the whole language of values,
/// including control flow (`if`, `while`, `case`) and binding forms (`let`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    /// `name <- expr`
    Assign {
        name: String,
        expr: Box<Expr>,
    },

    /// Method call. Covers all three COOL dispatch forms:
    /// - dynamic:        `recv.method(args)`   (`static_type` is `None`)
    /// - static:         `recv@Type.method(args)` (`static_type` is `Some`)
    /// - implicit self:  `method(args)` is desugared by the parser into a
    ///   dispatch with `recv = Self_`.
    Dispatch {
        recv: Box<Expr>,
        static_type: Option<String>,
        method: String,
        args: Vec<Expr>,
    },

    /// `if cond then then_ else else_ fi`
    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },

    /// `while cond loop body pool` — evaluates to `Object` (void) in COOL.
    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },

    /// `{ e1; e2; ...; en; }` — value is that of the last expression.
    Block(Vec<Expr>),

    /// `let x1 : T1 [<- i1], ... in body`
    Let {
        bindings: Vec<LetBinding>,
        body: Box<Expr>,
    },

    /// `case expr of name : Type => arm; ... esac`
    Case {
        expr: Box<Expr>,
        arms: Vec<CaseArm>,
    },

    /// `new Type` — allocates a fresh object of the named type.
    New(String),

    /// `isvoid expr` — true iff `expr` evaluates to the void value.
    IsVoid(Box<Expr>),

    /// `not expr` — boolean negation.
    Not(Box<Expr>),

    /// `~expr` — integer (arithmetic) negation.
    Neg(Box<Expr>),

    /// Infix binary operation; see [`BinOp`].
    Bin {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    /// `( expr )`. We keep parentheses as an explicit node rather than
    /// discarding them; it has no semantic effect but keeps the AST a faithful
    /// record of the source and simplifies some debugging.
    Paren(Box<Expr>),

    // --- Leaf / literal expressions ---
    Id(String),  // an object identifier reference
    Int(i64),    // integer literal
    Str(String), // string literal (escapes already decoded by the lexer)
    Bool(bool),  // `true` / `false`
    Self_,       // the `self` keyword
}

/// One binding inside a `let`: `name : ty [ <- init ]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBinding {
    pub name: String,
    pub ty: String,
    pub init: Option<Expr>,
}

/// One arm of a `case`: `name : ty => expr`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub name: String,
    pub ty: String,
    pub expr: Expr,
}

/// Infix binary operators. Unary operators (`~`, `not`, `isvoid`) are their own
/// `Expr` variants rather than living here, matching how COOL's grammar treats
/// them. Operator *precedence* is not encoded on these values — it is resolved
/// entirely by the Pratt parser (see `parser.rs`), so by the time we have a
/// `BinOp` the tree shape already reflects the correct grouping.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Le,
    Eq,
}
