// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub classes: Vec<Class>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    pub name: String,           // TYPE
    pub parent: Option<String>, // TYPE
    pub features: Vec<Feature>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Feature {
    Method {
        name: String, // ID
        formals: Vec<Formal>,
        ret_type: String, // TYPE
        body: Expr,
    },
    Attr {
        name: String, // ID
        ty: String,   // TYPE
        init: Option<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Formal {
    pub name: String, // ID
    pub ty: String,   // TYPE
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Assign {
        name: String,
        expr: Box<Expr>,
    },

    Dispatch {
        recv: Box<Expr>,
        static_type: Option<String>,
        method: String,
        args: Vec<Expr>,
    },

    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Box<Expr>,
    },

    While {
        cond: Box<Expr>,
        body: Box<Expr>,
    },

    Block(Vec<Expr>),

    Let {
        bindings: Vec<LetBinding>,
        body: Box<Expr>,
    },

    Case {
        expr: Box<Expr>,
        arms: Vec<CaseArm>,
    },

    New(String),

    IsVoid(Box<Expr>),

    // unary boolean negation
    Not(Box<Expr>),

    // unary arithemtic negation
    Neg(Box<Expr>), // ~expr

    // infix binary operations
    Bin {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },

    // parenthesis expression
    Paren(Box<Expr>),

    // literals
    Id(String),
    Int(i64),
    Str(String),
    Bool(bool),
    Self_,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBinding {
    pub name: String,
    pub ty: String,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
    pub name: String,
    pub ty: String,
    pub expr: Expr,
}

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
