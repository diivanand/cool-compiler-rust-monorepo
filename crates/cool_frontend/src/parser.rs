// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use chumsky::prelude::*;
use chumsky::{extra, pratt};

use crate::ast::*;
use crate::lexer::Tok;

/// chumsky 0.12 errors are lifetime-parameterized
pub type ParseError<'src> = chumsky::error::Simple<'src, Tok>;
pub type PExtra<'src> = extra::Err<ParseError<'src>>;

/// Public API: parse a token slice into a Program.
pub fn parse_program<'src>(tokens: &'src [Tok]) -> Result<Program, Vec<ParseError<'src>>> {
    program_parser().parse(tokens).into_result()
}

pub fn program_parser<'src>() -> impl Parser<'src, &'src [Tok], Program, PExtra<'src>> {
    class_parser()
        .then_ignore(just(Tok::Semi))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|classes| Program { classes })
        .then_ignore(end())
}

fn class_parser<'src>() -> impl Parser<'src, &'src [Tok], Class, PExtra<'src>> {
    just(Tok::KwClass)
        .ignore_then(type_id())
        .then(just(Tok::KwInherits).ignore_then(type_id()).or_not())
        .then(
            just(Tok::LBrace)
                .ignore_then(
                    feature_parser()
                        .then_ignore(just(Tok::Semi))
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Tok::RBrace)),
        )
        .map(|((name, parent), features)| Class { name, parent, features })
}

fn feature_parser<'src>() -> impl Parser<'src, &'src [Tok], Feature, PExtra<'src>> {
    let method = obj_id()
        .then(
            just(Tok::LParen)
                .ignore_then(
                    formal_parser()
                        .separated_by(just(Tok::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|opt| opt.unwrap_or_default()),
                )
                .then_ignore(just(Tok::RParen)),
        )
        .then_ignore(just(Tok::Colon))
        .then(type_id())
        .then(
            just(Tok::LBrace)
                .ignore_then(expr_parser())
                .then_ignore(just(Tok::RBrace)),
        )
        .map(|(((name, formals), ret_type), body)| Feature::Method {
            name,
            formals,
            ret_type,
            body,
        });

    let attr = obj_id()
        .then_ignore(just(Tok::Colon))
        .then(type_id())
        .then(just(Tok::Assign).ignore_then(expr_parser()).or_not())
        .map(|((name, ty), init)| Feature::Attr { name, ty, init });

    method.or(attr)
}

fn formal_parser<'src>() -> impl Parser<'src, &'src [Tok], Formal, PExtra<'src>> {
    obj_id()
        .then_ignore(just(Tok::Colon))
        .then(type_id())
        .map(|(name, ty)| Formal { name, ty })
}

fn type_id<'src>() -> impl Parser<'src, &'src [Tok], String, PExtra<'src>> {
    select! { Tok::TypeId(s) => s }
        .or(just(Tok::SelfType).to("SELF_TYPE".to_string()))
}

fn obj_id<'src>() -> impl Parser<'src, &'src [Tok], String, PExtra<'src>> {
    select! { Tok::ObjId(s) => s }
}

fn literal<'src>() -> impl Parser<'src, &'src [Tok], Expr, PExtra<'src>> {
    select! {
        Tok::Int(n) => Expr::Int(n),
        Tok::Str(s) => Expr::Str(s),
        Tok::KwTrue => Expr::Bool(true),
        Tok::KwFalse => Expr::Bool(false),
    }
}

pub fn expr_parser<'src>() -> impl Parser<'src, &'src [Tok], Expr, PExtra<'src>> {
    recursive(|expr| {
        let paren = just(Tok::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(Tok::RParen))
            .map(|e| Expr::Paren(Box::new(e)));

        let self_id = just(Tok::SelfId).to(Expr::Self_);
        let id = obj_id().map(Expr::Id);

        let block = just(Tok::LBrace)
            .ignore_then(
                expr.clone()
                    .then_ignore(just(Tok::Semi))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Tok::RBrace))
            .map(Expr::Block);

        let if_ = just(Tok::KwIf)
            .ignore_then(expr.clone())
            .then_ignore(just(Tok::KwThen))
            .then(expr.clone())
            .then_ignore(just(Tok::KwElse))
            .then(expr.clone())
            .then_ignore(just(Tok::KwFi))
            .map(|((cond, then_), else_)| Expr::If {
                cond: Box::new(cond),
                then_: Box::new(then_),
                else_: Box::new(else_),
            });

        let while_ = just(Tok::KwWhile)
            .ignore_then(expr.clone())
            .then_ignore(just(Tok::KwLoop))
            .then(expr.clone())
            .then_ignore(just(Tok::KwPool))
            .map(|(cond, body)| Expr::While {
                cond: Box::new(cond),
                body: Box::new(body),
            });

        let let_binding = obj_id()
            .then_ignore(just(Tok::Colon))
            .then(type_id())
            .then(just(Tok::Assign).ignore_then(expr.clone()).or_not())
            .map(|((name, ty), init)| LetBinding { name, ty, init });

        let let_ = just(Tok::KwLet)
            .ignore_then(
                let_binding
                    .separated_by(just(Tok::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Tok::KwIn))
            .then(expr.clone())
            .map(|(bindings, body)| Expr::Let {
                bindings,
                body: Box::new(body),
            });

        let case_arm = obj_id()
            .then_ignore(just(Tok::Colon))
            .then(type_id())
            .then_ignore(just(Tok::Darrow))
            .then(expr.clone())
            .then_ignore(just(Tok::Semi))
            .map(|((name, ty), expr)| CaseArm { name, ty, expr });

        let case_ = just(Tok::KwCase)
            .ignore_then(expr.clone())
            .then_ignore(just(Tok::KwOf))
            .then(case_arm.repeated().at_least(1).collect::<Vec<_>>())
            .then_ignore(just(Tok::KwEsac))
            .map(|(e, arms)| Expr::Case {
                expr: Box::new(e),
                arms,
            });

        let new_ = just(Tok::KwNew).ignore_then(type_id()).map(Expr::New);

        let atom = if_
            .or(while_)
            .or(let_)
            .or(case_)
            .or(block)
            .or(new_)
            .or(paren)
            .or(literal())
            .or(self_id)
            .or(id);

        // args: ( [expr (, expr)*]? )
        let args = just(Tok::LParen)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Tok::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|opt| opt.unwrap_or_default()),
            )
            .then_ignore(just(Tok::RParen));

        // Optional self-dispatch: id(args) => self.id(args)
        let primary = atom
            .then(args.clone().or_not())
            .map(|(a, maybe_args)| match (a, maybe_args) {
                (Expr::Id(name), Some(args)) => Expr::Dispatch {
                    recv: Box::new(Expr::Self_),
                    static_type: None,
                    method: name,
                    args,
                },
                (other, _) => other,
            });

        // recv [@TYPE] . id(args)
        let dispatch_step = just(Tok::At)
            .ignore_then(type_id())
            .or_not()
            .then_ignore(just(Tok::Dot))
            .then(obj_id())
            .then(args.clone())
            .map(|((static_ty, method), args)| (static_ty, method, args));

        let postfix = primary
            .then(dispatch_step.repeated().collect::<Vec<_>>())
            .map(|(base, steps)| {
                steps.into_iter().fold(base, |recv, (static_type, method, args)| Expr::Dispatch {
                    recv: Box::new(recv),
                    static_type,
                    method,
                    args,
                })
            });

        // Pratt fold signatures (from docs) :contentReference[oaicite:2]{index=2}
        // prefix: |op, rhs, extra|
        // infix:  |lhs, op, rhs, extra|
        let pratt_expr = postfix.pratt((
            pratt::prefix(1, just(Tok::Tilde), |_, rhs, _| Expr::Neg(Box::new(rhs))),
            pratt::prefix(1, just(Tok::KwIsVoid), |_, rhs, _| Expr::IsVoid(Box::new(rhs))),
            pratt::prefix(1, just(Tok::KwNot), |_, rhs, _| Expr::Not(Box::new(rhs))),

            pratt::infix(pratt::left(2), just(Tok::Star), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Mul,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            pratt::infix(pratt::left(2), just(Tok::Slash), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Div,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),

            pratt::infix(pratt::left(3), just(Tok::Plus), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Add,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            pratt::infix(pratt::left(3), just(Tok::Minus), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),

            pratt::infix(pratt::left(4), just(Tok::Le), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Le,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            pratt::infix(pratt::left(4), just(Tok::Lt), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Lt,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            pratt::infix(pratt::left(4), just(Tok::Eq), |lhs, _, rhs, _| Expr::Bin {
                op: BinOp::Eq,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),

            pratt::infix(pratt::right(5), just(Tok::Assign), |lhs, _, rhs, _| match lhs {
                Expr::Id(name) => Expr::Assign {
                    name,
                    expr: Box::new(rhs),
                },
                other => Expr::Assign {
                    name: "<non-id-lhs>".to_string(),
                    expr: Box::new(Expr::Paren(Box::new(other))),
                },
            }),
        ));

        // Make recursive closure output Clone by boxing :contentReference[oaicite:3]{index=3}
        pratt_expr.boxed()
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn parses_minimal_main_class() {
        let src = r#"
            class Main {
              main() : Int { 1 + 2 * 3 };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
        assert_eq!(prog.classes[0].name, "Main");
        assert_eq!(prog.classes[0].features.len(), 1);
    }

    #[test]
    fn parses_block_let_case_dispatch() {
        let src = r#"
            class Main inherits Object {
              main() : Int {
                {
                  x <- 0;
                  let y : Int <- 5, z : Int in self.out_int(x);
                  case x of a : Int => a + 1; esac;
                  x;
                }
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].parent.as_deref(), Some("Object"));
    }

    #[test]
    fn parses_attribute_without_init() {
        let src = r#"
            class Point {
              x : Int;
              y : Int;
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].features.len(), 2);

        match &prog.classes[0].features[0] {
            Feature::Attr { name, ty, init } => {
                assert_eq!(name, "x");
                assert_eq!(ty, "Int");
                assert!(init.is_none());
            }
            _ => panic!("Expected attribute"),
        }
    }

    #[test]
    fn parses_attribute_with_init() {
        let src = r#"
            class Counter {
              count : Int <- 0;
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Attr { name, ty, init } => {
                assert_eq!(name, "count");
                assert_eq!(ty, "Int");
                assert!(init.is_some());
            }
            _ => panic!("Expected attribute"),
        }
    }

    #[test]
    fn parses_method_with_no_params() {
        let src = r#"
            class Test {
              getValue() : Int { 42 };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { name, formals, ret_type, .. } => {
                assert_eq!(name, "getValue");
                assert_eq!(formals.len(), 0);
                assert_eq!(ret_type, "Int");
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_method_with_multiple_params() {
        let src = r#"
            class Math {
              add(x : Int, y : Int, z : Int) : Int { x + y + z };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { name, formals, .. } => {
                assert_eq!(name, "add");
                assert_eq!(formals.len(), 3);
                assert_eq!(formals[0].name, "x");
                assert_eq!(formals[1].name, "y");
                assert_eq!(formals[2].name, "z");
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_if_then_else_expression() {
        let src = r#"
            class Test {
              test(x : Int) : Int {
                if x < 10 then 1 else 0 fi
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::If { .. } => (),
                    _ => panic!("Expected if expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_while_loop() {
        let src = r#"
            class Test {
              loop_test(x : Int) : Int {
                while x < 10 loop x <- x + 1 pool
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::While { .. } => (),
                    _ => panic!("Expected while expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_new_expression() {
        let src = r#"
            class Test {
              create() : Object { new Object };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::New(ty) => assert_eq!(ty, "Object"),
                    _ => panic!("Expected new expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_new_self_type() {
        let src = r#"
            class Test {
              clone() : SELF_TYPE { new SELF_TYPE };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_case_expression_multiple_arms() {
        let src = r#"
            class Test {
              test(x : Object) : Int {
                case x of
                  a : Int => a + 1;
                  b : String => 0;
                  c : Object => 42;
                esac
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Case { arms, .. } => {
                        assert_eq!(arms.len(), 3);
                        assert_eq!(arms[0].name, "a");
                        assert_eq!(arms[0].ty, "Int");
                        assert_eq!(arms[1].name, "b");
                        assert_eq!(arms[1].ty, "String");
                    }
                    _ => panic!("Expected case expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_let_multiple_bindings() {
        let src = r#"
            class Test {
              test() : Int {
                let x : Int <- 1, y : Int <- 2, z : Int in x + y + z
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Let { bindings, .. } => {
                        assert_eq!(bindings.len(), 3);
                        assert_eq!(bindings[0].name, "x");
                        assert_eq!(bindings[1].name, "y");
                        assert_eq!(bindings[2].name, "z");
                    }
                    _ => panic!("Expected let expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_let_without_init() {
        let src = r#"
            class Test {
              test() : Int {
                let x : Int in x
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Let { bindings, .. } => {
                        assert_eq!(bindings.len(), 1);
                        assert!(bindings[0].init.is_none());
                    }
                    _ => panic!("Expected let expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_static_dispatch() {
        let src = r#"
            class Test {
              test(x : Object) : Int {
                x@Object.type_name()
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Dispatch { static_type, method, .. } => {
                        assert_eq!(static_type.as_deref(), Some("Object"));
                        assert_eq!(method, "type_name");
                    }
                    _ => panic!("Expected dispatch expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_chained_dispatch() {
        let src = r#"
            class Test {
              test() : String {
                obj.method1().method2().method3()
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Dispatch { method, .. } => {
                        assert_eq!(method, "method3");
                    }
                    _ => panic!("Expected dispatch expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_self_dispatch() {
        let src = r#"
            class Test {
              helper() : Int { 1 };
              test() : Int {
                helper()
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[1] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Dispatch { recv, method, .. } => {
                        assert!(matches!(**recv, Expr::Self_));
                        assert_eq!(method, "helper");
                    }
                    _ => panic!("Expected dispatch expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_binary_operators() {
        let src = r#"
            class Test {
              test() : Int {
                1 + 2 * 3 - 4 / 5
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_comparison_operators() {
        let src = r#"
            class Test {
              test(x : Int) : Bool {
                x < 10
              };
              test2(x : Int) : Bool {
                x <= 10
              };
              test3(x : Int) : Bool {
                x = 10
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].features.len(), 3);
    }

    #[test]
    fn parses_unary_operators() {
        let src = r#"
            class Test {
              neg(x : Int) : Int { ~x };
              check(x : Object) : Bool { isvoid x };
              negate(x : Bool) : Bool { not x };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                assert!(matches!(body, Expr::Neg(_)));
            }
            _ => panic!("Expected method"),
        }

        match &prog.classes[0].features[1] {
            Feature::Method { body, .. } => {
                assert!(matches!(body, Expr::IsVoid(_)));
            }
            _ => panic!("Expected method"),
        }

        match &prog.classes[0].features[2] {
            Feature::Method { body, .. } => {
                assert!(matches!(body, Expr::Not(_)));
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_assignment_expression() {
        let src = r#"
            class Test {
              test() : Int {
                {
                  x <- 5;
                  x;
                }
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_parenthesized_expression() {
        let src = r#"
            class Test {
              test() : Int {
                (1 + 2) * 3
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_multiple_classes() {
        let src = r#"
            class Point {
              x : Int;
              y : Int;
            };
            class Circle {
              center : Point;
              radius : Int;
            };
            class Main {
              main() : Int { 0 };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 3);
        assert_eq!(prog.classes[0].name, "Point");
        assert_eq!(prog.classes[1].name, "Circle");
        assert_eq!(prog.classes[2].name, "Main");
    }

    #[test]
    fn parses_class_with_inheritance() {
        let src = r#"
            class Base {
              x : Int;
            };
            class Derived inherits Base {
              y : Int;
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].parent, None);
        assert_eq!(prog.classes[1].parent.as_deref(), Some("Base"));
    }

    #[test]
    fn parses_nested_blocks() {
        let src = r#"
            class Test {
              test() : Int {
                {
                  {
                    1;
                    2;
                  };
                  3;
                }
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_nested_if_expressions() {
        let src = r#"
            class Test {
              test(x : Int, y : Int) : Int {
                if x < 0 then
                  if y < 0 then 1 else 2 fi
                else
                  if y < 0 then 3 else 4 fi
                fi
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_dispatch_with_arguments() {
        let src = r#"
            class Test {
              test() : Int {
                obj.method(1, 2, 3)
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Dispatch { args, .. } => {
                        assert_eq!(args.len(), 3);
                    }
                    _ => panic!("Expected dispatch expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_self_expression() {
        let src = r#"
            class Test {
              test() : SELF_TYPE { self };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                assert!(matches!(body, Expr::Self_));
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_literal_expressions() {
        let src = r#"
            class Test {
              int_lit() : Int { 42 };
              str_lit() : String { "hello" };
              bool_true() : Bool { true };
              bool_false() : Bool { false };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].features.len(), 4);
    }

    #[test]
    fn parses_complex_nested_expression() {
        let src = r#"
            class Test {
              complex() : Int {
                let x : Int <- 5 in
                  if x < 10 then
                    {
                      x <- x + 1;
                      while x < 20 loop x <- x * 2 pool;
                      case x of
                        a : Int => a + 1;
                      esac;
                    }
                  else
                    0
                  fi
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes.len(), 1);
    }

    #[test]
    fn parses_method_with_trailing_comma_in_params() {
        let src = r#"
            class Test {
              test(x : Int, y : Int,) : Int { x + y };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { formals, .. } => {
                assert_eq!(formals.len(), 2);
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_dispatch_with_trailing_comma_in_args() {
        let src = r#"
            class Test {
              test() : Int {
                obj.method(1, 2,)
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();

        match &prog.classes[0].features[0] {
            Feature::Method { body, .. } => {
                match body {
                    Expr::Dispatch { args, .. } => {
                        assert_eq!(args.len(), 2);
                    }
                    _ => panic!("Expected dispatch expression"),
                }
            }
            _ => panic!("Expected method"),
        }
    }

    #[test]
    fn parses_empty_class() {
        let src = r#"
            class Empty { };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        assert_eq!(prog.classes[0].features.len(), 0);
    }
}
