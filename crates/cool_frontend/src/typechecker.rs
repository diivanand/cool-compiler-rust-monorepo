use std::collections::{BTreeSet, HashMap};

use crate::ast::*;

/// COOL has special type SELF_TYPE which depends on the current class.
/// We'll represent types as either a concrete class name or SELF_TYPE.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CoolType {
    Named(String),
    SelfType,
}

impl CoolType {
    pub fn named<S: Into<String>>(s: S) -> Self {
        CoolType::Named(s.into())
    }

    pub fn as_named(&self) -> Option<&str> {
        match self {
            CoolType::Named(s) => Some(s.as_str()),
            CoolType::SelfType => None,
        }
    }
}

/// A type error with a human-readable message.
#[derive(Clone, Debug)]
pub struct TypeError {
    pub msg: String,
}

impl TypeError {
    pub fn new<S: Into<String>>(msg: S) -> Self {
        Self { msg: msg.into() }
    }
}

/// Built-in class names in COOL.
const OBJECT: &str = "Object";
const IO: &str = "IO";
const INT: &str = "Int";
const BOOL: &str = "Bool";
const STRING: &str = "String";

#[derive(Clone, Debug, PartialEq, Eq)]
struct MethodSig {
    formals: Vec<CoolType>,
    ret: CoolType,
}

#[derive(Clone, Debug)]
struct ClassInfo {
    parent: String,
    attrs: HashMap<String, CoolType>,
    methods: HashMap<String, MethodSig>,
}

/// Global type context with class table + inheritance.
#[derive(Clone, Debug)]
pub struct TypeContext {
    classes: HashMap<String, ClassInfo>,
}

impl TypeContext {
    pub fn new() -> Self {
        let mut ctx = TypeContext {
            classes: HashMap::new(),
        };
        ctx.install_builtins();
        ctx
    }

    fn install_builtins(&mut self) {
        self.classes.insert(
            OBJECT.to_string(),
            ClassInfo {
                parent: OBJECT.to_string(),
                attrs: HashMap::new(),
                methods: HashMap::new(),
            },
        );

        self.classes.insert(
            IO.to_string(),
            ClassInfo {
                parent: OBJECT.to_string(),
                attrs: HashMap::new(),
                methods: HashMap::new(),
            },
        );

        for b in [INT, BOOL, STRING] {
            self.classes.insert(
                b.to_string(),
                ClassInfo {
                    parent: OBJECT.to_string(),
                    attrs: HashMap::new(),
                    methods: HashMap::new(),
                },
            );
        }
    }

    fn has_class(&self, name: &str) -> bool {
        self.classes.contains_key(name)
    }

    fn resolve_self_type(&self, ty: &CoolType, current_class: &str) -> CoolType {
        match ty {
            CoolType::SelfType => CoolType::Named(current_class.to_string()),
            CoolType::Named(_) => ty.clone(),
        }
    }

    /// Conformance (≤): A ≤ B if A is a descendant of B.
    fn conforms(&self, a: &CoolType, b: &CoolType, current_class: &str) -> bool {
        let a = self.resolve_self_type(a, current_class);
        let b = self.resolve_self_type(b, current_class);

        let Some(a_name) = a.as_named() else { return false };
        let Some(b_name) = b.as_named() else { return false };

        if a_name == b_name {
            return true;
        }

        // Walk upward using owned Strings (prevents lifetime issues).
        let mut cur = a_name.to_string();
        loop {
            let info = match self.classes.get(&cur) {
                Some(i) => i,
                None => return false,
            };
            if info.parent == cur {
                return false;
            }
            if info.parent == b_name {
                return true;
            }
            cur = info.parent.clone();
        }
    }

    /// LUB / join: closest common ancestor.
    fn lub(&self, a: &CoolType, b: &CoolType, current_class: &str) -> CoolType {
        let a = self.resolve_self_type(a, current_class);
        let b = self.resolve_self_type(b, current_class);

        let Some(a0) = a.as_named() else { return CoolType::named(OBJECT) };
        let Some(b0) = b.as_named() else { return CoolType::named(OBJECT) };

        if a0 == b0 {
            return CoolType::named(a0);
        }

        let mut ancestors = BTreeSet::<String>::new();
        let mut cur = a0.to_string();
        loop {
            ancestors.insert(cur.clone());
            let info = match self.classes.get(&cur) {
                Some(i) => i,
                None => break,
            };
            if info.parent == cur {
                break;
            }
            cur = info.parent.clone();
        }

        let mut cur = b0.to_string();
        loop {
            if ancestors.contains(&cur) {
                return CoolType::named(cur);
            }
            let info = match self.classes.get(&cur) {
                Some(i) => i,
                None => break,
            };
            if info.parent == cur {
                break;
            }
            cur = info.parent.clone();
        }

        CoolType::named(OBJECT)
    }

    fn lookup_method(&self, class: &str, method: &str) -> Option<MethodSig> {
        let mut cur = class.to_string();
        loop {
            let info = self.classes.get(&cur)?;
            if let Some(sig) = info.methods.get(method) {
                return Some(sig.clone());
            }
            if info.parent == cur {
                return None;
            }
            cur = info.parent.clone();
        }
    }
}

/// Scoped object environment O(v)=T.
#[derive(Clone, Debug)]
struct ObjEnv {
    scopes: Vec<HashMap<String, CoolType>>,
}

impl ObjEnv {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, name: String, ty: CoolType) {
        self.scopes.last_mut().unwrap().insert(name, ty);
    }

    fn get(&self, name: &str) -> Option<CoolType> {
        for s in self.scopes.iter().rev() {
            if let Some(t) = s.get(name) {
                return Some(t.clone());
            }
        }
        None
    }
}

fn parse_ty_name(ty: &str) -> CoolType {
    if ty == "SELF_TYPE" {
        CoolType::SelfType
    } else {
        CoolType::named(ty)
    }
}

/// Public entry point.
pub fn type_check_program(p: &Program) -> Result<(), Vec<TypeError>> {
    let mut errors = vec![];
    let mut ctx = TypeContext::new();

    install_user_class_headers(&mut ctx, p, &mut errors);
    validate_inheritance(&ctx, &mut errors);
    install_features_and_check(&ctx, p, &mut errors);

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn install_user_class_headers(ctx: &mut TypeContext, p: &Program, errors: &mut Vec<TypeError>) {
    for c in &p.classes {
        let name = c.name.as_str();

        if ctx.has_class(name) {
            errors.push(TypeError::new(format!("Duplicate class definition: {name}")));
            continue;
        }

        let parent = c.parent.clone().unwrap_or_else(|| OBJECT.to_string());

        // Common restriction
        if parent == INT || parent == BOOL || parent == STRING || parent == "SELF_TYPE" {
            errors.push(TypeError::new(format!(
                "Class {name} cannot inherit from {parent}"
            )));
        }

        ctx.classes.insert(
            name.to_string(),
            ClassInfo {
                parent,
                attrs: HashMap::new(),
                methods: HashMap::new(),
            },
        );
    }
}

fn validate_inheritance(ctx: &TypeContext, errors: &mut Vec<TypeError>) {
    // Parent existence
    for (name, info) in &ctx.classes {
        if name == OBJECT {
            continue;
        }
        if !ctx.has_class(&info.parent) {
            errors.push(TypeError::new(format!(
                "Class {name} has undefined parent {}",
                info.parent
            )));
        }
    }

    // Cycle detection
    for name in ctx.classes.keys() {
        let mut seen = BTreeSet::<String>::new();
        let mut cur = name.clone();

        loop {
            if !seen.insert(cur.clone()) {
                errors.push(TypeError::new(format!(
                    "Inheritance cycle detected involving class {name}"
                )));
                break;
            }

            let info = match ctx.classes.get(&cur) {
                Some(i) => i,
                None => break,
            };
            if info.parent == cur {
                break;
            }
            cur = info.parent.clone();
        }
    }
}

fn install_features_and_check(ctx: &TypeContext, p: &Program, errors: &mut Vec<TypeError>) {
    let mut ctx2 = ctx.clone();

    // Phase 1: install signatures with borrow-safe structure
    for class in &p.classes {
        let cname = class.name.clone();

        for feat in &class.features {
            match feat {
                Feature::Attr { name, ty, .. } => {
                    let declared = parse_ty_name(ty);

                    if let CoolType::Named(tn) = &declared {
                        if !ctx2.has_class(tn) {
                            errors.push(TypeError::new(format!(
                                "Unknown type '{tn}' for attribute {cname}.{name}"
                            )));
                        }
                    }

                    let duplicate = match ctx2.classes.get(&cname) {
                        Some(info) => info.attrs.contains_key(name),
                        None => false,
                    };
                    if duplicate {
                        errors.push(TypeError::new(format!(
                            "Duplicate attribute {name} in class {cname}"
                        )));
                        continue;
                    }

                    if let Some(info) = ctx2.classes.get_mut(&cname) {
                        info.attrs.insert(name.clone(), declared);
                    }
                }

                Feature::Method {
                    name,
                    formals,
                    ret_type,
                    ..
                } => {
                    let mut formal_tys = Vec::new();
                    let mut formal_names = BTreeSet::new();

                    for f in formals {
                        if !formal_names.insert(f.name.clone()) {
                            errors.push(TypeError::new(format!(
                                "Duplicate formal parameter {} in method {cname}.{name}",
                                f.name
                            )));
                        }
                        let t = parse_ty_name(&f.ty);
                        if let CoolType::Named(tn) = &t {
                            if !ctx2.has_class(tn) {
                                errors.push(TypeError::new(format!(
                                    "Unknown type '{tn}' for formal {} in method {cname}.{name}",
                                    f.name
                                )));
                            }
                        }
                        formal_tys.push(t);
                    }

                    let ret = parse_ty_name(ret_type);
                    if let CoolType::Named(tn) = &ret {
                        if !ctx2.has_class(tn) {
                            errors.push(TypeError::new(format!(
                                "Unknown return type '{tn}' for method {cname}.{name}"
                            )));
                        }
                    }

                    let sig = MethodSig {
                        formals: formal_tys,
                        ret,
                    };

                    let duplicate = match ctx2.classes.get(&cname) {
                        Some(info) => info.methods.contains_key(name),
                        None => false,
                    };
                    if duplicate {
                        errors.push(TypeError::new(format!(
                            "Duplicate method {name} in class {cname}"
                        )));
                        continue;
                    }

                    if let Some(info) = ctx2.classes.get_mut(&cname) {
                        info.methods.insert(name.clone(), sig);
                    }
                }
            }
        }
    }

    // Phase 2: override checks (immutable borrows only)
    for class in &p.classes {
        let cname = class.name.as_str();
        let parent = ctx2
            .classes
            .get(cname)
            .map(|c| c.parent.clone())
            .unwrap_or_else(|| OBJECT.to_string());

        for feat in &class.features {
            if let Feature::Method { name, .. } = feat {
                if let Some(parent_sig) = ctx2.lookup_method(&parent, name) {
                    if let Some(my_sig) = ctx2.lookup_method(cname, name) {
                        if my_sig != parent_sig {
                            errors.push(TypeError::new(format!(
                                "Invalid override of method {cname}.{name}: signature differs from inherited method"
                            )));
                        }
                    }
                }
            }
        }
    }

    // Phase 3: type-check bodies
    for class in &p.classes {
        let cname = class.name.as_str();

        let mut o = ObjEnv::new();
        o.insert("self".to_string(), CoolType::SelfType);
        add_all_attrs_to_env(&ctx2, cname, &mut o);

        for feat in &class.features {
            match feat {
                Feature::Attr { name, ty, init } => {
                    if let Some(init_expr) = init {
                        let declared = parse_ty_name(ty);
                        let t_init = type_of_expr(&ctx2, &mut o.clone(), cname, init_expr, errors);
                        if !ctx2.conforms(&t_init, &declared, cname) {
                            errors.push(TypeError::new(format!(
                                "Attribute init type does not conform: {cname}.{name} : {:?} <- {:?}",
                                declared, t_init
                            )));
                        }
                    }
                }

                Feature::Method {
                    name,
                    formals,
                    ret_type,
                    body,
                } => {
                    let mut o2 = o.clone();
                    o2.push();
                    for f in formals {
                        o2.insert(f.name.clone(), parse_ty_name(&f.ty));
                    }

                    let body_ty = type_of_expr(&ctx2, &mut o2, cname, body, errors);
                    let declared_ret = parse_ty_name(ret_type);

                    if !ctx2.conforms(&body_ty, &declared_ret, cname) {
                        errors.push(TypeError::new(format!(
                            "Method body type does not conform: {cname}.{name} declared {:?} but body is {:?}",
                            declared_ret, body_ty
                        )));
                    }
                }
            }
        }
    }
}

fn add_all_attrs_to_env(ctx: &TypeContext, class: &str, env: &mut ObjEnv) {
    let mut chain = Vec::<String>::new();
    let mut cur = class.to_string();
    loop {
        chain.push(cur.clone());
        let info = match ctx.classes.get(&cur) {
            Some(i) => i,
            None => break,
        };
        if info.parent == cur {
            break;
        }
        cur = info.parent.clone();
    }

    for c in chain.into_iter().rev() {
        if let Some(info) = ctx.classes.get(&c) {
            for (k, v) in &info.attrs {
                env.insert(k.clone(), v.clone());
            }
        }
    }
}

/// Expression typing.
fn type_of_expr(
    ctx: &TypeContext,
    o: &mut ObjEnv,
    current_class: &str,
    e: &Expr,
    errors: &mut Vec<TypeError>,
) -> CoolType {
    match e {
        Expr::Int(_) => CoolType::named(INT),
        Expr::Str(_) => CoolType::named(STRING),
        Expr::Bool(_) => CoolType::named(BOOL),
        Expr::Self_ => CoolType::SelfType,

        Expr::Id(name) => o.get(name).unwrap_or_else(|| {
            errors.push(TypeError::new(format!("Undefined identifier '{name}'")));
            CoolType::named(OBJECT)
        }),

        Expr::Assign { name, expr } => {
            if name == "self" {
                errors.push(TypeError::new("Cannot assign to self"));
            }

            let t_var = match o.get(name) {
                Some(t) => t,
                None => {
                    errors.push(TypeError::new(format!(
                        "Assignment to undefined identifier '{name}'"
                    )));
                    return type_of_expr(ctx, o, current_class, expr, errors);
                }
            };

            let t_rhs = type_of_expr(ctx, o, current_class, expr, errors);
            if !ctx.conforms(&t_rhs, &t_var, current_class) {
                errors.push(TypeError::new(format!(
                    "Type mismatch in assignment '{name} <- ...': rhs {:?} does not conform to {:?}",
                    t_rhs, t_var
                )));
            }
            t_rhs
        }

        Expr::Block(exprs) => {
            let mut last = CoolType::named(OBJECT);
            for ex in exprs {
                last = type_of_expr(ctx, o, current_class, ex, errors);
            }
            last
        }

        Expr::If { cond, then_, else_ } => {
            let t_cond = type_of_expr(ctx, o, current_class, cond, errors);
            if ctx.resolve_self_type(&t_cond, current_class) != CoolType::named(BOOL) {
                errors.push(TypeError::new(format!(
                    "If condition must be Bool, got {:?}",
                    t_cond
                )));
            }
            let t_then = type_of_expr(ctx, o, current_class, then_, errors);
            let t_else = type_of_expr(ctx, o, current_class, else_, errors);
            ctx.lub(&t_then, &t_else, current_class)
        }

        Expr::While { cond, body } => {
            let t_cond = type_of_expr(ctx, o, current_class, cond, errors);
            if ctx.resolve_self_type(&t_cond, current_class) != CoolType::named(BOOL) {
                errors.push(TypeError::new(format!(
                    "While condition must be Bool, got {:?}",
                    t_cond
                )));
            }
            let _ = type_of_expr(ctx, o, current_class, body, errors);
            CoolType::named(OBJECT)
        }

        Expr::Let { bindings, body } => {
            o.push();
            for b in bindings {
                let declared = parse_ty_name(&b.ty);
                if let CoolType::Named(tn) = &declared {
                    if !ctx.has_class(tn) {
                        errors.push(TypeError::new(format!(
                            "Unknown type '{tn}' in let binding '{}'",
                            b.name
                        )));
                    }
                }

                if let Some(init) = &b.init {
                    let t_init = type_of_expr(ctx, o, current_class, init, errors);
                    if !ctx.conforms(&t_init, &declared, current_class) {
                        errors.push(TypeError::new(format!(
                            "Let init type mismatch for {} : {:?} <- {:?}",
                            b.name, declared, t_init
                        )));
                    }
                }

                o.insert(b.name.clone(), declared);
            }

            let t_body = type_of_expr(ctx, o, current_class, body, errors);
            o.pop();
            t_body
        }

        Expr::Case { expr, arms } => {
            let _ = type_of_expr(ctx, o, current_class, expr, errors);

            let mut seen_types = BTreeSet::<String>::new();
            let mut acc: Option<CoolType> = None;

            for arm in arms {
                let arm_ty = parse_ty_name(&arm.ty);
                if let CoolType::Named(tn) = ctx.resolve_self_type(&arm_ty, current_class) {
                    if !seen_types.insert(tn.clone()) {
                        errors.push(TypeError::new(format!(
                            "Duplicate type '{tn}' in case arms"
                        )));
                    }
                }

                o.push();
                o.insert(arm.name.clone(), arm_ty);
                let t_arm_expr = type_of_expr(ctx, o, current_class, &arm.expr, errors);
                o.pop();

                acc = Some(match acc {
                    None => t_arm_expr,
                    Some(prev) => ctx.lub(&prev, &t_arm_expr, current_class),
                });
            }

            acc.unwrap_or_else(|| CoolType::named(OBJECT))
        }

        Expr::New(t) => {
            if t == "SELF_TYPE" {
                CoolType::SelfType
            } else if !ctx.has_class(t) {
                errors.push(TypeError::new(format!("Unknown type in 'new': {t}")));
                CoolType::named(OBJECT)
            } else {
                CoolType::named(t)
            }
        }

        Expr::IsVoid(inner) => {
            let _ = type_of_expr(ctx, o, current_class, inner, errors);
            CoolType::named(BOOL)
        }

        Expr::Not(inner) => {
            let t = type_of_expr(ctx, o, current_class, inner, errors);
            if ctx.resolve_self_type(&t, current_class) != CoolType::named(BOOL) {
                errors.push(TypeError::new(format!("'not' expects Bool, got {:?}", t)));
            }
            CoolType::named(BOOL)
        }

        Expr::Neg(inner) => {
            let t = type_of_expr(ctx, o, current_class, inner, errors);
            if ctx.resolve_self_type(&t, current_class) != CoolType::named(INT) {
                errors.push(TypeError::new(format!("'~' expects Int, got {:?}", t)));
            }
            CoolType::named(INT)
        }

        Expr::Bin { op, lhs, rhs } => {
            let tl = type_of_expr(ctx, o, current_class, lhs, errors);
            let tr = type_of_expr(ctx, o, current_class, rhs, errors);

            match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                    if ctx.resolve_self_type(&tl, current_class) != CoolType::named(INT)
                        || ctx.resolve_self_type(&tr, current_class) != CoolType::named(INT)
                    {
                        errors.push(TypeError::new(format!(
                            "Arithmetic op expects Int/Int, got {:?} and {:?}",
                            tl, tr
                        )));
                    }
                    CoolType::named(INT)
                }

                BinOp::Lt | BinOp::Le => {
                    if ctx.resolve_self_type(&tl, current_class) != CoolType::named(INT)
                        || ctx.resolve_self_type(&tr, current_class) != CoolType::named(INT)
                    {
                        errors.push(TypeError::new(format!(
                            "Comparison op expects Int/Int, got {:?} and {:?}",
                            tl, tr
                        )));
                    }
                    CoolType::named(BOOL)
                }

                BinOp::Eq => {
                    let l = ctx.resolve_self_type(&tl, current_class);
                    let r = ctx.resolve_self_type(&tr, current_class);

                    let basic = |t: &CoolType| -> Option<&'static str> {
                        match t {
                            CoolType::Named(s) if s == INT => Some(INT),
                            CoolType::Named(s) if s == BOOL => Some(BOOL),
                            CoolType::Named(s) if s == STRING => Some(STRING),
                            _ => None,
                        }
                    };

                    if let (Some(lb), Some(rb)) = (basic(&l), basic(&r)) {
                        if lb != rb {
                            errors.push(TypeError::new(format!(
                                "Illegal equality test between {} and {}",
                                lb, rb
                            )));
                        }
                    } else if basic(&l).is_some() || basic(&r).is_some() {
                        errors.push(TypeError::new(format!(
                            "Illegal equality test between {:?} and {:?}",
                            l, r
                        )));
                    }

                    CoolType::named(BOOL)
                }
            }
        }

        Expr::Dispatch {
            recv,
            static_type,
            method,
            args,
        } => {
            let t0 = type_of_expr(ctx, o, current_class, recv, errors);
            let t0_resolved = ctx.resolve_self_type(&t0, current_class);

            let recv_class = match &t0_resolved {
                CoolType::Named(c) => c.clone(),
                CoolType::SelfType => current_class.to_string(),
            };

            let dispatch_class = if let Some(st) = static_type {
                if !ctx.has_class(st) {
                    errors.push(TypeError::new(format!("Unknown static dispatch type @{st}")));
                    recv_class.clone()
                } else {
                    let st_ty = CoolType::named(st.clone());
                    if !ctx.conforms(&t0, &st_ty, current_class) {
                        errors.push(TypeError::new(format!(
                            "Static dispatch requires receiver type {:?} to conform to {}",
                            t0, st
                        )));
                    }
                    st.clone()
                }
            } else {
                recv_class.clone()
            };

            let Some(sig) = ctx.lookup_method(&dispatch_class, method) else {
                errors.push(TypeError::new(format!(
                    "Unknown method {dispatch_class}.{method}"
                )));
                return CoolType::named(OBJECT);
            };

            if sig.formals.len() != args.len() {
                errors.push(TypeError::new(format!(
                    "Arity mismatch calling {dispatch_class}.{method}: expected {}, got {}",
                    sig.formals.len(),
                    args.len()
                )));
            }

            for (i, arg) in args.iter().enumerate() {
                let t_arg = type_of_expr(ctx, o, current_class, arg, errors);
                if let Some(t_formal) = sig.formals.get(i) {
                    if !ctx.conforms(&t_arg, t_formal, current_class) {
                        errors.push(TypeError::new(format!(
                            "Arg {} type mismatch calling {dispatch_class}.{method}: expected {:?}, got {:?}",
                            i + 1,
                            t_formal,
                            t_arg
                        )));
                    }
                }
            }

            match sig.ret {
                CoolType::SelfType => t0,
                other => other,
            }
        }

        Expr::Paren(inner) => type_of_expr(ctx, o, current_class, inner, errors),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse_program;

    #[test]
    fn typechecks_simple_arith() {
        let src = r#"
            class Main inherits Object {
              main() : Int { 1 + 2 * 3 };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        let res = type_check_program(&prog);
        assert!(res.is_ok(), "{res:?}");
    }

    #[test]
    fn rejects_bad_if_condition() {
        let src = r#"
            class Main inherits Object {
              main() : Int {
                if 1 then 2 else 3 fi
              };
            };
        "#;

        let toks = lex(src).unwrap();
        let prog = parse_program(&toks).unwrap();
        let res = type_check_program(&prog);
        assert!(res.is_err());
        let errs = res.err().unwrap();
        assert!(errs.iter().any(|e| e.msg.contains("If condition must be Bool")));
    }
}
