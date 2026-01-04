use cool_codegen::Codegen;
use cool_frontend::{lex, parse_program, type_check_program};
use inkwell::context::Context;

fn compile_from_str(src: &str) -> Result<(), String> {
    let toks = lex(src).map_err(|e| format!("lex error: {e:?}"))?;
    let prog = parse_program(&toks).map_err(|e| format!("parse error: {e:?}"))?;

    // typecheck returns Ok(()) or errors; adapt if your signature differs
    type_check_program(&prog).map_err(|errs| format!("type errors: {errs:?}"))?;

    let ctx = Context::create();
    let mut cg = Codegen::new(&ctx, "test_mod");
    cg.compile_program(&prog)
        .map_err(|e| format!("codegen error: {e}"))?;

    cg.module
        .verify()
        .map_err(|e| format!("llvm verify failed: {e}"))?;

    Ok(())
}

#[test]
fn codegen_minimal_arithmetic() {
    let src = r#"
        class Main inherits Object {
          main() : Int { 1 + 2 * 3 };
        };
    "#;

    compile_from_str(src).unwrap();
}
