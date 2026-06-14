"""End-to-end tests over the checked-in programs in the `examples/` folder.

Given today's codegen subset (`codegen_int_expr` handles only integer literals
and `+ - * /`), only `test.cl` compiles and runs. `hello.cl` and `bad.cl` are
expected to fail compilation; we assert *why* so these tests document the current
behavior and will flag changes when the compiler grows.
"""

from __future__ import annotations

from pathlib import Path

from conftest import Coolc, requires_apple_silicon

pytestmark = requires_apple_silicon


def test_test_cl_compiles_and_prints_7(coolc: Coolc, examples_dir: Path) -> None:
    """`examples/test.cl` is `main() : Int { 1 + 2 * 3 }` -> must print `7`."""
    result = coolc.compile(examples_dir / "test.cl")

    assert result.returncode == 0, f"compile failed:\n{result.stderr}"
    assert "Built ./a.out" in result.stdout
    assert result.executable.is_file()

    run = coolc.run_program(result)
    assert run.returncode == 0
    # print_int emits the digits with no trailing newline.
    assert run.stdout == "7"


def test_hello_cl_codegen_unsupported(coolc: Coolc, examples_dir: Path) -> None:
    """`examples/hello.cl` type-checks (block returns Bool <= Object) but its body
    is not in the codegen subset (a block, not a bare Int expression), so codegen
    fails. Update this test once lowering supports blocks/non-Int Main bodies.
    """
    result = coolc.compile(examples_dir / "hello.cl")

    assert result.returncode == 1
    assert not result.executable.is_file()
    combined = (result.stdout + result.stderr).lower()
    assert "codegen" in combined or "unsupported" in combined, result.stderr


def test_bad_cl_typecheck_error(coolc: Coolc, examples_dir: Path) -> None:
    """`examples/bad.cl` uses `if 1 then ...` — the condition is `Int`, not `Bool`,
    so the type checker rejects it before codegen runs.
    """
    result = coolc.compile(examples_dir / "bad.cl")

    assert result.returncode == 1
    assert not result.executable.is_file()
    assert "type check" in result.stderr.lower()
    assert "If condition must be Bool" in result.stderr
