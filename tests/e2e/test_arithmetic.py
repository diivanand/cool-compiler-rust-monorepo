"""End-to-end arithmetic tests driven by real COOL programs in
``examples/arithmetic/``.

Each runnable case is a committed ``<name>.cl`` file with a sibling
``<name>.expected`` holding the exact stdout the compiled program must print. The
test discovers every pair, compiles it with `coolc`, runs the binary, and compares
output — so adding a new case is just dropping in a new ``.cl`` + ``.expected`` pair.

The programs use only what the current codegen subset supports: integer literals
and the four binary operators ``+ - * /`` (all left-associative; ``/`` truncates).
``examples/arithmetic/unsupported/`` holds programs outside that subset (e.g.
parentheses) that are expected to fail compilation today.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from conftest import Coolc, requires_apple_silicon

pytestmark = requires_apple_silicon

# Resolve example dirs relative to this file (tests/e2e/ -> repo root -> examples).
_ARITHMETIC_DIR = Path(__file__).resolve().parents[2] / "examples" / "arithmetic"
_RUNNABLE = sorted(_ARITHMETIC_DIR.glob("*.cl"))
_UNSUPPORTED = sorted((_ARITHMETIC_DIR / "unsupported").glob("*.cl"))


@pytest.mark.parametrize("program", _RUNNABLE, ids=[p.stem for p in _RUNNABLE])
def test_arithmetic_program_output(coolc: Coolc, program: Path) -> None:
    """Compile and run a committed arithmetic program; assert its printed output
    matches the sibling ``.expected`` file."""
    expected = program.with_suffix(".expected").read_text()

    result = coolc.compile(program)
    assert result.returncode == 0, f"compile of {program.name} failed:\n{result.stderr}"

    run = coolc.run_program(result)
    assert run.returncode == 0
    # print_int emits digits with no trailing newline; strip both sides so a
    # trailing newline in the committed .expected file doesn't cause spurious fails.
    assert run.stdout.strip() == expected.strip(), (
        f"{program.name} printed {run.stdout!r}, expected {expected!r}"
    )


@pytest.mark.parametrize("program", _UNSUPPORTED, ids=[p.stem for p in _UNSUPPORTED])
def test_unsupported_program_fails_to_compile(coolc: Coolc, program: Path) -> None:
    """Programs in ``examples/arithmetic/unsupported/`` use constructs outside the
    current codegen subset (e.g. parentheses), so compilation is expected to fail.
    When the backend grows to support one of these, move its file up into
    ``examples/arithmetic/`` (with a ``.expected`` sibling) so it becomes a
    positive test instead.
    """
    result = coolc.compile(program)

    assert result.returncode == 1
    assert not result.executable.is_file()
    combined = (result.stdout + result.stderr).lower()
    assert "codegen" in combined or "unsupported" in combined, result.stderr
