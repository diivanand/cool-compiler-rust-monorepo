"""Shared pytest fixtures and helpers for the coolc end-to-end test suite.

These tests are *black box*: they invoke the real `coolc` executable on COOL
source files, link against the runtime, run the produced native binary, and
assert on its output. That exercises the whole pipeline
(lex -> parse -> typecheck -> lower -> emit -> link -> run), which the in-process
Rust tests deliberately do not.

Design notes
------------
* The emitted object/executable target ``aarch64-apple-darwin``, so the produced
  ``./a.out`` only runs on Apple Silicon macOS. On any other host the whole suite
  is *skipped* (see :data:`requires_apple_silicon`), never failed.
* ``coolc`` writes ``a.out``/``a.out.o`` relative to the current working directory
  and links ``target/debug/libcool_runtime.a`` relative to the cwd. To keep tests
  isolated and avoid polluting the repo, every compile runs inside a fresh pytest
  ``tmp_path`` with a symlink to the real runtime archive placed at
  ``<tmp>/target/debug/libcool_runtime.a``.
"""

from __future__ import annotations

import platform
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

import pytest

# Skip marker applied to every e2e module: the linked binary is arm64 macOS only.
requires_apple_silicon = pytest.mark.skipif(
    not (sys.platform == "darwin" and platform.machine() == "arm64"),
    reason="coolc emits aarch64-apple-darwin objects; running ./a.out requires Apple Silicon macOS",
)


@pytest.fixture(scope="session")
def repo_root() -> Path:
    """Absolute path to the workspace root (this file is tests/e2e/conftest.py)."""
    return Path(__file__).resolve().parents[2]


@pytest.fixture(scope="session")
def examples_dir(repo_root: Path) -> Path:
    return repo_root / "examples"


@dataclass(frozen=True)
class Binaries:
    """Absolute paths to the artifacts the tests depend on."""

    coolc: Path
    runtime_lib: Path


@pytest.fixture(scope="session")
def built_binaries(repo_root: Path) -> Binaries:
    """Build (once) and locate the `coolc` binary and the runtime static lib.

    `cool_runtime` is a staticlib and is *not* a Cargo dependency of `coolc`, so
    both must be built explicitly. We build both in one `cargo` invocation.
    """
    if not (sys.platform == "darwin" and platform.machine() == "arm64"):
        pytest.skip("e2e tests require Apple Silicon macOS")

    if shutil.which("cargo") is None:
        pytest.fail("`cargo` not found on PATH; cannot build the Rust binaries for e2e tests")

    subprocess.run(
        ["cargo", "build", "-p", "cool_runtime", "-p", "coolc"],
        cwd=repo_root,
        check=True,
    )

    coolc = repo_root / "target" / "debug" / "coolc"
    runtime_lib = repo_root / "target" / "debug" / "libcool_runtime.a"
    assert coolc.is_file(), f"coolc binary not found at {coolc} after build"
    assert runtime_lib.is_file(), f"runtime lib not found at {runtime_lib} after build"
    return Binaries(coolc=coolc, runtime_lib=runtime_lib)


@dataclass(frozen=True)
class CompileResult:
    """Outcome of one `coolc` invocation."""

    returncode: int
    stdout: str
    stderr: str
    workdir: Path

    @property
    def executable(self) -> Path:
        return self.workdir / "a.out"

    @property
    def succeeded(self) -> bool:
        return self.returncode == 0 and self.executable.is_file()


class Coolc:
    """A thin driver around the `coolc` executable, scoped to one work directory."""

    def __init__(self, binaries: Binaries, workdir: Path) -> None:
        self._binaries = binaries
        self.workdir = workdir
        # coolc links `target/debug/libcool_runtime.a` relative to its cwd, so make
        # the real archive reachable at that path inside our isolated workdir.
        link_dir = workdir / "target" / "debug"
        link_dir.mkdir(parents=True, exist_ok=True)
        (link_dir / "libcool_runtime.a").symlink_to(binaries.runtime_lib)

    def write_program(self, source: str, name: str = "prog.cl") -> Path:
        """Write COOL source into the workdir and return its path."""
        path = self.workdir / name
        path.write_text(source)
        return path

    def main_returning_int(self, expr: str, name: str = "prog.cl") -> Path:
        """Convenience: wrap an Int expression in a minimal `Main.main` and write it."""
        return self.write_program(
            f"class Main {{\n  main() : Int {{\n    {expr}\n  }};\n}};\n", name=name
        )

    def compile(self, *sources: Path) -> CompileResult:
        """Run `coolc` on the given source files (absolute paths) inside the workdir."""
        proc = subprocess.run(
            [str(self._binaries.coolc), *(str(s) for s in sources)],
            cwd=self.workdir,
            capture_output=True,
            text=True,
        )
        return CompileResult(
            returncode=proc.returncode,
            stdout=proc.stdout,
            stderr=proc.stderr,
            workdir=self.workdir,
        )

    def run_program(self, result: CompileResult) -> subprocess.CompletedProcess[str]:
        """Execute the compiled `./a.out` and capture its output."""
        assert result.succeeded, "cannot run a program that did not compile successfully"
        return subprocess.run(
            [str(result.executable)],
            cwd=self.workdir,
            capture_output=True,
            text=True,
        )


@pytest.fixture
def coolc(built_binaries: Binaries, tmp_path: Path) -> Coolc:
    """A `Coolc` driver bound to a fresh, isolated working directory per test."""
    return Coolc(built_binaries, tmp_path)
