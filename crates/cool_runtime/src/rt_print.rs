// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! Runtime I/O helpers exposed to generated COOL code.
//!
//! ## Why we don't use Rust's `println!`/`print!` here
//!
//! `cool_runtime` is compiled as a `staticlib` and linked into an executable
//! whose entry point is the LLVM-generated C `main` (see `cool_codegen`), *not*
//! Rust's `fn main`. That means Rust's standard runtime startup/shutdown
//! (`std::rt::lang_start`) never runs.
//!
//! Rust's `print!` writes into a process-global, line-buffered `Stdout`. That
//! buffer is normally flushed when the Rust runtime tears down at the end of
//! `main`. Because our `main` is C, that teardown never happens, so anything
//! left in the buffer (e.g. a value printed without a trailing newline) is
//! silently dropped on exit.
//!
//! To stay correct regardless of who owns `main`, we bypass Rust's buffered
//! stdout entirely and write straight to file descriptor 1 via `libc::write`.
//! This is unbuffered, so there is nothing to flush and nothing to lose.

use std::ffi::{CStr, c_void};

/// File descriptor for standard output (POSIX `STDOUT_FILENO`).
const STDOUT_FD: libc::c_int = 1;

/// Write an entire byte slice to stdout, retrying on partial writes.
///
/// `write(2)` may legally write fewer bytes than requested (e.g. when output is
/// a pipe), so we loop until everything is flushed or an error occurs. We
/// deliberately swallow errors: COOL has no notion of a failed `out_*` call.
fn write_all_stdout(bytes: &[u8]) {
    let mut written = 0;
    while written < bytes.len() {
        // SAFETY: we pass a valid pointer/length pair derived from `bytes`, and
        // only advance `written` by the non-negative count `write` reports.
        let n = unsafe {
            libc::write(
                STDOUT_FD,
                bytes[written..].as_ptr() as *const c_void,
                bytes.len() - written,
            )
        };
        if n <= 0 {
            break; // error or nothing written; give up rather than spin forever
        }
        written += n as usize;
    }
}

/// Print a NUL-terminated C string (COOL `String`) without a trailing newline.
pub fn print_cstr(s: *const libc::c_char) {
    if s.is_null() {
        write_all_stdout(b"<null>");
        return;
    }
    // SAFETY: the caller guarantees `s` points at a valid NUL-terminated string.
    let cstr = unsafe { CStr::from_ptr(s) };
    // COOL strings are byte strings; emit the raw bytes verbatim rather than
    // forcing UTF-8 validation, which would reject otherwise-valid output.
    write_all_stdout(cstr.to_bytes());
}

/// Print a 32-bit integer (COOL `Int`) without a trailing newline.
pub fn print_int(i: i32) {
    // `itoa`-style formatting via the standard library is plenty here; the
    // resulting digits are pure ASCII, so the bytes write directly.
    write_all_stdout(i.to_string().as_bytes());
}
