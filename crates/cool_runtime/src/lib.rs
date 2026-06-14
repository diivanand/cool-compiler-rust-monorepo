// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! `cool_runtime` — the small support library every compiled COOL program links
//! against. It is built as a `staticlib` (see `Cargo.toml`) so the C `main` that
//! codegen emits can call directly into it.
//!
//! It provides two services:
//! - a [garbage collector](gc) for heap object allocation and reclamation, and
//! - [I/O primitives](rt_print) backing COOL's `out_int` / `out_string`.
//!
//! Everything the generated code calls is exported below as `extern "C"` with a
//! stable, unmangled name. Those names are the ABI; they must match the
//! declarations in `cool_codegen::abi`.

mod gc;
mod rt_print;

pub use gc::{ObjHeader, VTable};

// SAFETY: These exported symbols must have globally-unique names across the final linked binary.
// We intentionally export C ABI entrypoints used by the generated LLVM code. See Rust 2024
// "unsafe attributes" for why #[no_mangle] must be written as #[unsafe(no_mangle)].

#[unsafe(no_mangle)]
pub extern "C" fn cool_rt_print_cstr(s: *const libc::c_char) {
    rt_print::print_cstr(s);
}

#[unsafe(no_mangle)]
pub extern "C" fn cool_rt_print_int(i: i32) {
    rt_print::print_int(i);
}

#[unsafe(no_mangle)]
pub extern "C" fn cool_gc_push_root(slot: *mut *mut u8) {
    gc::push_root(slot);
}

#[unsafe(no_mangle)]
pub extern "C" fn cool_gc_pop_roots(n: u32) {
    gc::pop_roots(n as usize);
}

#[unsafe(no_mangle)]
pub extern "C" fn cool_gc_collect() {
    gc::collect();
}

/// Allocate an object of `size_bytes` with `tag` and `vtable`.
/// Returns pointer to object base (i.e., ObjHeader start).
#[unsafe(no_mangle)]
pub extern "C" fn cool_alloc(tag: u32, size_bytes: u32, vtable: *const VTable) -> *mut u8 {
    gc::alloc(tag, size_bytes, vtable)
}
