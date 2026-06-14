// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! The Application Binary Interface (ABI) contract between the generated code
//! and the runtime.
//!
//! Codegen and `cool_runtime` are compiled separately and only meet at link
//! time, so they must agree *byte-for-byte* on object layout and *exactly* on
//! symbol names. This module is the single source of truth for both. If you
//! change `ObjHeader` in the runtime, change the offsets here (and the matching
//! `struct_type` in `lowering.rs`) too — a mismatch produces memory corruption
//! that no compiler error will catch.

/// Number of pointer-sized words in the object header.
pub const OBJ_HEADER_WORDS: usize = 4;

/// Byte offsets of each field within the object header. Note the gap between
/// `MARKED` (offset 12, one byte) and `TAG` (offset 16): the 3 bytes of padding
/// keep `tag` aligned to a 4-byte boundary, matching the C struct layout.
pub const OFFSET_VTABLE_PTR: usize = 0;
pub const OFFSET_SIZE_BYTES: usize = 8;
pub const OFFSET_MARKED: usize = 12;
pub const OFFSET_TAG: usize = 16;

/// Common class tags (reserved)
pub const TAG_OBJECT: u32 = 0;
pub const TAG_INT: u32 = 1;
pub const TAG_BOOL: u32 = 2;
pub const TAG_STRING: u32 = 3;

/// Runtime function names (must match `#[unsafe(no_mangle)]` exports)
pub const FN_ALLOC: &str = "cool_alloc";
pub const FN_GC_PUSH_ROOT: &str = "cool_gc_push_root";
pub const FN_GC_POP_ROOTS: &str = "cool_gc_pop_roots";
pub const FN_GC_COLLECT: &str = "cool_gc_collect";

pub const FN_PRINT_INT: &str = "cool_rt_print_int";
pub const FN_PRINT_CSTR: &str = "cool_rt_print_cstr";
