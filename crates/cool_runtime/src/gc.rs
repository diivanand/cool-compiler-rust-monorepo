// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! A simple stop-the-world, mark-and-sweep garbage collector.
//!
//! ## How a tracing GC works, in three steps
//!
//! 1. **Roots**: the set of pointers the program can reach *directly* — here,
//!    live local variables, which the generated code registers in a *shadow
//!    stack* via [`push_root`]/[`pop_roots`]. (A "shadow stack" is an explicit,
//!    GC-visible list of root slots, used instead of trying to scan the real
//!    machine stack and registers.)
//! 2. **Mark**: starting from the roots, recursively visit every reachable
//!    object and set its `marked` bit. The vtable's pointer-offset map tells us
//!    which fields of an object are themselves object pointers to follow.
//! 3. **Sweep**: walk every allocation; free the ones still unmarked (garbage)
//!    and clear the mark bit on the survivors for next time.
//!
//! "Stop-the-world" means collection runs to completion with no mutator
//! (program) activity interleaved — the simplest model to reason about.
//!
//! Allocation is amortized: we only collect when `bytes_allocated` crosses a
//! growing threshold (`next_gc`), so programs that allocate a little never pay.

use libc::{free, malloc};
use std::cell::RefCell;
use std::ptr;

/// Per-class metadata, shared (immutably) by all instances of a class. Its
/// address is stored in each object's header, both for method dispatch and so
/// the GC can find the object's pointer fields.
///
/// `#[repr(C)]` pins the field order/layout so it matches what codegen emits.
#[repr(C)]
pub struct VTable {
    pub tag: u32,
    pub obj_size: u32,
    /// How many of this object's fields are object pointers the GC must trace.
    pub ptr_count: u32,
    /// Pointer to an array of `ptr_count` byte offsets locating those fields.
    pub ptr_offsets: *const u32,
    // In the real layout, method function-pointers follow these fields; the GC
    // never touches them, so we leave them out of this struct view.
}

// SAFETY: VTables are immutable metadata created by the compiler/codegen and never mutated at runtime.
// The raw pointer fields are treated as read-only for the lifetime of the program.
unsafe impl Send for VTable {}
unsafe impl Sync for VTable {}

/// The common prefix of every heap object. Codegen builds an identical struct
/// (see `cool_codegen::lowering`), and the offsets are mirrored in
/// `cool_codegen::abi`.
#[repr(C)]
pub struct ObjHeader {
    pub vtable: *const VTable, // MUST be first: dispatch loads slot 0 = *obj
    pub size_bytes: u32,       // total object size in bytes
    pub marked: u8,            // GC mark bit (0 = white/unvisited, 1 = reachable)
    pub _pad0: [u8; 3],        // padding so `tag` is 4-byte aligned
    pub tag: u32,              // class tag / type id
}

/// All collector bookkeeping for one thread.
struct HeapState {
    /// Every live allocation's base pointer, so sweep can iterate the whole heap.
    objects: Vec<*mut u8>,
    /// The shadow stack: addresses of the *slots* holding root pointers. We store
    /// `*mut *mut u8` (a pointer to a pointer) so we read the slot's *current*
    /// value at collection time, even after the variable is reassigned.
    roots: Vec<*mut *mut u8>,
    bytes_allocated: usize,
    /// Allocate-until threshold that triggers the next collection.
    next_gc: usize,
}

// State is `thread_local` because this minimal GC isn't synchronized; each thread
// owns its own heap. `RefCell` gives us checked interior mutability through the
// shared `&` access that `thread_local!` provides.
thread_local! {
    static HEAP: RefCell<HeapState> = const { RefCell::new(HeapState {
        objects: Vec::new(),
        roots: Vec::new(),
        bytes_allocated: 0,
        next_gc: 1 << 20, // 1 MiB initial threshold
    }) };
}

/// Register a stack slot as a GC root. Generated code calls this when a local
/// variable holding an object pointer comes into scope.
pub fn push_root(slot: *mut *mut u8) {
    HEAP.with(|h| h.borrow_mut().roots.push(slot));
}

/// Unregister the `n` most-recently-pushed roots (variables going out of scope).
/// `saturating_sub` guards against an over-pop underflowing the length.
pub fn pop_roots(n: usize) {
    HEAP.with(|h| {
        let mut st = h.borrow_mut();
        let new_len = st.roots.len().saturating_sub(n);
        st.roots.truncate(new_len);
    });
}

/// Allocate an object of `size_bytes` with `tag` and `vtable`.
/// Returns pointer to object base (i.e., ObjHeader start).
pub fn alloc(tag: u32, size_bytes: u32, vtable: *const VTable) -> *mut u8 {
    let sz = size_bytes as usize;

    unsafe {
        let mem = malloc(sz) as *mut u8;
        if mem.is_null() {
            panic!("OOM allocating {sz} bytes");
        }
        ptr::write_bytes(mem, 0, sz);

        let hdr = mem as *mut ObjHeader;
        (*hdr).vtable = vtable;
        (*hdr).size_bytes = size_bytes;
        (*hdr).marked = 0;
        (*hdr).tag = tag;

        // Push into heap tracking
        let should_collect = HEAP.with(|h| {
            let mut st = h.borrow_mut();
            st.objects.push(mem);
            st.bytes_allocated += sz;
            st.bytes_allocated >= st.next_gc
        });

        if should_collect {
            collect();
        }

        mem
    }
}

/// Run a full collection: mark everything reachable from the roots, then sweep
/// away the rest.
pub fn collect() {
    // We snapshot the roots into a local Vec first so that we are not holding a
    // borrow of `HEAP` while `mark_from_roots`/`sweep` re-borrow it. Mixing a
    // live borrow with the re-borrows inside marking would panic at runtime.
    let roots_snapshot: Vec<*mut *mut u8> = HEAP.with(|h| h.borrow().roots.clone());

    mark_from_roots(&roots_snapshot);
    sweep_and_update_threshold();
}

fn mark_from_roots(roots: &[*mut *mut u8]) {
    for &slot in roots {
        unsafe {
            if slot.is_null() {
                continue;
            }
            let obj = *slot;
            if obj.is_null() {
                continue;
            }
            mark_obj(obj);
        }
    }
}

/// Recursively mark `obj` and everything it points to.
///
/// The `marked` check at the top is what makes this terminate on cyclic object
/// graphs: once an object is marked we never recurse into it again.
///
/// Note: this recurses on the *native* stack, so a very deep or long object
/// chain could overflow it. A production GC would use an explicit work-list
/// (mark stack) instead; we keep the recursive form for clarity.
unsafe fn mark_obj(obj: *mut u8) {
    unsafe {
        let hdr = obj as *mut ObjHeader;
        if (*hdr).marked != 0 {
            return; // already visited — also breaks reference cycles
        }
        (*hdr).marked = 1;

        let vt = (*hdr).vtable;
        if vt.is_null() {
            return;
        }

        let ptr_count = (*vt).ptr_count as usize;
        let offsets = (*vt).ptr_offsets;

        for i in 0..ptr_count {
            let off = *offsets.add(i) as usize;
            let field_ptr_addr = obj.add(off) as *mut *mut u8;
            let child = *field_ptr_addr;
            if !child.is_null() {
                mark_obj(child);
            }
        }
    }
}

/// Free every unmarked object, retain the marked ones (clearing their bit for
/// the next cycle), and grow the collection threshold based on how much survived
/// — so a program with a large live set collects less often.
fn sweep_and_update_threshold() {
    HEAP.with(|h| {
        let mut st = h.borrow_mut();

        let mut alive = Vec::with_capacity(st.objects.len());
        let mut new_bytes = 0usize;

        for &obj in &st.objects {
            unsafe {
                let hdr = obj as *mut ObjHeader;
                if (*hdr).marked != 0 {
                    (*hdr).marked = 0;
                    alive.push(obj);
                    new_bytes += (*hdr).size_bytes as usize;
                } else {
                    free(obj as *mut _);
                }
            }
        }

        st.objects = alive;
        st.bytes_allocated = new_bytes;
        st.next_gc = (st.bytes_allocated * 2).max(1 << 20);
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gc_collects_unreachable() {
        // Fake class with no pointer fields
        static OFFSETS: [u32; 0] = [];
        static VT: VTable = VTable {
            tag: 1,
            obj_size: std::mem::size_of::<ObjHeader>() as u32,
            ptr_count: 0,
            ptr_offsets: OFFSETS.as_ptr(),
        };

        let a = alloc(1, std::mem::size_of::<ObjHeader>() as u32, &VT);
        let _b = alloc(1, std::mem::size_of::<ObjHeader>() as u32, &VT);

        // Root only 'a'
        let mut slot_a = a;
        push_root(&mut slot_a as *mut *mut u8);

        collect();

        // Only 'a' should remain alive
        let count = HEAP.with(|h| h.borrow().objects.len());
        assert_eq!(count, 1);

        pop_roots(1);

        collect();
        let count = HEAP.with(|h| h.borrow().objects.len());
        assert_eq!(count, 0);
    }
}
