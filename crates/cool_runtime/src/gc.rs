// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use libc::{free, malloc};
use std::cell::RefCell;
use std::ptr;

#[repr(C)]
pub struct VTable {
    pub tag: u32,
    pub obj_size: u32,
    pub ptr_count: u32,
    pub ptr_offsets: *const u32,
    // followed by method pointers in memory; runtime treats as opaque
}

// SAFETY: VTables are immutable metadata created by the compiler/codegen and never mutated at runtime.
// The raw pointer fields are treated as read-only for the lifetime of the program.
unsafe impl Send for VTable {}
unsafe impl Sync for VTable {}

#[repr(C)]
pub struct ObjHeader {
    pub vtable: *const VTable, // MUST be first for dispatch
    pub size_bytes: u32,       // total object size
    pub marked: u8,            // mark bit for GC
    pub _pad0: [u8; 3],
    pub tag: u32,              // class tag / type id
}

struct HeapState {
    objects: Vec<*mut u8>,    // base pointers
    roots: Vec<*mut *mut u8>, // addresses of stack slots (shadow stack)
    bytes_allocated: usize,
    next_gc: usize,
}

thread_local! {
    static HEAP: RefCell<HeapState> = RefCell::new(HeapState {
        objects: Vec::new(),
        roots: Vec::new(),
        bytes_allocated: 0,
        next_gc: 1 << 20, // 1 MiB initial threshold
    });
}

pub fn push_root(slot: *mut *mut u8) {
    HEAP.with(|h| h.borrow_mut().roots.push(slot));
}

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

pub fn collect() {
    // Snapshot roots (no aliasing with mark/sweep)
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

unsafe fn mark_obj(obj: *mut u8) {
    unsafe {
        let hdr = obj as *mut ObjHeader;
        if (*hdr).marked != 0 {
            return;
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
