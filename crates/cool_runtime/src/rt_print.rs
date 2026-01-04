// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use std::ffi::CStr;

pub fn print_cstr(s: *const libc::c_char) {
    if s.is_null() {
        eprintln!("<null>");
        return;
    }
    unsafe {
        match CStr::from_ptr(s).to_str() {
            Ok(t) => print!("{t}"),
            Err(_) => eprintln!("<invalid utf8>"),
        }
    }
}

pub fn print_int(i: i32) {
    print!("{i}");
}
