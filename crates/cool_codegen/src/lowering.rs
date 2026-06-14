// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

//! AST → LLVM IR lowering.
//!
//! This module is where abstract COOL semantics become concrete machine-level
//! representations. Two ideas drive everything here:
//!
//! ## 1. The object model (boxing)
//!
//! Every COOL value is an *object* on the heap, laid out as a common
//! [`ObjHeader`](cool_runtime::ObjHeader) followed by the type's own fields:
//!
//! ```text
//!   Object in memory:        Int object:            Main object:
//!   ┌──────────────┐         ┌──────────────┐       ┌──────────────┐
//!   │ vtable ptr   │         │ header       │       │ header       │
//!   │ size_bytes   │         │ value: i32   │       └──────────────┘
//!   │ marked  (gc) │         └──────────────┘
//!   │ tag          │
//!   └──────────────┘
//! ```
//!
//! Even an integer is "boxed": the literal `7` becomes a heap `Int` whose
//! payload field holds the machine `i32`. [`codegen_int_expr`] computes the raw
//! `i32`; [`define_main_main`] wraps it in an `Int` object.
//!
//! ## 2. Dynamic dispatch (vtables)
//!
//! The first word of every object points at its class's *vtable* — a constant
//! table of metadata (tag, size, GC pointer-offset map) followed by the class's
//! method pointers. A method call loads the vtable, indexes the right slot, and
//! calls through the function pointer. [`define_c_main`] demonstrates this by
//! dispatching `Main.main` through `vtable_Main` slot 0 rather than calling it
//! by name.
//!
//! ## Current scope
//!
//! Lowering is hardcoded for the demo shape `class Main { main() : Int { <int> } }`.
//! Generalizing this walk to arbitrary classes, attributes, and the full `Expr`
//! set is the project's next milestone.

use crate::CodegenError;
use crate::abi;
use cool_frontend::{BinOp, Expr, Feature, Program};
use inkwell::values::ValueKind;
use inkwell::{
    AddressSpace,
    builder::Builder,
    context::Context,
    module::Module,
    types::{FunctionType, StructType},
    values::{BasicMetadataValueEnum, FunctionValue, GlobalValue, PointerValue},
};

/// Lower a whole program into `module`'s IR.
///
/// The sequence here is the skeleton every COOL backend needs:
/// 1. declare the runtime functions we'll call (`cool_alloc`, `cool_rt_print_int`);
/// 2. define the LLVM struct types for the object header and each class;
/// 3. emit each class's constant vtable as an LLVM global;
/// 4. define the method bodies (`Main_main`) and the C `main` entry point.
pub fn lower_program<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    p: &Program,
) -> Result<(), CodegenError> {
    let main_body = find_main_main_body(p)?;

    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();
    let void_ty = ctx.void_type();

    // Since LLVM 15, all pointers are *opaque* — a single `ptr` type with no
    // pointee, regardless of what it points at. We reuse this one `optr_ty`
    // everywhere a pointer is needed and rely on `struct_gep` with an explicit
    // struct type when we need to compute field addresses.
    let optr_ty = ctx.ptr_type(AddressSpace::default());

    // --- Declare the runtime functions we will call ---
    // These are defined in `cool_runtime` and resolved at link time. We only
    // declare their *signatures* here so the IR can reference them.
    //   cool_alloc(tag: i32, size_bytes: i32, vtable: ptr) -> ptr
    let alloc_fn_ty = optr_ty.fn_type(&[i32_ty.into(), i32_ty.into(), optr_ty.into()], false);
    let alloc_fn = module.add_function(abi::FN_ALLOC, alloc_fn_ty, None);

    //   cool_rt_print_int(i: i32) -> void
    let print_int_fn_ty = void_ty.fn_type(&[i32_ty.into()], false);
    let print_int_fn = module.add_function(abi::FN_PRINT_INT, print_int_fn_ty, None);

    // --- Object header struct, mirroring `cool_runtime::ObjHeader` exactly ---
    // The layout (and the explicit 3-byte pad after the `marked` bool) must
    // match the runtime's `#[repr(C)]` struct, or the GC and generated code
    // would disagree about where each field lives. See `abi.rs` for the offsets.
    //   { vtable: ptr, size_bytes: i32, marked: i8, pad: [3 x i8], tag: i32 }
    let obj_header_ty = ctx.struct_type(
        &[
            optr_ty.into(),
            i32_ty.into(),
            i8_ty.into(),
            i8_ty.array_type(3).into(),
            i32_ty.into(),
        ],
        false,
    );

    // Per-class object types: the shared header followed by that class's fields.
    // `Int` adds a single payload `i32`; `Main` (in this demo) adds nothing.
    let int_obj_ty = ctx.struct_type(&[obj_header_ty.into(), i32_ty.into()], false);
    let main_obj_ty = ctx.struct_type(&[obj_header_ty.into()], false);

    // --- vtable struct types ---
    // A vtable starts with three i32s and a pointer that mirror the runtime's
    // `VTable` { tag, obj_size, ptr_count, ptr_offsets } (the GC reads these to
    // find pointer fields), then appends one function pointer per method. `Int`
    // has no methods here; `Main` has one (`main`) in trailing slot 0.
    let i32_ptr_ty = ctx.ptr_type(AddressSpace::default());

    let vtable_int_ty = ctx.struct_type(
        &[
            i32_ty.into(),
            i32_ty.into(),
            i32_ty.into(),
            i32_ptr_ty.into(),
        ],
        false,
    );

    let vtable_main_ty = ctx.struct_type(
        &[
            i32_ty.into(),
            i32_ty.into(),
            i32_ty.into(),
            i32_ptr_ty.into(),
            optr_ty.into(), // slot0 fn ptr
        ],
        false,
    );

    let null_i32ptr = i32_ptr_ty.const_null();

    // Sizes
    let int_size_bytes = int_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Int size".into()))?
        .const_truncate(i32_ty);

    let main_size_bytes = main_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Main size".into()))?
        .const_truncate(i32_ty);

    // Declare Main_main(self: ptr) -> ptr
    let main_main_fn_ty = optr_ty.fn_type(&[optr_ty.into()], false);
    let main_main_fn = module.add_function("Main_main", main_main_fn_ty, None);

    // vtables
    let vt_int = module.add_global(vtable_int_ty, None, "vtable_Int");
    vt_int.set_constant(true);
    vt_int.set_initializer(&vtable_int_ty.const_named_struct(&[
        i32_ty.const_int(abi::TAG_INT as u64, false).into(),
        int_size_bytes.into(),
        i32_ty.const_zero().into(),
        null_i32ptr.into(),
    ]));

    let vt_main = module.add_global(vtable_main_ty, None, "vtable_Main");
    vt_main.set_constant(true);

    let main_main_ptr = main_main_fn.as_global_value().as_pointer_value();
    vt_main.set_initializer(&vtable_main_ty.const_named_struct(&[
        i32_ty.const_int(100, false).into(), // TODO: real tags later
        main_size_bytes.into(),
        i32_ty.const_zero().into(),
        null_i32ptr.into(),
        main_main_ptr.const_cast(optr_ty).into(),
    ]));

    define_main_main(
        ctx,
        &main_main_fn,
        &obj_header_ty,
        &int_obj_ty,
        &vt_int,
        &alloc_fn,
        &main_body,
    )?;

    define_c_main(
        ctx,
        module,
        &main_obj_ty,
        &obj_header_ty,
        &int_obj_ty,
        &vtable_main_ty,
        &vt_main,
        &alloc_fn,
        &print_int_fn,
    )?;

    Ok(())
}

/// Locate the body expression of `Main.main` — the program's entry point per
/// the COOL spec. Returns a structured error if either is missing.
fn find_main_main_body(p: &Program) -> Result<Expr, CodegenError> {
    let main_class = p
        .classes
        .iter()
        .find(|c| c.name == "Main")
        .ok_or(CodegenError::MissingMainClass)?;

    for f in &main_class.features {
        match f {
            Feature::Method {
                name,
                formals: _,
                ret_type: _,
                body,
            } if name == "main" => return Ok(body.clone()),
            _ => {}
        }
    }
    Err(CodegenError::MissingMainMethod)
}

/// Emit a direct call to `f` and extract its return value as a pointer.
///
/// LLVM calls can return void, so `try_as_basic_value` yields either a value or
/// a bare instruction; we treat the void case as an error since every caller
/// here expects an object pointer back (e.g. from `cool_alloc`).
fn call_returns_ptr_fn<'ctx>(
    builder: &Builder<'ctx>,
    f: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    name: &str,
) -> Result<PointerValue<'ctx>, CodegenError> {
    let call = builder.build_call(f, args, name)?;

    match call.try_as_basic_value() {
        ValueKind::Basic(v) => Ok(v.into_pointer_value()),
        ValueKind::Instruction(_) => {
            Err(CodegenError::Llvm(format!("call `{}` returned void", name)))
        }
    }
}

/// Like [`call_returns_ptr_fn`], but for an *indirect* call through a function
/// pointer loaded at runtime — this is how dynamic dispatch through a vtable
/// slot works. `fn_ty` tells LLVM how to interpret the otherwise-opaque pointer.
fn call_returns_ptr_indirect<'ctx>(
    builder: &Builder<'ctx>,
    fn_ty: FunctionType<'ctx>,
    fptr: PointerValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    name: &str,
) -> Result<PointerValue<'ctx>, CodegenError> {
    let call = builder.build_indirect_call(fn_ty, fptr, args, name)?;

    match call.try_as_basic_value() {
        ValueKind::Basic(v) => Ok(v.into_pointer_value()),
        ValueKind::Instruction(_) => Err(CodegenError::Llvm(format!(
            "indirect call `{}` returned void",
            name
        ))),
    }
}

/// Emit the body of `Main_main(self) -> ptr`.
///
/// In plain terms: evaluate the method's integer expression to a raw `i32`, then
/// **box** it — allocate an `Int` object via `cool_alloc`, fill in its header
/// (vtable/size/marked/tag) and payload, and return the pointer. The header is
/// written field-by-field with `build_struct_gep` (get-element-pointer), which
/// computes the address of a struct field given the struct type and field index.
fn define_main_main<'ctx>(
    ctx: &'ctx Context,
    f: &FunctionValue<'ctx>,
    obj_header_ty: &StructType<'ctx>,
    int_obj_ty: &StructType<'ctx>,
    vt_int: &GlobalValue<'ctx>,
    alloc_fn: &FunctionValue<'ctx>,
    body: &Expr,
) -> Result<(), CodegenError> {
    let builder = ctx.create_builder();
    // A function's IR lives in "basic blocks"; we create the entry block and
    // point the builder at its end so subsequent build_* calls append into it.
    let entry = ctx.append_basic_block(*f, "entry");
    builder.position_at_end(entry);

    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();
    let optr_ty = ctx.ptr_type(AddressSpace::default());

    let val_i32 = codegen_int_expr(ctx, &builder, body)?;

    // Ask the runtime to allocate an Int object: cool_alloc(tag, size, vtable).
    // The explicit element type keeps inference happy across the `.into()`s.
    let int_size = int_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Int size".into()))?
        .const_truncate(i32_ty);
    let alloc_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![
        i32_ty.const_int(abi::TAG_INT as u64, false).into(),
        int_size.into(),
        vt_int.as_pointer_value().const_cast(optr_ty).into(),
    ];

    let boxed_ptr = call_returns_ptr_fn(&builder, *alloc_fn, &alloc_args, "int_alloc")?;

    // typed cast for struct_gep (this triggers a deprecation warning; ok)
    let int_ptr = builder.build_pointer_cast(boxed_ptr, optr_ty, "int_ptr")?;

    // header = field 0
    let hdr_gep = builder.build_struct_gep(*int_obj_ty, int_ptr, 0, "hdr_gep")?;

    let hdr_ptr = builder.build_pointer_cast(hdr_gep, optr_ty, "hdr_ptr")?;

    // header.vtable = field 0
    let vtable_gep = builder.build_struct_gep(*obj_header_ty, hdr_ptr, 0, "hdr_vtable")?;
    builder.build_store(vtable_gep, vt_int.as_pointer_value().const_cast(optr_ty))?;

    // header.size_bytes = field 1
    let size_gep = builder.build_struct_gep(*obj_header_ty, hdr_ptr, 1, "hdr_size")?;
    let size_i32 = int_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Int size".into()))?
        .const_truncate(i32_ty);
    builder.build_store(size_gep, size_i32)?;

    // header.marked = field 2
    let marked_gep = builder.build_struct_gep(*obj_header_ty, hdr_ptr, 2, "hdr_marked")?;
    builder.build_store(marked_gep, i8_ty.const_zero())?;

    // header.tag = field 4
    let tag_gep = builder.build_struct_gep(*obj_header_ty, hdr_ptr, 4, "hdr_tag")?;
    builder.build_store(tag_gep, i32_ty.const_int(abi::TAG_INT as u64, false))?;

    // int.value = field 1
    let val_gep = builder.build_struct_gep(*int_obj_ty, int_ptr, 1, "int_val")?;
    builder.build_store(val_gep, val_i32)?;

    builder.build_return(Some(&boxed_ptr))?;

    Ok(())
}

/// Emit the C-ABI `main` — the actual process entry point the OS/loader calls.
///
/// It performs the program's bootstrap and demonstrates dynamic dispatch:
/// 1. allocate the `Main` object;
/// 2. load its vtable pointer from the object header;
/// 3. read the `main` method pointer out of vtable slot 0;
/// 4. call it *indirectly* (this is dynamic dispatch — we never name `Main_main`);
/// 5. unbox the returned `Int` and hand its `i32` to `cool_rt_print_int`;
/// 6. return 0.
///
/// Going through the vtable instead of a direct call is deliberate: it's the
/// same mechanism that will make overridden methods resolve correctly once
/// lowering is generalized.
//
// The long parameter list threads the LLVM types/globals built once in
// `lower_program`. When lowering grows beyond this demo, these should be bundled
// into a "codegen context" struct; for now an allow keeps the demo readable.
#[allow(clippy::too_many_arguments)]
fn define_c_main<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    main_obj_ty: &StructType<'ctx>,
    obj_header_ty: &StructType<'ctx>,
    int_obj_ty: &StructType<'ctx>,
    vtable_main_ty: &StructType<'ctx>,
    vt_main: &GlobalValue<'ctx>,
    alloc_fn: &FunctionValue<'ctx>,
    print_int_fn: &FunctionValue<'ctx>,
) -> Result<(), CodegenError> {
    let builder = ctx.create_builder();
    let i32_ty = ctx.i32_type();
    let optr_ty = ctx.ptr_type(AddressSpace::default());

    let c_main_ty = i32_ty.fn_type(&[], false);
    let c_main = module.add_function("main", c_main_ty, None);
    let entry = ctx.append_basic_block(c_main, "entry");
    builder.position_at_end(entry);

    // Allocate the Main object. (The tag 100 is a placeholder; real class tags
    // will be assigned once lowering is generalized — see the vtable TODO above.)
    let main_size = main_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Main size".into()))?
        .const_truncate(i32_ty);
    let alloc_args: Vec<BasicMetadataValueEnum<'ctx>> = vec![
        i32_ty.const_int(100, false).into(),
        main_size.into(),
        vt_main.as_pointer_value().const_cast(optr_ty).into(),
    ];

    let main_obj = call_returns_ptr_fn(&builder, *alloc_fn, &alloc_args, "main_alloc")?;

    // cast to Main* for struct gep
    let main_ptr = builder.build_pointer_cast(main_obj, optr_ty, "main_ptr")?;

    // header ptr
    let hdr_gep = builder.build_struct_gep(*main_obj_ty, main_ptr, 0, "main_hdr_gep")?;
    let hdr_ptr = builder.build_pointer_cast(hdr_gep, optr_ty, "main_hdr_ptr")?;

    // load header.vtable
    let vtable_gep = builder.build_struct_gep(*obj_header_ty, hdr_ptr, 0, "main_vtable_gep")?;

    let vtable_loaded = builder.build_load(optr_ty, vtable_gep, "vtable_loaded")?;

    let vt_main_ptr =
        builder.build_pointer_cast(vtable_loaded.into_pointer_value(), optr_ty, "vt_main_ptr")?;

    // slot0: field 4
    let slot0_gep = builder.build_struct_gep(*vtable_main_ty, vt_main_ptr, 4, "slot0_gep")?;

    let slot0 = builder.build_load(optr_ty, slot0_gep, "slot0")?;

    // cast to fn(ptr)->ptr and call indirectly
    let method_fn_ty = optr_ty.fn_type(&[optr_ty.into()], false);
    let method_ptr_ty = optr_ty;
    let slot0_typed =
        builder.build_pointer_cast(slot0.into_pointer_value(), method_ptr_ty, "slot0_typed")?;

    let boxed_int = call_returns_ptr_indirect(
        &builder,
        method_fn_ty,
        slot0_typed,
        &[main_obj.into()],
        "call_main_main",
    )?;

    // unbox
    let int_ptr = builder.build_pointer_cast(boxed_int, optr_ty, "int_ptr")?;

    let val_gep = builder.build_struct_gep(*int_obj_ty, int_ptr, 1, "int_val_gep")?;

    let val_i32 = builder
        .build_load(i32_ty, val_gep, "int_val")?
        .into_int_value();

    builder.build_call(*print_int_fn, &[val_i32.into()], "print")?;

    builder.build_return(Some(&i32_ty.const_zero()))?;

    Ok(())
}

/// Lower an integer-valued expression to a raw (unboxed) LLVM `i32`.
///
/// This is the leaf of the current codegen subset: it recursively emits the
/// arithmetic tree directly as LLVM integer instructions. Note it produces a
/// *machine* `i32`, not a COOL `Int` object — `define_main_main` is responsible
/// for boxing the final result into a heap `Int` before returning it.
///
/// `build_int_add` and friends return `Result<_, BuilderError>`; the trailing
/// `?` converts that into our `CodegenError` via the `From` impl in `lib.rs`,
/// and the surrounding `Ok(...)` re-wraps the success value.
fn codegen_int_expr<'ctx>(
    ctx: &'ctx Context,
    builder: &Builder<'ctx>,
    e: &Expr,
) -> Result<inkwell::values::IntValue<'ctx>, CodegenError> {
    let i32_ty = ctx.i32_type();

    match e {
        Expr::Int(n) => Ok(i32_ty.const_int(*n as u64, true)),
        Expr::Bin { op, lhs, rhs } => {
            let a = codegen_int_expr(ctx, builder, lhs)?;
            let b = codegen_int_expr(ctx, builder, rhs)?;
            Ok(match op {
                BinOp::Add => builder.build_int_add(a, b, "add")?,
                BinOp::Sub => builder.build_int_sub(a, b, "sub")?,
                BinOp::Mul => builder.build_int_mul(a, b, "mul")?,
                BinOp::Div => builder.build_int_signed_div(a, b, "div")?,
                _ => {
                    return Err(CodegenError::Unsupported(
                        "non-arithmetic operator not supported yet",
                    ));
                }
            })
        }
        _ => Err(CodegenError::Unsupported(
            "expression not supported in current codegen subset",
        )),
    }
}
