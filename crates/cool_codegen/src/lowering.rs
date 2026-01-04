// Copyright 2025 Diivanand Ramalingam
// Licensed under the Apache License, Version 2.0

use crate::abi;
use crate::CodegenError;
use cool_frontend::{BinOp, Expr, Feature, Program};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{FunctionType, StructType},
    values::{BasicMetadataValueEnum, FunctionValue, GlobalValue, PointerValue},
    AddressSpace,
};
use inkwell::values::ValueKind;

pub fn lower_program<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    p: &Program,
) -> Result<(), CodegenError> {
    let main_body = find_main_main_body(p)?;

    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();
    let void_ty = ctx.void_type();

    // Opaque pointer (LLVM 15+)
    let optr_ty = ctx.ptr_type(AddressSpace::default());

    // --- Runtime decls ---
    let alloc_fn_ty = optr_ty.fn_type(&[i32_ty.into(), i32_ty.into(), optr_ty.into()], false);
    let alloc_fn = module.add_function(abi::FN_ALLOC, alloc_fn_ty, None);

    let print_int_fn_ty = void_ty.fn_type(&[i32_ty.into()], false);
    let print_int_fn = module.add_function(abi::FN_PRINT_INT, print_int_fn_ty, None);

    // --- ObjHeader layout ---
    // vtable: ptr
    // size_bytes: i32
    // marked: i8
    // pad: [3 x i8]
    // tag: i32
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

    // Int: { header, i32 }
    let int_obj_ty = ctx.struct_type(&[obj_header_ty.into(), i32_ty.into()], false);

    // Main: { header }
    let main_obj_ty = ctx.struct_type(&[obj_header_ty.into()], false);

    // vtable structs (compiler convenience)
    let i32_ptr_ty = i32_ty.ptr_type(AddressSpace::default());

    let vtable_int_ty =
        ctx.struct_type(&[i32_ty.into(), i32_ty.into(), i32_ty.into(), i32_ptr_ty.into()], false);

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

fn call_returns_ptr_fn<'ctx>(
    builder: &Builder<'ctx>,
    f: FunctionValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    name: &str,
) -> Result<PointerValue<'ctx>, CodegenError> {
    let call = builder
        .build_call(f, args, name)
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    match call.try_as_basic_value() {
        ValueKind::Basic(v) => Ok(v.into_pointer_value()),
        ValueKind::Instruction(_) => Err(CodegenError::Llvm(format!(
            "call `{}` returned void",
            name
        ))),
    }
}

fn call_returns_ptr_indirect<'ctx>(
    builder: &Builder<'ctx>,
    fn_ty: FunctionType<'ctx>,
    fptr: PointerValue<'ctx>,
    args: &[BasicMetadataValueEnum<'ctx>],
    name: &str,
) -> Result<PointerValue<'ctx>, CodegenError> {
    let call = builder
        .build_indirect_call(fn_ty, fptr, args, name)
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    match call.try_as_basic_value() {
        ValueKind::Basic(v) => Ok(v.into_pointer_value()),
        ValueKind::Instruction(_) => Err(CodegenError::Llvm(format!(
            "indirect call `{}` returned void",
            name
        ))),
    }
}

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
    let entry = ctx.append_basic_block(*f, "entry");
    builder.position_at_end(entry);

    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();
    let optr_ty = ctx.ptr_type(AddressSpace::default());

    let val_i32 = codegen_int_expr(ctx, &builder, body)?;

    // Use an explicit Vec<BasicMetadataValueEnum> to avoid inference mismatches.
    let mut alloc_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
    alloc_args.push(i32_ty.const_int(abi::TAG_INT as u64, false).into());
    alloc_args.push(
        int_obj_ty
            .size_of()
            .ok_or_else(|| CodegenError::Llvm("could not compute Int size".into()))?
            .const_truncate(i32_ty)
            .into(),
    );
    alloc_args.push(vt_int.as_pointer_value().const_cast(optr_ty).into());

    let boxed_ptr = call_returns_ptr_fn(&builder, *alloc_fn, &alloc_args, "int_alloc")?;

    // typed cast for struct_gep (this triggers a deprecation warning; ok)
    let int_ptr = builder
        .build_pointer_cast(
            boxed_ptr,
            int_obj_ty.ptr_type(AddressSpace::default()),
            "int_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header = field 0
    let hdr_gep = unsafe {
        builder
            .build_struct_gep(*int_obj_ty, int_ptr, 0, "hdr_gep")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };

    let hdr_ptr = builder
        .build_pointer_cast(
            hdr_gep,
            obj_header_ty.ptr_type(AddressSpace::default()),
            "hdr_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header.vtable = field 0
    let vtable_gep = unsafe {
        builder
            .build_struct_gep(*obj_header_ty, hdr_ptr, 0, "hdr_vtable")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    builder
        .build_store(vtable_gep, vt_int.as_pointer_value().const_cast(optr_ty))
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header.size_bytes = field 1
    let size_gep = unsafe {
        builder
            .build_struct_gep(*obj_header_ty, hdr_ptr, 1, "hdr_size")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    let size_i32 = int_obj_ty
        .size_of()
        .ok_or_else(|| CodegenError::Llvm("could not compute Int size".into()))?
        .const_truncate(i32_ty);
    builder
        .build_store(size_gep, size_i32)
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header.marked = field 2
    let marked_gep = unsafe {
        builder
            .build_struct_gep(*obj_header_ty, hdr_ptr, 2, "hdr_marked")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    builder
        .build_store(marked_gep, i8_ty.const_zero())
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header.tag = field 4
    let tag_gep = unsafe {
        builder
            .build_struct_gep(*obj_header_ty, hdr_ptr, 4, "hdr_tag")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    builder
        .build_store(tag_gep, i32_ty.const_int(abi::TAG_INT as u64, false))
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // int.value = field 1
    let val_gep = unsafe {
        builder
            .build_struct_gep(*int_obj_ty, int_ptr, 1, "int_val")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    builder
        .build_store(val_gep, val_i32)
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    builder
        .build_return(Some(&boxed_ptr))
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    Ok(())
}

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

    // Use an explicit Vec<BasicMetadataValueEnum> to avoid inference mismatches.
    let mut alloc_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
    alloc_args.push(i32_ty.const_int(100, false).into());
    alloc_args.push(
        main_obj_ty
            .size_of()
            .ok_or_else(|| CodegenError::Llvm("could not compute Main size".into()))?
            .const_truncate(i32_ty)
            .into(),
    );
    alloc_args.push(vt_main.as_pointer_value().const_cast(optr_ty).into());

    let main_obj = call_returns_ptr_fn(&builder, *alloc_fn, &alloc_args, "main_alloc")?;

    // cast to Main* for struct gep
    let main_ptr = builder
        .build_pointer_cast(
            main_obj,
            main_obj_ty.ptr_type(AddressSpace::default()),
            "main_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // header ptr
    let hdr_gep = unsafe {
        builder
            .build_struct_gep(*main_obj_ty, main_ptr, 0, "main_hdr_gep")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };
    let hdr_ptr = builder
        .build_pointer_cast(
            hdr_gep,
            obj_header_ty.ptr_type(AddressSpace::default()),
            "main_hdr_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // load header.vtable
    let vtable_gep = unsafe {
        builder
            .build_struct_gep(*obj_header_ty, hdr_ptr, 0, "main_vtable_gep")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };

    let vtable_loaded = builder
        .build_load(optr_ty, vtable_gep, "vtable_loaded")
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    let vt_main_ptr = builder
        .build_pointer_cast(
            vtable_loaded.into_pointer_value(),
            vtable_main_ty.ptr_type(AddressSpace::default()),
            "vt_main_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // slot0: field 4
    let slot0_gep = unsafe {
        builder
            .build_struct_gep(*vtable_main_ty, vt_main_ptr, 4, "slot0_gep")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };

    let slot0 = builder
        .build_load(optr_ty, slot0_gep, "slot0")
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    // cast to fn(ptr)->ptr and call indirectly
    let method_fn_ty = optr_ty.fn_type(&[optr_ty.into()], false);
    let method_ptr_ty = method_fn_ty.ptr_type(AddressSpace::default());
    let slot0_typed = builder
        .build_pointer_cast(slot0.into_pointer_value(), method_ptr_ty, "slot0_typed")
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    let boxed_int = call_returns_ptr_indirect(
        &builder,
        method_fn_ty,
        slot0_typed,
        &[main_obj.into()],
        "call_main_main",
    )?;

    // unbox
    let int_ptr = builder
        .build_pointer_cast(
            boxed_int,
            int_obj_ty.ptr_type(AddressSpace::default()),
            "int_ptr",
        )
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    let val_gep = unsafe {
        builder
            .build_struct_gep(*int_obj_ty, int_ptr, 1, "int_val_gep")
            .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
    };

    let val_i32 = builder
        .build_load(i32_ty, val_gep, "int_val")
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?
        .into_int_value();

    builder
        .build_call(*print_int_fn, &[val_i32.into()], "print")
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    builder
        .build_return(Some(&i32_ty.const_zero()))
        .map_err(|e| CodegenError::Llvm(format!("{e:?}")))?;

    Ok(())
}

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
            match op {
                BinOp::Add => builder
                    .build_int_add(a, b, "add")
                    .map_err(|e| CodegenError::Llvm(format!("{e:?}"))),
                BinOp::Sub => builder
                    .build_int_sub(a, b, "sub")
                    .map_err(|e| CodegenError::Llvm(format!("{e:?}"))),
                BinOp::Mul => builder
                    .build_int_mul(a, b, "mul")
                    .map_err(|e| CodegenError::Llvm(format!("{e:?}"))),
                BinOp::Div => builder
                    .build_int_signed_div(a, b, "div")
                    .map_err(|e| CodegenError::Llvm(format!("{e:?}"))),
                _ => Err(CodegenError::Unsupported(
                    "non-arithmetic operator not supported yet",
                )),
            }
        }
        _ => Err(CodegenError::Unsupported(
            "expression not supported in current codegen subset",
        )),
    }
}
