Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2013 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic llvm-back-end-va-list-type-alignment
    (back-end :: <llvm-back-end>)
 => (type :: <llvm-type>, alignment :: <integer>);

define method llvm-back-end-va-list-type-alignment
    (back-end :: <llvm-back-end>)
 => (type :: <llvm-type>, alignment :: <integer>);
  values($llvm-i8*-type, dylan-value(#"<raw-pointer>").raw-type-alignment)
end method;

// See http://www.x86-64.org/documentation/abi.pdf
define method llvm-back-end-va-list-type-alignment
    (back-end :: <llvm-x86_64-back-end>)
 => (type :: <llvm-type>, alignment :: <integer>);
  let t = back-end.%type-table;
  let va-list-struct-name = "struct.__va_list_tag";
  let struct-type
    = element(t, va-list-struct-name, default: #f)
    | (t[va-list-struct-name]
         := make(<llvm-struct-type>,
                 name: va-list-struct-name,
                 elements: vector(// gp_offset
                                  $llvm-i32-type,
                                  // fp_offset
                                  $llvm-i32-type,
                                  // overflow_arg_area
                                  $llvm-i8*-type,
                                  // reg_save_area
                                  $llvm-i8*-type)));
  let va-list-type
    = element(t, "va_list", default: #f)
    | (t["va_list"]
         := make(<llvm-array-type>, size: 1, element-type: struct-type));
  values(va-list-type, 16)
end method;

define method op--va-decl-start
    (back-end :: <llvm-back-end>) => (va-list :: <llvm-value>);
  // Allocate a va_list stack variable
  let (va-list-type, va-list-alignment)
    = llvm-back-end-va-list-type-alignment(back-end);
  let va-list = ins--alloca(back-end, va-list-type, i32(1),
                            alignment: va-list-alignment);
  let va-list-cast = ins--bitcast(back-end, va-list, $llvm-i8*-type);

  // Initialize it
  ins--call-intrinsic(back-end, "llvm.va_start", vector(va-list-cast));

  va-list-cast
end method;

// By default we assume the va_arg instruction works. For target
// platforms where this is not the case, see the definitions for the
// EmitVAArg method in clang's lib/CodeGen/TargetInfo.cpp
define method op--va-arg
    (back-end :: <llvm-back-end>, va-list :: <llvm-value>, type :: <llvm-type>)
 => (value :: <llvm-value>);
  ins--va-arg(back-end, va-list, type)
end method;

define method op--va-arg
    (back-end :: <llvm-x86_64-back-end>, va-list :: <llvm-value>,
     type :: <llvm-pointer-type>)
 => (value :: <llvm-value>);
  let va-list-type = back-end.%type-table["va_list"];
  let ap = ins--bitcast(back-end, va-list,
                        llvm-pointer-to(back-end, va-list-type));

  // Read gp_offset
  let gp-offset-p = ins--gep(back-end, ap, 0, 0, i32(0));
  let gp-offset = ins--load(back-end, gp-offset-p, alignment: 16);

  // Is this arg in the reg save area? (step 3)
  let cmp = ins--icmp-ule(back-end, gp-offset, i32(48 - 8 * 1));
  ins--if (back-end, cmp)
    // Read reg_save_area (step 4)
    let reg-save-area-p = ins--gep(back-end, ap, 0, 0, i32(3));
    let reg-save-area = ins--load(back-end, reg-save-area-p, alignment: 16);

    // Load argument
    let gp-offset-zext = ins--zext(back-end, gp-offset, $llvm-i64-type);
    let arg-p = ins--gep(back-end, reg-save-area, gp-offset-zext);
    let arg-p-cast = ins--bitcast(back-end, arg-p,
                                  llvm-pointer-to(back-end, type));
    let arg = ins--load(back-end, arg-p-cast, alignment: 8);

    // Update gp_offset (step 5)
    let gp-offset-new = ins--add(back-end, gp-offset, i32(8));
    ins--store(back-end, gp-offset-new, gp-offset-p);

    // Return fetched value (step 6)
    arg
  ins--else
    // Read overflow_arg_area (step 8)
    let overflow-arg-area-p = ins--gep(back-end, ap, 0, 0, i32(2));
    let overflow-arg-area
      = ins--load(back-end, overflow-arg-area-p, alignment: 8);

    // Load argument
    let arg-p-cast = ins--bitcast(back-end, overflow-arg-area,
                                  llvm-pointer-to(back-end, type));
    let arg = ins--load(back-end, arg-p-cast, alignment: 8);

    // Update overflow_arg_area (step 9)
    let overflow-arg-area-new = ins--gep(back-end, overflow-arg-area, 8);
    ins--store(back-end, overflow-arg-area-new, overflow-arg-area-p);

    // Return fetched value (step 11)
    arg
  end ins--if;
end method;

define method op--va-list-to-stack-vector
    (back-end :: <llvm-back-end>, va-list :: <llvm-value>, count)
 => (value :: <llvm-value>);
  let module = back-end.llvm-builder-module;

  // Check the count to see if we want to return an empty vector
  let cmp = ins--icmp-ne(back-end, count, 0);
  ins--if (back-end, cmp)
    let new-vector = op--stack-allocate-vector(back-end, count);
    ins--iterate loop (back-end, index = 0)
      let cmp = ins--icmp-slt(back-end, index, count);
      ins--if (back-end, cmp)
        // Retrieve a varargs item and store it in the vector
        let arg = op--va-arg(back-end, va-list, $llvm-object-pointer-type);
        call-primitive(back-end, primitive-vector-element-setter-descriptor,
                       arg, new-vector, index);
        loop(ins--add(back-end, index, 1));
      ins--else
        new-vector
      end ins--if;
    end ins--iterate;
  ins--else
    // Return the canonical empty vector it the count was zero
    emit-reference(back-end, module, dylan-value(#"%empty-vector"))
  end ins--if
end method;

define method op--va-end
    (back-end :: <llvm-back-end>, va-list :: <llvm-value>) => ();
  ins--call-intrinsic(back-end, "llvm.va_end", vector(va-list));
end method;
