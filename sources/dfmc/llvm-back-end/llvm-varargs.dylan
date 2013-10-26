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

define method op--va-decl-start
    (back-end :: <llvm-back-end>) => (va-list :: <llvm-value>);
  // Allocate a va_list stack variable
  let (va-list-type, va-list-alignment)
    = llvm-back-end-va-list-type-alignment(back-end);
  let va-list = ins--alloca(back-end, va-list-type, i32(va-list-alignment));
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

define method op--va-list-to-stack-vector
    (back-end :: <llvm-back-end>, va-list :: <llvm-value>, count)
 => (value :: <llvm-value>);
  let module = back-end.llvm-builder-module;

  // Basic blocks
  let entry-bb = back-end.llvm-builder-basic-block;
  let loop-head-bb = make(<llvm-basic-block>);
  let loop-tail-bb = make(<llvm-basic-block>);
  let new-vector-bb = make(<llvm-basic-block>);
  let return-common-bb = make(<llvm-basic-block>);

  // Check the count to see if we want to return an empty vector
  let cmp = ins--icmp-ne(back-end, count, 0);
  ins--br(back-end, cmp, new-vector-bb, return-common-bb);

  // Allocate a new vector
  ins--block(back-end, new-vector-bb);
  let new-vector = op--stack-allocate-vector(back-end, count);
  ins--br(back-end, loop-head-bb);

  // Loop head
  ins--block(back-end, loop-head-bb);
  let index-placeholder
    = make(<llvm-symbolic-value>,
           type: back-end.%type-table["iWord"], name: "index");
  let index
    = ins--phi*(back-end, 0, new-vector-bb, index-placeholder, loop-tail-bb);
  let cmp = ins--icmp-slt(back-end, index, count);
  ins--br(back-end, cmp, loop-tail-bb, return-common-bb);

  // Loop tail: retrieve a varargs item and store it in the vector
  ins--block(back-end, loop-tail-bb);
  let arg = op--va-arg(back-end, va-list, $llvm-object-pointer-type);
  call-primitive(back-end, primitive-vector-element-setter-descriptor,
                 arg, new-vector, index);

  let next-index = ins--add(back-end, index, 1);
  index-placeholder.llvm-placeholder-value-forward := next-index;
  ins--br(back-end, loop-head-bb);

  // Return the canonical empty vector it the count was zero, or the
  // newly allocated vector if it was not
  ins--block(back-end, return-common-bb);
  let empty-vector
    = emit-reference(back-end, module, dylan-value(#"%empty-vector"));
  ins--phi*(back-end, empty-vector, entry-bb, new-vector, loop-head-bb)
end method;

define method op--va-end
    (back-end :: <llvm-back-end>, va-list :: <llvm-value>) => ();
  ins--call-intrinsic(back-end, "llvm.va_end", vector(va-list));
end method;
