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
