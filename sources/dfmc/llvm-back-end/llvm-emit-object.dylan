Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// emit-name support

define sealed method string-emitter
    (back-end :: <llvm-back-end>, stream, name :: <byte-string>)
  name
end method;

define method emit-name-internal
    (back-end :: <llvm-back-end>, m, o :: <module-binding>)
 => (name :: <string>);
  global-mangle(back-end, o)
end method;


/// References

define constant $dylan-tag-bits = 2;

define constant $dylan-tag-pointer   = 0;
define constant $dylan-tag-integer   = 1;
define constant $dylan-tag-character = 2;
define constant $dylan-tag-unichar   = 3; // FIXME

// Generic

define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o)
 => (reference :: <llvm-constant-value>)
  if (direct-object?(o))
    emit-object(back-end, m, o)
  else
    assert($dylan-tag-pointer == 0);
    // Memoized reference constant
    let name = emit-name(back-end, m, o);
    element(back-end.%reference-table, name, default: #f)
      | (element(back-end.%reference-table, name)
           := make(<llvm-cast-constant>,
                   operator: #"BITCAST",
                   type: $llvm-object-pointer-type,
                   operands: vector(llvm-builder-global(back-end, name))))
  end if
end method;

define method emit-wrapper-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&mm-wrapper>)
 => (reference :: <llvm-constant-value>)
  let wrapper-name = emit-name(back-end, m, o);
  let mm-w = dylan-value(#"<mm-wrapper>");
  make(<llvm-cast-constant>,
       operator: #"BITCAST",
       type: llvm-pointer-to(back-end, llvm-class-type(back-end, mm-w)),
       operands: vector(llvm-builder-global(back-end, wrapper-name)))
end method;

// Raw type references are replaced by references to a "static type"
// type marker. This is currently just <object>.
define method emit-reference
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&raw-type>)
 => (reference :: <llvm-constant-value>)
  emit-reference(back-end, m, dylan-value(#"<object>"))
end method;


/// Direct objects

// Raw objects

define method emit-object
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&raw-single-float>)
 => (reference :: <llvm-constant-value>)
  make(<llvm-float-constant>,
       type: llvm-reference-type(back-end, dylan-value(#"<raw-single-float>")),
       float: o.^raw-object-value);
end method;

define method emit-object
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&raw-double-float>)
 => (reference :: <llvm-constant-value>)
  make(<llvm-float-constant>,
       type: llvm-reference-type(back-end, dylan-value(#"<raw-double-float>")),
       float: o.^raw-object-value);
end method;

// Integral raw objects
define method emit-object
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <&raw-object>)
 => (reference :: <llvm-constant-value>)
  let value = coerce-machine-word-to-an-integer(o.^raw-object-value);
  element(back-end.%raw-object-table, value, default: #f)
    | begin
        let type = llvm-reference-type(back-end, o.^object-class);
        let constant
          = make(<llvm-integer-constant>, type: type, integer: value);
        // Only memoize word-sized raw objects
        if (type == back-end.%type-table["iWord"])
          element(back-end.%raw-object-table, value) := constant;
        end if;
        constant
      end
end method;

define function coerce-machine-word-to-an-integer
    (word) => (object :: <abstract-integer>)
  select (word by instance?)
    <abstract-integer> =>
      word;
    <machine-word> =>
      make(<double-integer>,
           high: if (negative?(word))
                   $maximum-unsigned-machine-word
                 else
                   $minimum-unsigned-machine-word
                 end if,
           low: word);
  end
end function;

// Integers

define method emit-object
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <abstract-integer>)
 => (reference :: <llvm-constant-value>);
  op--tag-integer(back-end, o)
end method;

// Characters

define method emit-object
    (back-end :: <llvm-back-end>, m :: <llvm-module>, o :: <character>)
 => (reference :: <llvm-constant-value>)
  op--tag-character(back-end, o)
end method;

// Raw byte characters

define method llvm-raw-byte-character
    (back-end :: <llvm-back-end>, c :: <byte-character>)
 => (result :: <llvm-constant-value>)
  back-end.%byte-character-constants[as(<integer>, c)]
end method;
