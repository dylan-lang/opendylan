Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// LLVM type for object pointers, and for tagged direct objects
// (integers and characters)
define constant $llvm-object-pointer-type :: <llvm-type> = $llvm-i8*-type;

// "Don't care" placeholder value of that type
define constant $object-pointer-undef
  = make(<llvm-undef-constant>, type: $llvm-object-pointer-type);


/// Memoized pointer types

define inline method llvm-pointer-to
    (back-end :: <llvm-back-end>, type :: <llvm-type>)
 => (pointer-type :: <llvm-pointer-type>);
  let type = llvm-type-forward(type);
  element(back-end.%pointer-to-table, type, default: #f)
    | (element(back-end.%pointer-to-table, type)
         := make(<llvm-pointer-type>, pointee: type))
end method;


/// Built-in types

define method initialize-type-table
    (back-end :: <llvm-back-end>) => ()
  let t = back-end.%type-table;

  // Word-size integer
  t["iWord"]
    := make(<llvm-integer-type>, width: back-end-word-size(back-end) * 8);

  // Double-word-size integer
  t["iDoubleWord"]
    := make(<llvm-integer-type>, width: back-end-word-size(back-end) * 8 * 2);

  // Raw types
  local
    method register-raw-type
        (type-name :: <symbol>,
         type :: <llvm-type>,
         dbg-encoding :: false-or(<symbol>))
     => ();
      let raw-type = dylan-value(type-name);
      back-end.%raw-type-table[raw-type] := type;
      back-end.%raw-type-dbg-encoding-table[raw-type] := dbg-encoding;
    end method;

  // FIXME
  let llvm-long-double-type = make(<llvm-primitive-type>, kind: #"X86_FP80");

  register-raw-type(#"<raw-c-signed-char>",        $llvm-i8-type,
                    #"signed-char");
  register-raw-type(#"<raw-c-unsigned-char>",      $llvm-i8-type,
                    #"unsigned-char");
  register-raw-type(#"<raw-c-signed-short>",       $llvm-i16-type,
                    #"signed");
  register-raw-type(#"<raw-c-unsigned-short>",     $llvm-i16-type,
                    #"unsigned");
  register-raw-type(#"<raw-c-signed-int>",         $llvm-i32-type,
                    #"signed");
  register-raw-type(#"<raw-c-unsigned-int>",       $llvm-i32-type,
                    #"unsigned");
  register-raw-type(#"<raw-c-signed-long>",        t["iWord"],
                    #"signed");
  register-raw-type(#"<raw-c-unsigned-long>",      t["iWord"],
                    #"unsigned");
  register-raw-type(#"<raw-c-signed-long-long>",   $llvm-i64-type,
                    #"signed");
  register-raw-type(#"<raw-c-unsigned-long-long>", $llvm-i64-type,
                    #"unsigned");
  register-raw-type(#"<raw-c-float>",              $llvm-float-type,
                    #"float");
  register-raw-type(#"<raw-c-double>",             $llvm-double-type,
                    #"float");
  register-raw-type(#"<raw-c-long-double>",        llvm-long-double-type,
                    #"float");
  register-raw-type(#"<raw-c-void>",               $llvm-void-type,
                    #f);
  register-raw-type(#"<raw-c-pointer>",            $llvm-i8*-type,
                    #"pointer");
  register-raw-type(#"<raw-boolean>",              $llvm-i8-type,
                    #"boolean");
  register-raw-type(#"<raw-byte-character>",       $llvm-i8-type,
                    #"unsigned-char");
  register-raw-type(#"<raw-unicode-character>",    $llvm-i32-type,
                    #"unsigned-char");
  register-raw-type(#"<raw-byte>",                 $llvm-i8-type,
                    #"unsigned");
  register-raw-type(#"<raw-double-byte>",          $llvm-i16-type,
                    #"unsigned");
  register-raw-type(#"<raw-byte-string>",          $llvm-i8*-type,
                    #"pointer");
  register-raw-type(#"<raw-integer>",              t["iWord"],
                    #"signed");
  register-raw-type(#"<raw-single-float>",         $llvm-float-type,
                    #"float");
  register-raw-type(#"<raw-machine-word>",         t["iWord"],
                    #"unsigned");
  register-raw-type(#"<raw-double-float>",         $llvm-double-type,
                    #"float");
  register-raw-type(#"<raw-extended-float>",       llvm-long-double-type,
                    #"float");
  register-raw-type(#"<raw-pointer>",              $llvm-i8*-type,
                    #"pointer");
  register-raw-type(#"<raw-address>",              t["iWord"],
                    #"address");
  register-raw-type(#"<raw-c-size-t>",             t["iWord"],
                    #"unsigned");
  register-raw-type(#"<raw-c-ssize-t>",            t["iWord"],
                    #"signed");

  // MM Wrapper
  let mm-name = emit-name-internal(back-end, #f, dylan-value(#"<mm-wrapper>"));
  let placeholder = make(<llvm-opaque-type>);
  t[mm-name]
    := make(<llvm-struct-type>,
            name: mm-name,
            elements: vector(// Wrapper-Wrapper
                             placeholder,
                             // Class pointer
                             $llvm-object-pointer-type,
                             // Subtype mask (as a tagged fixed <integer>)
                             $llvm-object-pointer-type,
                             // Fixed part length and format
                             t["iWord"],
                             // Variable part length and format
                             t["iWord"],
                             // Pattern vector size (as a tagged <integer>)
                             $llvm-object-pointer-type,
                             // (Empty) Pattern vector
                             make(<llvm-array-type>,
                                  size: 0,
                                  element-type: t["iWord"])));
  placeholder.llvm-placeholder-type-forward
    := llvm-pointer-to(back-end, t[mm-name]);

  // Heap fixup table struct
  let heap-fixup-struct-name = "struct.heapfixup";
  back-end.llvm-heap-fixup-entry-llvm-type
    := make(<llvm-struct-type>,
            name: heap-fixup-struct-name,
            elements: vector(// Heap object
                             $llvm-object-pointer-type,
                             // Heap reference
                             llvm-pointer-to(back-end,
                                             $llvm-object-pointer-type)));
end method;

// Register each of the built-in types in a new module's type symbol table
define method llvm-register-types
    (back-end :: <llvm-back-end>, module :: <llvm-module>) => ()
  for (type keyed-by name in back-end.%type-table)
    module.llvm-type-table[name] := type;
  end for;
end method;


/// Object types

define method llvm-object-type
    (back-end :: <llvm-back-end>, o)
 => (type :: <llvm-type>);
  let class = o.&object-class;
  ^ensure-slots-initialized(class);
  let rslotd = class.^repeated-slot-descriptor; 
  let repeated-size = rslotd & ^slot-value(o, ^size-slot-descriptor(rslotd));
  llvm-class-type(back-end, class, repeated-size: repeated-size)
end method;

// Compute the type for representing instances of a class as an LLVM struct;
// Classes with repeated slot definitions require one type definition for
// each size encountered.
define method llvm-class-type
    (back-end :: <llvm-back-end>, class :: <&class>,
     #key repeated-size :: false-or(<integer>) = #f)
 => (type :: <llvm-type>);
  let base-name = emit-name-internal(back-end, #f, class);
  let type-name
    = if (repeated-size & ~zero?(repeated-size))
        format-to-string("%s.%d", base-name, repeated-size)
      else
        base-name
      end if;

  // Locate the memoized type, if any, with that name
  let module = back-end.llvm-builder-module;
  let type-table = module.llvm-type-table;
  let type = element(type-table, type-name, default: #f);
  if (type)
    type
  else
    let islots = class.^instance-slot-descriptors;
    let rslotd = class.^repeated-slot-descriptor; 
    let elements
      = make(<simple-object-vector>,
             size: if (rslotd) islots.size + 2 else islots.size + 1 end);

    // The first element is always the wrapper pointer
  let mm-name = emit-name-internal(back-end, #f, dylan-value(#"<mm-wrapper>"));
    elements[0] := llvm-pointer-to(back-end, type-table[mm-name]);

    // One element for each slot
    for (instance-slot in islots, index from 1)
      elements[index]
        := llvm-reference-type(back-end, instance-slot.^slot-type);
    finally
      if (rslotd)
        // One array element for the repeated slot
        let repeated-type = llvm-repeated-type(back-end, rslotd.^slot-type);
        elements[index] := make(<llvm-array-type>,
                                size: repeated-size | 0,
                                element-type: repeated-type);
      end if;
    end for;

    element(type-table, type-name)
      := make(<llvm-struct-type>, name: type-name, elements: elements)
  end if
end method;

// Uses of raw types utilize the registered LLVM type for the raw type
define method llvm-reference-type
    (back-end :: <llvm-back-end>, o :: <&raw-type>)
 => (type :: <llvm-type>);
  back-end.%raw-type-table[o]
end method;

// Raw structure types
define method llvm-reference-type
    (back-end :: <llvm-back-end>, o :: <&raw-struct-type>)
 => (type :: <llvm-type>);
  let name = concatenate("struct.", o.^debug-name);

  // Locate the memoized type, if any, with that name
  let type-table = back-end.llvm-builder-module.llvm-type-table;
  let type = element(type-table, name, default: #f);
  if (type)
    type
  else
    let elements = make(<stretchy-object-vector>);
    do(curry(add-llvm-struct-member, back-end, elements),
       o.raw-aggregate-members);
    element(type-table, name)
      := make(<llvm-struct-type>, name: name, elements: elements)
  end
end method;

define method add-llvm-struct-member
    (back-end :: <llvm-back-end>, elements :: <stretchy-object-vector>,
     member :: <raw-aggregate-member>)
 => ();
  add!(elements, llvm-reference-type(back-end, member.member-raw-type));
end method;

define method add-llvm-struct-member
    (back-end :: <llvm-back-end>, elements :: <stretchy-object-vector>,
     member :: <raw-aggregate-array-member>)
 => ();
  let element-type = llvm-reference-type(back-end, member.member-raw-type);
  add!(elements,
       make(<llvm-array-type>,
	    size: member.member-array-length,
	    element-type: element-type));
end method;

// References to most objects use the object pointer type
define method llvm-reference-type
    (back-end :: <llvm-back-end>, o)
 => (type :: <llvm-type>);
  $llvm-object-pointer-type
end method;

// Types for storage in repeated slots (matching repeated-representation-size)
define method llvm-repeated-type
    (back-end :: <llvm-back-end>, o)
 => (type :: <llvm-type>);
  select (o)
    dylan-value(#"<byte-character>") =>
      $llvm-i8-type;
    dylan-value(#"<unicode-character>"), dylan-value(#"<machine-word>") =>
      llvm-reference-type(back-end, dylan-value(#"<raw-machine-word>"));
    dylan-value(#"<single-float>") =>
      $llvm-float-type;
    dylan-value(#"<double-float>") =>
      $llvm-double-type;
    otherwise =>
      llvm-reference-type(back-end, o);
  end select
end method;

define method llvm-repeated-type
    (back-end :: <llvm-back-end>, o :: <&limited-integer>)
 => (type :: <llvm-type>);
  make(<llvm-integer-type>, width: repeated-representation-size(o) * 8)
end method;


/// Code types

// Lambdas (internal entry points)

define method llvm-signature-types
    (back-end :: <llvm-back-end>, o :: <&iep>,
     sig-spec :: <signature-spec>, sig :: <&signature>)
 => (parameter-types :: <sequence>);
  let parameter-types = make(<stretchy-object-vector>);

  // Required parameters
  for (type in ^signature-required(sig),
       i from 0 below ^signature-number-required(sig))
    add!(parameter-types, llvm-reference-type(back-end, type));
  end for;
  // Optional parameters
  if (^signature-optionals?(sig))
    add!(parameter-types, $llvm-object-pointer-type);
  end if;
  // Keyword parameters
  for (spec in spec-argument-key-variable-specs(sig-spec))
    add!(parameter-types, $llvm-object-pointer-type);
  end for;
  // Calling convention parameters
  add!(parameter-types, $llvm-object-pointer-type); // next-methods
  add!(parameter-types, $llvm-object-pointer-type); // function

  parameter-types
end method;

define method llvm-dynamic-signature-types
    (back-end :: <llvm-back-end>, o :: <&iep>, sig-spec :: <signature-spec>)
 => (parameter-types :: <sequence>);
  let parameter-types = make(<stretchy-object-vector>);

  // Required parameters
  for (spec in spec-argument-required-variable-specs(sig-spec))
    add!(parameter-types, $llvm-object-pointer-type);
  end for;
  // Optional parameters
  if (spec-argument-optionals?(sig-spec))
    add!(parameter-types, $llvm-object-pointer-type);
  end if;
  // Keyword parameters
  for (spec in spec-argument-key-variable-specs(sig-spec))
    add!(parameter-types, $llvm-object-pointer-type);
  end for;
  // Calling convention parameters
  add!(parameter-types, $llvm-object-pointer-type); // next-methods
  add!(parameter-types, $llvm-object-pointer-type); // function

  parameter-types
end method;

// Function type for an Internal Entry Point function
define method llvm-lambda-type
    (back-end :: <llvm-back-end>, o :: <&iep>)
 => (type :: <llvm-function-type>);
  let fun = function(o);
  let signature = ^function-signature(fun);

  // Compute parameter types
  let parameter-types
    = if (signature)
        llvm-signature-types(back-end, o, signature-spec(fun), signature)
      else
        llvm-dynamic-signature-types(back-end, o, signature-spec(fun))
      end if;
  make(<llvm-function-type>,
       return-type: llvm-reference-type(back-end, back-end.%mv-struct-type),
       parameter-types: parameter-types,
       varargs?: #f)
end method;

// Function type for a C function
// FIXME these are actually subject to target-specific/ABI-specific
// normalization
define method llvm-c-function-type
    (back-end :: <llvm-back-end>, o :: <&c-function>)
 => (type :: <llvm-function-type>);
  let signature = o.c-signature;
  let parameter-types
    = map(curry(llvm-reference-type, back-end), signature.^signature-required);
  let return-type
    = if (empty?(signature.^signature-values))
        $llvm-void-type
      else
        llvm-reference-type(back-end, signature.^signature-values.first);
      end if;
  make(<llvm-function-type>,
       parameter-types: parameter-types,
       return-type: return-type,
       varargs?: #f)
end method;
