Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-ffi
  use functional-dylan;
  use equal-table;
end library;

define module generic-ffi
  use dylan;
end module;

define module c-ffi
  use functional-dylan,
    rename: { \+ => sealed+, \- => sealed-,
              aref => sealed-aref, aref-setter => sealed-aref-setter };
  use equal-table;
  use stream;
  use generic-ffi;
  export

    <C-value>, 
      size-of, alignment-of, pointer-type,
    
    // Fundamental types:

    <C-signed-char>,  <C-unsigned-char>,  <C-char>,
    <C-signed-short>, <C-unsigned-short>, <C-short>,
    <C-signed-int>,   <C-unsigned-int>,   <C-int>,
    <C-signed-long>,  <C-unsigned-long>,  <C-long>,
    <C-float>, 
    <C-double>, 
    <C-long-double>,
    <C-void>,

    // Structs and unions:

    <C-struct>, <C-union>,

    // Derived types:

    <C-pointer>,
      referenced-type, null-pointer, null-pointer?,
    <C-typed-pointer>,   
      pointer-value, pointer-value-setter, default-allocator,
    <C-untyped-pointer>,

    contents-type, canonical-type,

    <C-array>, 
      array-type, array-type-element-type, array-type-dimensions,

    <C-function>,
      function-type,

    // Mapping substrate:

    import-value, 
    export-value, 

    class-for-map,

    // Untility types:

    <C-string>, <C-character>,
    // <C-boolean>, <C-dylan-object>,

    // Macros:

    C-subtype-definer,
    C-type-alias-definer,
    C-struct-definer,
    C-union-definer,
    C-variable-definer,
    C-constant-definer,
    C-function-definer,
    C-callable-wrapper-definer,

    // Because of hygiene failure:

    infix-define-friendly-C-subtype,
    infix-define-friendly-C-struct,
    infix-define-friendly-C-union,
    infix-define-friendly-C-variable,
    infix-define-friendly-C-constant,
    infix-define-friendly-C-function,
    infix-define-friendly-C-callable-wrapper,
    infix-define-friendly-C-pointer-type,

    define-C-pointer-types,

    %register-wrapper-class,
    %register-pointer-wrapper-class,

    <field-descriptor>, initialize-c-struct,
    <arg-descriptor>, <result-descriptor>, generate-c-call-out,
    lw-import, lw-export,

    // Extras:

    pointer-pointer, pointer-pointer-setter,
    copy-wrapper-properties, referenced-type-setter, pointer-type-setter,
    copy-wrappers-properties,
    pointer-element-count,
    low-level-value-at, low-level-value-at-setter;

end module c-ffi;

// eof
