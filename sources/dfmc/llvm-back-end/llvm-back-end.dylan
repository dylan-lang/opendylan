Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Maximum number of values for multiple-value return
define constant $maximum-value-count = 64;

define abstract class <llvm-back-end> (<back-end>, <llvm-builder>)
  // Predefined LLVM types
  constant slot %type-table :: <mutable-explicit-key-collection>
    = make(<string-table>);

  // Heap fixup table entry type
  slot llvm-heap-fixup-entry-llvm-type :: <llvm-type>;

  // LLVM type for each defined <&raw-type> instance
  constant slot %raw-type-table :: <object-table>
    = make(<object-table>);

  // Debug encoding type for each defined <&raw-type> instance
  constant slot %raw-type-dbg-encoding-table :: <object-table>
    = make(<object-table>);

  // Cache of <llvm-pointer-type> objects, keyed by pointee type
  constant slot %pointer-to-table :: <object-table>
    = make(<object-table>, weak: #"value");

  // Cache of direct objects
  constant slot %direct-object-table :: <object-table>
    = make(<object-table>, weak: #"value");

  // Cache of (integral, word-sized) raw objects
  constant slot %raw-object-table :: <object-table>
    = make(<object-table>, weak: #"value");

  // Cache of object references
  constant slot %reference-table :: <string-table>
    = make(<string-table>, weak: #"value");

  // Computed <llvm-function> objects for runtime and C primitives,
  // keyed by <llvm-primitive-descriptor> object reference
  constant slot %primitive-function-table :: <object-table>
    = make(<object-table>);

  // Precomputed byte character constants
  constant slot %byte-character-constants :: <simple-object-vector>
    = make(<simple-object-vector>, size: 256);

  // Value import function
  inherited slot llvm-builder-value-function,
    init-value: llvm-back-end-value-function;

  slot llvm-back-end-dbg-compile-unit :: false-or(<llvm-metadata>),
    init-value: #f;

  // Debug file descriptor
  constant slot %source-record-dbg-file-table :: <object-table>
    = make(<object-table>);

  // Debug function table
  constant slot %lambda-dbg-function-table :: <object-table>
    = make(<object-table>);

  // LLVM debug type for each defined <&raw-type> instance (or <object>)
  constant slot %dbg-type-table :: <object-table>
    = make(<object-table>);

  // Precomputed multiple value return structure type
  slot %mv-struct-type :: <&raw-struct-type>;

  // Precomputed Thread Environment Block structure type
  slot llvm-teb-struct-type :: <&raw-struct-type>;

  // Precomputed Bind Exit Frame structure type
  slot llvm-bef-struct-type :: <&raw-struct-type>;

  // gep indices for each field
  constant slot %raw-struct-field-index :: <object-table>
    = make(<object-table>);
end;

define generic llvm-back-end-target-triple
    (back-end :: <llvm-back-end>) => (triple :: <string>);

define generic llvm-back-end-data-layout
    (back-end :: <llvm-back-end>) => (layout :: <string>);

define sealed method initialize
    (back-end :: <llvm-back-end>, #key, #all-keys) => ()
  next-method();

  without-dependency-tracking
    // Initialize MV return value structure
    back-end.%mv-struct-type
      := make(<&raw-struct-type>,
              debug-name: "dylan-mv",
              options: #[],
              members:
                vector(make(<raw-aggregate-ordinary-member>,
                            raw-type: dylan-value(#"<raw-pointer>")),
                       make(<raw-aggregate-ordinary-member>,
                            raw-type: dylan-value(#"<raw-byte>"))));

    // Initialize TEB structure
    initialize-teb-struct-type(back-end);

    // Initialize NLX bind exit frame structure
    initialize-bef-struct-type(back-end);

    // Initialize predefined/raw LLVM types
    initialize-type-table(back-end);

    // Create canonical instances of the 256 i8 constants
    for (i from 0 below 256)
      back-end.%byte-character-constants[i]
        := make(<llvm-integer-constant>, type: $llvm-i8-type, integer: i);
    end for;
  end;
end method;

define thread variable *loose-mode?* = #f;
define thread variable *interactive-mode?* = #f;

define method llvm-retract-cached (back-end :: <llvm-back-end>) => ()
  remove-all-keys!(back-end.%pointer-to-table);
  remove-all-keys!(back-end.%direct-object-table);
  remove-all-keys!(back-end.%raw-object-table);
  remove-all-keys!(back-end.%reference-table);
  remove-all-keys!(back-end.%primitive-function-table)
end method;


/// Value import support

define method llvm-back-end-value-function
    (builder :: <llvm-back-end>, value :: <llvm-value>)
 => (result :: <llvm-value>);
  value
end method;

// Automatically declare referenced global values in the current module
define method llvm-back-end-value-function
    (builder :: <llvm-back-end>, value :: <llvm-global-value>)
 => (result :: <llvm-value>);
  llvm-builder-declare-global(builder, value.llvm-global-name, value)
end method;

define method llvm-back-end-value-function
    (back-end :: <llvm-back-end>, value :: <abstract-integer>)
 => (result :: <llvm-value>);
  element(back-end.%raw-object-table, value, default: #f)
    | begin
        let iWord-type = back-end.%type-table["iWord"];
        let constant
          = make(<llvm-integer-constant>, type: iWord-type, integer: value);
        element(back-end.%raw-object-table, value) := constant;
        constant
      end
end method;

define method llvm-back-end-value-function
    (back-end :: <llvm-back-end>, value :: <single-float>)
 => (result :: <llvm-value>);
  make(<llvm-float-constant>, type: $llvm-float-type, float: value)
end method;

define method llvm-back-end-value-function
    (back-end :: <llvm-back-end>, value :: <double-float>)
 => (result :: <llvm-value>);
  make(<llvm-float-constant>, type: $llvm-double-type, float: value)
end method;
