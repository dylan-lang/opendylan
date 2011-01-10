module:    dylan-user
Synopsis:  The library definition for the HARP-COFF library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library coff-builder
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use coff-manager;
  use collections;
  use io;
  use system;
  use binary-builder;

  export coff-builder;
end library;


define module dylan-and-big-ints
  use dylan, export: all;
  use functional-extensions, export: {unsupplied, supplied?};
  use dylan-extensions, export: all;
  use big-integers, prefix: "generic-", export: all;
end module;

define module coff-builder
  use dylan-and-big-ints;
  use coff-constants, export: all;
  use coff-writer, export: {write-coff};
  use coff-sizes;
  use coff-representation, export: all;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use format;
  use streams;
  use locators;
  use binary-builder, export: all;

  export 
    <coff-builder>,
    make-coff-symbol,
    define-external-symbol,
    define-public-symbol,
    add-data-short,
    add-data-vector,
    add-line-number,
    add-line-number-symbol,
    add-function-line-number-definition,
    add-source-file-definition,
    insert-relocation,
    insert-interactor-relocation,
    split-word-into-bytes,
    split-short-into-bytes,
    fixup-coff-builder,
    builder-model-object-name
    
    ;

end module;


