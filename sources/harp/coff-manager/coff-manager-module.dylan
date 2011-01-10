module:    dylan-user
Synopsis:  The module definition for the OUTPUT-COFF module
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library coff-manager
  use functional-dylan;
  use io;
  use system;

  export coff-representation,
         coff-sizes,
         coff-builder,
         coff-writer;
end library;


define module coff-representation
  use dylan;
  use syntax-case;

  export <byte>, <short>, <word>,
         <coff-unit>,
         <coff-index>,
         index, index-setter, 

         <coff-string>, <coff-short-string>, <coff-long-string>,
         string-data,

         <coff-symbol-record>, <coff-symbol>, <coff-auxiliary-symbol>,
         section, section-setter, symbol-name, 
         symbol-value, symbol-value-setter,
         symbol-type, symbol-type-setter,
         storage-class, storage-class-setter,
         aux-symbols, aux-symbols-setter,

         <coff-relocation>, 
         <coff-relative-relocation>, <coff-absolute-relocation>,
         relocation-symbol, relocation-symbol-setter,
         relocation-type, relocation-type-setter,

         <coff-section>, <coff-symbol-locator>,
         <coff-undefined-locator>,
         section-name,   section-name-setter,
         section-data, section-data-units,
         section-flags, section-flags-setter, 
         section-alignment,
         virtual-size, virtual-size-setter,
         rva-offset, rva-offset-setter, 
         line-numbers, line-numbers-setter,
         big-endian?,
         relocations, relocations-setter, 
         coff-section-undefined,

         <coff-table>,
         table,
         coff-element, coff-element-setter, 

         <coff-symbol-table>, 

         <coff-string-table>, 
         add-coff-string, 

         <coff-section-table>,

         machine, time-stamp, header-size, 
         characteristics, sections,
         symbols, strings,
         coff-section-data, coff-section-data-setter,
         coff-symbol-data,  coff-symbol-data-setter,
         coff-string-data , coff-string-data-setter,
         <coff-file>; 
end module;


define module coff-sizes
  use dylan;
  use syntax-case;
  use coff-representation;

  export 
    file-headers-size,
    section-headers-size,
    sections-size,
    symbol-table-size,
    string-table-size,
    total-file-size,
    symbol-table-offset,
    first-section-offset,
    string-table-offset,
    total-relocation-size,
    total-line-numbers-size,
    unit-size;
end module;


define module coff-writer
  use dylan;
  use streams, exclude: {count, name};
  use format;
  use print;
  use locators;
  use syntax-case;
  use coff-representation;
  use coff-sizes;

  export 
    write-coff,
    write-coff-header,
    write-section-table,
    write-section-data,
    write-coff-section,
    write-relocations,
    write-line-numbers,
    write-symbol-table,
    write-string-table,
    write-byte,
    write-short,
    write-word;
end module;


define module coff-builder
  use dylanworks;
  use syntax-case;
  use coff-writer, export: {write-coff};
  use coff-representation;
  export
    <coff-builder>,
    coff-file,
    make-coff-builder,
    make-coff-symbol,
    define-external-symbol, 
    add-symbol-definition,
    add-data, 
    add-data-byte, 
    add-data-string,
    select-coff-section,
    fixup-coff-builder;

end module;


