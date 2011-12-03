module:    dylan-user
Synopsis:  The module definition for the OUTPUT-COFF module
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module dylan-and-big-integers
  use dylan, export: all;
  use dylan-extensions, export: all;
  use common-extensions, export: {unsupplied, supplied?};
  use big-integers, prefix: "generic-", export: all;
end module;


define module coff-constants
  use dylan-and-big-integers;

  export $type-no-pad,
         $cnt-code,
         $cnt-initialized-data,
         $cnt-uninitialized-data,
         $lnk-other,
         $lnk-info,
         $lnk-remove,
         $lnk-comdat,
         $mem-fardata,
         $mem-purgeable,
         $mem-16bit,
         $mem-locked,
         $mem-preload,
         $align-1bytes,
         $align-2bytes,
         $align-4bytes,
         $align-8bytes,
         $align-16bytes,
         $align-32bytes,
         $align-64bytes,
         $lnk-nreloc-ovfl,
         $mem-discardable,
         $mem-not-cached,
         $mem-not-paged,
         $mem-shared,
         $mem-execute,
         $mem-read,
         $mem-write,

         $code-flags,
         $data-flags,
         $debug-flags,

         $sym-undefined,
         $sym-absolute,
         $sym-debug,

         $sym-end-of-function,
         $sym-automatic,
         $sym-external,
         $sym-static,
         $sym-function,
         $sym-file,
         $sym-section,
         $sym-type-null,
         $sym-type-function;
end module;

define module coff-representation
  use dylan-and-big-integers;
  use coff-constants, export: all;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use binary-manager, export: all;

  export <coff-byte>, <coff-short>, <coff-word>,
         <coff-unit>,
         <coff-index>,
         index, index-setter, 

         <coff-string>, <coff-short-string>, <coff-long-string>,
         string-data,

         <coff-symbol-record>, 
         <coff-symbol>,
         section, section-setter, symbol-name, 
         symbol-value, symbol-value-setter,
         symbol-type, symbol-type-setter,
         storage-class, storage-class-setter,
         aux-symbols, aux-symbols-setter,

         <coff-auxiliary-symbol>, 
         <coff-plain-auxiliary-symbol>,
         <coff-string-auxiliary-symbol>,
         <coff-empty-auxiliary-symbol>,
         <coff-section-auxiliary-symbol>,
         <coff-chained-auxiliary-symbol>,
         <coff-function-lines-auxiliary-symbol>,
         <coff-function-definition-auxiliary-symbol>,
         auxiliary-data, auxiliary-data-setter,
         auxiliary-string, auxiliary-string-setter,
         auxiliary-line-number, auxiliary-line-number-setter,
         auxiliary-next-function, auxiliary-next-function-setter, 
         auxiliary-tag, auxiliary-tag-setter, 
         auxiliary-next-function-index, auxiliary-tag-index,
         auxiliary-total-size, auxiliary-total-size-setter,
         auxiliary-line-numbers, auxiliary-line-numbers-setter,
         auxiliary-line-numbers-offset,
         auxiliary-section, auxiliary-section-setter,
         auxiliary-check-sum, auxiliary-check-sum-setter,
         auxiliary-number, auxiliary-number-setter,
         auxiliary-selection, auxiliary-selection-setter,
         next-function-index,

         <coff-relocation>, 
         <coff-genuine-relocation>, <coff-interactor-relocation>,
         <coff-relative-relocation>, <coff-absolute-relocation>,
         relocation-symbol, relocation-symbol-setter,
         relocation-type, relocation-type-setter,
         interactor-handle,

         <coff-line-number>,
         <coff-line-number-relative>, <coff-line-number-symbol>,
         line-number-rva, line-number-rva-setter,
         line-number-symbol, line-number-symbol-setter,
         line-file-offset, line-file-offset-setter,
         line-number,

         <coff-symbol-locator>,
         <coff-undefined-locator>,
         <coff-section>, <coff-bss-section>,
         section-data-size-in-file,
         section-data-size-in-image,
         virtual-size, virtual-size-setter,
         rva-offset, rva-offset-setter, 
         raw-data-pointer, raw-data-pointer-setter,
         relocs-pointer, relocs-pointer-setter,
         relocs-number, relocs-number-setter,
         linenumbers-pointer, linenumbers-pointer-setter,
         linenumbers-number, linenumbers-number-setter,
         line-numbers, line-numbers-setter,
         big-endian?,
         relocations, relocations-setter, 
         coff-section-undefined,
         coff-section-sym-absolute,
         coff-section-sym-debug,

         <coff-table>, <coff-duplicated-table>,
         id-table-data,
         ordered-data, ordered-data-setter,
         coff-element, coff-element-setter, 

         <coff-symbol-table>, 

         <coff-string-table>, 

         <coff-section-table>,

         machine, 
         time-stamp, time-stamp-setter,
         header-size, header-size-setter,
         characteristics, characteristics-setter,
         symbols, strings,
         <coff-file>; 
end module;


define module coff-sizes
  use dylan-and-big-integers;
  use coff-representation;

  export 
    size-of-symbol,
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
  use dylan-and-big-integers;
  use streams, exclude: {<byte>, count, name};
  use format;
  use print;
  use locators;
  use coff-representation;
  use coff-sizes;
  use byte-vector;

  export 
    write-coff-header,
    write-section-table,
    write-relocations,
    write-line-numbers,
    write-symbol-table,
    write-string-table,
    write-byte,
    write-short,
    write-word;
end module;


