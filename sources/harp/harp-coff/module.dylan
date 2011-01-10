module:    dylan-user
Synopsis:  The module definition for the HARP-COFF library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module dylan-and-big-ints
  use dylan, export: all;
  use functional-extensions, export: {unsupplied, supplied?};
  use dylan-extensions, export: all;
  use big-integers, prefix: "generic-", export: all;
end module;


define module cv4-builder
  use dylan-and-big-ints;
  use coff-representation, export: { string-data, symbol-name };
  use coff-builder, export: all;
  export
    initialize-debug-section,
    select-debug-section,
    add-cv4-string, 
    add-cv4-compile-flag,
    add-cv4-register,
    add-cv4-user-defined-type,
    add-cv4-end-of-block,
    add-cv4-object-file-name,
    add-cv4-bp-relative,
    add-cv4-local-data,
    add-cv4-global-data,
    add-cv4-local-proc-start,
    add-cv4-global-proc-start,
    add-cv4-block-start,
    add-cv4-code-label;

end module;

define module harp-coff
  use dylan-and-big-ints;
  use table-extensions, import: {<string-table>};
  use format-out;
  use harp-for-extenders, 
    exclude: { big-endian? };
  use cv4-builder, export: all;
  use binary-outputter;

  export
    <harp-coff-builder>,
    $coff-type$,
    coff-machine-type,
    imported-name-mangler;
end module;


