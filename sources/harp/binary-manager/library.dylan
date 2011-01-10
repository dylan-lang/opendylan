module:    dylan-user
Synopsis:  The library definition for the Binary Manager
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library binary-manager
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use collections;
  use io;
  use system;

  export binary-manager;
end library;


define module binary-manager
  use functional-dylan,
    exclude: { format-to-string };
  use dylan-extensions;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use format;
  use streams;
  use locators;

  export
    <binary-section>,
    section-name, section-alignment,
    section-flags, section-data, section-data-setter,
    raw-data-size, raw-data-size-setter,

    <binary-file>,
    sections,
    write-binary,
    write-binary-data,
    write-binary-section,
    binary-data-section?,
    write-section-data,

    <binary-table>,
    table-data,
    binary-table-member?, binary-element-add!
    
    ;

end module;
