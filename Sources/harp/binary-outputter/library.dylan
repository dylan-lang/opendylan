module:    dylan-user
Synopsis:  The library definition for the HARP-BINARY library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library binary-outputter
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use collections;
  use io;
  use system;
  use binary-manager;
  use binary-builder;
  use harp;

  export binary-outputter;
end library;


define module binary-outputter
  use functional-dylan;
  use dylan-extensions;
  use byte-vector;
  use table-extensions, import: {<string-table>};
  use format;
  use streams;
  use locators;
  use binary-manager;
  use binary-builder;
  use harp-for-extenders;

  export
    <harp-binary-builder>,
    do-export,

    assemble-harp-outputter
    
    ;

end module;


