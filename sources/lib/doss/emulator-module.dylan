Module:    dylan-user
Synopsis:  Define the emulator DOSS module patches
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module emulator-doss
  use functional-dylan,
    exclude: {close};
  use byte-vector;
  use mop;
  use set;
  use internal,
    import: {<translator-module>, module-name, find-translator-module,
             allocate, void-element, <hashed-collection>, <equal-table>,
             includes-key?, rehash!, elements, tally, table-table, 
             table-values};
  use streams-internals;
  use variable-search;
  use doss-internals;
end module emulator-doss;
