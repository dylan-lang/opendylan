module:        dylan-user
synopsis:      A runtime-manager library to manage an interactive symbol
               table.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library interactive-symbol-table
  use common-dylan;
  use collections;
  use io;
  use access-path;
  export interactive-symbol-table;
end library;

define module interactive-symbol-table
  use common-dylan;
  use table-extensions, import: {<string-table>};
  use access-path;
  create

     // Macros

     \defining-symbols,

     // The interactive symbol table class itself.

     <interactive-symbol-table>,

     // Various useful accessors.

     symbol-table-access-path,
     symbol-table-redefinition-allowed?,
     performing-multiple-definitions?,
     performing-multiple-definitions?-setter,
     sort-symbol-table-by-address,

     // Related error classes....
  
     <interactive-symbol-table-error>,
     <symbol-illegal-redefinition-error>,
     <symbol-illegal-redefinition-by-name-error>,
     <symbol-illegal-redefinition-by-address-error>,
     <symbol-does-not-exist-error>,
     <symbol-no-library-error>,
     <symbol-static-no-file-error>,
     <file-no-library-error>,
     <symbol-attempted-definition-in-undefined-file-error>,

     // ... and their accessors.

     offending-symbol-table,
     redefined-name,
     existing-symbol,
     target-name,
     defined-name,
     defined-filename,
     associated-library,
     undefined-file,

     // Debugging.

     debug-display-symbol-table,

     // Searching for symbols by address.

     symbol-table-nearest-symbols,
     symbol-table-symbol-relative-address,

     // Searching for symbols by name.

     symbol-table-find-symbol,

     // Defining and undefining symbols by name.

     symbol-table-define-symbol,
     symbol-table-undefine-symbol,

     // Defining new files (static scopes for symbols)

     symbol-table-define-file,

     // Locating and defining function boundaries for the profiler.
     // (Not yet implemented).

     // symbol-table-function-bounding-addresses,
     // symbol-table-define-function-boundary,
     // symbol-table-undefine-function-boundary,

     // Registering regions of interactive memory.

     symbol-table-register-region-for-library;

end module;

define module ist-implementation
  use common-dylan;
  use table-extensions, import: {<string-table>};
  use format;
  use format-out;
  use standard-io;
  use access-path;
  use interactive-symbol-table;
end module;
