Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Also requires the Functional Developer extension debug-name.

define library variable-search
  use functional-dylan;
  use dfmc-mangling;
  export
    variable-search;
end library;

define module variable-search
  use dylan;
  use dylan-extensions;
  use dylan-direct-c-ffi;
  use simple-format;
  use dfmc-mangling;
  export
    locate-variable, variable-value;
end module;
