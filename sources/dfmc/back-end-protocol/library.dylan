module:    dylan-user
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-back-end-protocol
  use functional-dylan;
  use system;
  use io;
  use dfmc-mangling;
  use dfmc-common;
  export dfmc-back-end-protocol;
end library;

define module dfmc-back-end-protocol
  use format-out;
  use functional-dylan;
  use dfmc-common;
  use dfmc-mangling, export: all;
  
  export 
    <back-end>,
    <local-variable>,
    <lambda-compiled-data>,

    mangler,
    raw-mangle,

    register-back-end,
    find-back-end,
    find-back-end-object;
end module;


