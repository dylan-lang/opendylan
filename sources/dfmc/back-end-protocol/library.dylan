module:    dylan-user
Synopsis:  Compiler-front-end independent back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-back-end-protocol
  use functional-dylan;
  use dfmc-mangling;
  export dfmc-back-end-protocol;
end library;

define module dfmc-back-end-protocol
  use functional-dylan;
  use dfmc-mangling, export: all;

  export 
    <back-end>,

    <local-variable>,
    <lambda-compiled-data>,

    mangler,
    raw-mangle

    ;
end module;

