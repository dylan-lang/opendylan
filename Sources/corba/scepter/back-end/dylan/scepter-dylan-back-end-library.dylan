Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scepter-dylan-back-end
  use functional-dylan;
  use io;
  use system;
  use generic-arithmetic;
  use big-integers;
  use scepter-core;
  use scepter-ast;

  export
    scepter-dylan-back-end;
end library;

