Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scepter-file-front-end
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use c-lexer;
  use parser-run-time;
  use scepter-core;
  use scepter-ast;

  export
    scepter-file-front-end;
end library;
