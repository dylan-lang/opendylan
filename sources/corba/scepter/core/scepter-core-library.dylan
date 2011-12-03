Module:    dylan-user
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scepter-core
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use c-lexer;
  use scepter-utilities, export: { scepter-utilities };

  export
    scepter-back-end,
    scepter-front-end,
    scepter-driver,
    scepter-error;
end library;

