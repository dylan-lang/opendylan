Module:    Dylan-User
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library CL
  use functional-dylan;
  use io;

  export CL-macros;
  export CL-sequences;
  export CL-plists;
  export CL-strings;
  export CL-internals;
end library CL;
