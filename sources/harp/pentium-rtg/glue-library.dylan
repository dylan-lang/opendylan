module:    dylan-user
Synopsis:  The library definition for the PENTIUM-GLUE-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library pentium-glue-rtg
  use functional-dylan;
  use io;
  use system;
  use dfmc-back-end-protocol;
  use harp;
  use pentium-harp;
  use native-glue-rtg;

  export pentium-rtg;
end library;
