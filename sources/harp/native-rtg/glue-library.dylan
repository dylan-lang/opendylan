module:    dylan-user
Synopsis:  The library definition for the NATIVE-GLUE-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library native-glue-rtg
  use functional-dylan;
  use io;
  use system;
  use dfmc-back-end-protocol;
  use harp;
  use native-harp;

  export native-rtg;
end library;
