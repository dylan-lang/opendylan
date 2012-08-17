module:    dylan-user
Synopsis:  The library definition for the X86-WINDOWS-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp-x86-windows-rtg
  use common-dylan;
  use io;
  use system;
  use dfmc-back-end-protocol;
  use harp;
  use harp-x86;
  use harp-x86-rtg;

  export harp-x86-windows-rtg;
end library;
