module:    dylan-user
Synopsis:  The library definition for the LINUX-CORE-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library linux-core-rtg
  use functional-dylan;
  use io;
  use system;
  use dfmc-back-end-protocol;
  use harp;
  use native-harp;
  use native-core-rtg;

  export linux-rtg;
end library;
