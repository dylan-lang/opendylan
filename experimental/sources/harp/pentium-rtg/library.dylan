module:    dylan-user
Synopsis:  The library definition for the PENTIUM-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library pentium-rtg
  use functional-dylan;
  use io;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use pentium-harp;
  use dylan-rtg;
  use threads;

  export pentium-rtg;
end library;
