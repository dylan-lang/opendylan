module:    dylan-user
Synopsis:  The module definition for the POWERPC-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module powerpc-rtg
  use functional-dylan;
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use native-harp;
  use powerpc-harp;
  use native-rtg;
  use threads;
end module;
