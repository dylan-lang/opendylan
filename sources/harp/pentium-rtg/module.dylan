module:    dylan-user
Synopsis:  The module definition for the PENTIUM-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module pentium-rtg
  use functional-dylan;
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use pentium-harp;
  use native-rtg, export: all;
  use threads;
end module;
