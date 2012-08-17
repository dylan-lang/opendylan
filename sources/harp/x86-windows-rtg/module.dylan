module:    dylan-user
Synopsis:  The module definition for the X86-WINDOWS-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module harp-x86-windows-rtg
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use harp-x86, export: { <harp-x86-windows-back-end> };
  use harp-x86-rtg, export: all;
  use threads;
end module;
