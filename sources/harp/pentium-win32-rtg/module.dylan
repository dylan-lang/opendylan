module:    dylan-user
Synopsis:  The module definition for the PENTIUM-WIN32-RTG library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module pentium-win32-rtg
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use pentium-harp, export: { <pentium-windows-back-end> };
  use pentium-rtg, export: all;
  use threads;
end module;
