module:    dylan-user
Synopsis:  The module definition for the HARP-LINUX-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module harp-unix-rtg
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use native-harp;
  use harp-native-rtg;
  use threads;

  export

	op--initialize-thread-instructions;
end module;
