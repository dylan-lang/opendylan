module:    dylan-user
Synopsis:  The library definition for the PENTIUM-LINUX-CORE-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library pentium-linux-core-rtg
  use common-dylan;
  use x86-harp;
  use native-core-rtg;
  use linux-core-rtg;
  use pentium-core-rtg;

  export pentium-linux-rtg;
end library;
