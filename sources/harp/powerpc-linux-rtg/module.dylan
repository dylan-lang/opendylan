module:    dylan-user
Synopsis:  The module definition for the POWERPC-LINUX-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module powerpc-linux-rtg
  use functional-dylan;
  use powerpc-harp, export: { <powerpc-linux-back-end> };
  use native-rtg, export: { create-dylan-runtime };
  use linux-rtg;
  use powerpc-rtg;
end module;
