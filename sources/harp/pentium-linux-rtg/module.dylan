module:    dylan-user
Synopsis:  The module definition for the PENTIUM-LINUX-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module pentium-linux-rtg
  use common-dylan;
  use x86-harp, export: { <x86-unix-back-end>,
                          <x86-linux-back-end>,
                          <x86-freebsd-back-end> };
  use native-rtg, export: { create-dylan-runtime };
  use linux-rtg;
  use pentium-rtg;
end module;
