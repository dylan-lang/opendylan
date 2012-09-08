module:    dylan-user
Synopsis:  The module definition for the HARP-X86-UNIX-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module harp-x86-unix-rtg
  use common-dylan;
  use harp-x86, export: { <harp-x86-unix-back-end>,
                          <harp-x86-linux-back-end>,
                          <harp-x86-freebsd-back-end>,
                          <harp-x86-darwin-back-end> };
  use harp-native-rtg, export: { create-dylan-runtime };
  use harp-unix-rtg;
  use harp-x86-rtg;
end module;
