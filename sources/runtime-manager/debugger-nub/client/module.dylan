Module:    dylan-user
Synopsis:  The CORBA client of the Remote Debugger
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module remote-nub-client
  use functional-dylan;
  use dylan-orb;
  use remote-nub-stubs, export: all;
  use remote-nub-skeletons, export: all;
end module remote-nub-client;
