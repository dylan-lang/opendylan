Module:       Dylan-User
Synopsis:     DUIM core + Vanilla back-end
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim
  use dylan;

  // Use the DUIM core and re-export all its modules,
  // then add the Vanilla back-end
  use duim-core, export: all;
  use vanilla-duim;
end library duim;
