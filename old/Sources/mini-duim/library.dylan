Module:    dylan-user
Synopsis:  A miniature DUIM
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library mini-duim
  use dylan;
  use functional-extensions;

  // For debugging
  use simple-streams;
  use simple-format;

  use transcendentals;

  export mini-duim;
end library mini-duim;
