Module:    Dylan-User
Synopsis:  Environment-Generic Source Control System Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module source-control-generic-backend
  use functional-dylan;
  use simple-format;
  use threads;
  use operating-system,
    import: { run-application };
  use commands;
  use locators;
  use file-system,
    import: { <pathname> };

  // We want this so we can display a dialog...
  use duim;

  use source-control-manager-internals;

  export <generic-source-control-system>;
end module source-control-generic-backend;
