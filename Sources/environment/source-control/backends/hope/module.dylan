Module:    Dylan-User
Synopsis:  Environment-Hope Interface
Author:    Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module source-control-hope-backend
  use functional-dylan;
  use simple-format;
  use threads;
  use operating-system,
    import: { run-application };
  use settings;
  use commands;
  use streams;
  use locators;
  use file-system;
  use source-control-manager-internals;

  export <hope-source-control-system>;
end module source-control-hope-backend;
