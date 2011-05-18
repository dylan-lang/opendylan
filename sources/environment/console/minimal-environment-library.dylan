Module:    Dylan-User
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library minimal-console-environment
  use functional-dylan;
  use system;
  use io;
  use commands;

  use dfmc-common, import: { dfmc-common };
  use release-info;

  use environment-protocols;
  use environment-commands;
  use environment-application-commands;
  use environment-internal-commands;

  // Back-ends
  use dfmc-pentium-harp-cg;      // Pentium backend
  use dfmc-harp-browser-support; // Harp browsing support
  use dfmc-c-back-end;           // C backend
  use dfmc-c-linker;             // C linker

  export console-environment;
end library minimal-console-environment;
