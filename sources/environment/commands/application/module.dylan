Module:    Dylan-User
Synopsis:  The application commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-application-commands
  use threads;
  use file-system;

  use commands;
  use command-lines;

  use environment-imports,
    exclude: { \profiling };
  use environment-protocols,
    exclude: { <optional-parameter>,
               parameter-name,
               parameter-type,
               parameter-keyword };
  use environment-manager;
  use environment-reports;
  use environment-commands;

  // For application callbacks
  use dfmc-application;

  // Debugging commands
  export <start-application-command>,
         <debug-application-command>,
         <play-command>;
end module environment-application-commands;
