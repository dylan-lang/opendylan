Module:    Dylan-User
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module console-environment
  use environment-imports;	// this gets functional-dylan
  use simple-debugging;
  use dfmc-common,
    import: { *debug-out* => *dfmc-debug-out* };
  use operating-system,
    import: { application-filename,
	      application-arguments,
	      exit-application };
  use file-system,
    import: { file-exists?,
              working-directory };
  use standard-io;
  use format;
  use commands;
  use command-lines;

  use environment-protocols,
    exclude: { application-arguments,
               application-filename };
  use environment-commands;
end module console-environment;
