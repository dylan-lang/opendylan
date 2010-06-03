Module:    Dylan-User
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module console-environment
  use environment-imports;	// this gets functional-dylan
  use simple-debugging;
  use operating-system,
    import: { application-filename,
	      application-arguments,
	      exit-application,
              environment-variable, environment-variable-setter };
  use file-system,
    import: { file-exists?,
              working-directory };
  use standard-io;
  use format;
  use commands;
  use command-lines,
    exclude: { info-title };

  use build-system,
    import: { configure-build-system };

  use environment-protocols,
    exclude: { application-filename,
               application-arguments };
  use environment-commands;
  use environment-application-commands;
end module console-environment;
