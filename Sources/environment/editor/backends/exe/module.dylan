Module:    Dylan-User
Synopsis:  Environment-Exe Editor Interface
Author:    Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module editor-exe-backend
  use functional-dylan;
  use operating-system,
    import: { run-application };
  use commands;

  use editor-manager-internals;

  export <exe-editor>,
	 editor-image,
	 editor-process-id, editor-process-id-setter;
end module editor-exe-backend;
