Module:    Dylan-User
Synopsis:  Environment Manager
Author:    Andy Armstrong, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-commands
  create properties, do-cmd-properties,
	 complete, do-cmd-complete,
	 edit-definitions, do-cmd-edit-definitions,
	 compile, do-cmd-compile,
	 documentation, do-cmd-documentation,
	 describe, do-cmd-describe,
	 browse, do-cmd-browse,
         browse-type, do-cmd-browse-type;

  create <file-project-not-found-warning>;
end module environment-commands;

define module environment-command-calling
  create register-command-function,
	 register-command-function-spec,
	 lookup-command-function,
	 lookup-command-function-spec,
	 lookup-and-coerced-apply,
	 <command-call-condition>,
	 <command-call-error>;
end module environment-command-calling;

define module environment-manager
  use environment-imports;
  use environment-protocols;
  use environment-command-calling;
  use environment-commands,
    export: all;

  use duim;
  use channels;

  // Functions which the environment provides.
  export find-deuce-frame,
         find-and-call-in-deuce-frame,
	 new-project-wizard,
         // The following backend the environment-commands functions.
         show-definition-summary,
         browse-object,
         browse-class-type,
         edit-object-definitions,
         show-properties,
         compile-project-location;

  // Miscellaneous functions provided by this library.
  export find-projects-from-pathname;

  // Channel for the environment to signal among parts of itself.
  export $environment-channel,
	 <environment-message>,
	 <environment-startup-message>,
	 // <environment-shutting-down-message>,
	 <environment-shutdown-message>;
end module environment-manager;
