Module:    Dylan-User
Synopsis:  Environment Manager
Author:    Andy Armstrong, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// -=- ENVIRONMENT-COMMANDS -=-
///
/// Commands available to external programs.

define module environment-commands
  create properties, do-cmd-properties,
	 complete, do-cmd-complete,
	 edit-definitions, do-cmd-edit-definitions,
	 compile, do-cmd-compile,
	 documentation, do-cmd-documentation,
	 describe, do-cmd-describe,
	 browse, do-cmd-browse, 
         browse-type, do-cmd-browse-type,
         browse-function, do-cmd-browse-function,
         OpenFile, do-cmd-open-file,
	 provide-results, do-cmd-provide-results;

  create <file-library-not-found-warning>,
         <file-project-not-found-warning>;
end module environment-commands;

/// -=- ENVIRONMENT-COMMAND-CALLING -=-
///
/// The interface to the mechanism for calling various environment-commands.

define module environment-command-calling
  create register-command-function,
	 lookup-command-function,
	 lookup-and-coerced-apply,
	 <command-call-condition>,
	 <command-call-error>;
end module environment-command-calling;

/// -=- ASYNCHRONOUS RESULTS HANDLING -=-
///
/// The asynch results modules could be put into a separate library.

define module asynchronous-results
  // Conditions common to awaiting and providing results.
  create <invalid-results-id>;

  // Awaiting results.
  create get-results-id,
	 wait-for-results,
	 abort-results,
	 \with-asynchronous-results,
	 <timeout-awaiting-results>,
	 <keep-waiting>,
	 <assume-results>;

  // Providing results.
  create provide-results;
end module asynchronous-results;

define module asynchronous-results-implementation
  use environment-imports;
  use threads;
  use asynchronous-results,
    export: all;
end module asynchronous-results-implementation;

/// -=- ENVIRONMENT-MANAGER -=-

define module environment-manager
  use environment-imports;
  use environment-protocols;
  use environment-command-calling;
  use environment-commands,
    export: all;
  use asynchronous-results,
    rename: { provide-results => %provide-results },
    export: all;

  // Functions which the environment provides.
  export find-deuce-frame,
         find-and-call-in-deuce-frame,
	 new-project-wizard,
         // The following backend the environment-commands functions.
         show-definition-summary,
         show-documentation,
	 browse-object,
         browse-object-type,
         browse-object-generic-function,
         edit-object-definitions,
         show-properties,
         compile-project-location,
         environment-open-file;

  // Miscellaneous functions provided by this library.
  export
    find-libraries-from-pathname,
    find-projects-from-pathname,
    location-info->source-location;

  // Channel for the environment to signal among parts of itself.
  export $environment-channel,
	 <environment-message>,
	 <environment-starting-message>,
	 <environment-started-message>,
	 <environment-stopping-message>,
	 <environment-stopped-message>;
end module environment-manager;
