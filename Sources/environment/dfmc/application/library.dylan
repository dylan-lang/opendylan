Module:    dylan-user
Synopsis:  environment application server library
Author:    Bill Chiles, Jason Trenouth, Paul Howard, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-environment-application
  use functional-dylan;
  use collections;
  use io;
  use channels;

  use environment-protocols;
  use dfmc-environment-database;
  use target-application;

  use debugger-manager;
  use interactive-symbol-table;
  use tether-downloader;
  use interactive-downloader;

  use user-projects;

  export dfmc-application;
end library dfmc-environment-application;


define module dfmc-application
  use environment-imports,
    rename: { load-library => os/load-library,
	      thread-name => threads/thread-name };
  use dylan-extensions,
     import: {<abstract-integer>};
  use threads,
    import: {<thread>, <lock>, with-lock, <notification>, wait-for, release,
             <simple-lock>};
  use byte-vector,
    import: {<byte-vector>};

  use project-manager-interface,
    exclude: { build-project,
               save-project-database,
               project-compilation-mode, project-compilation-mode-setter };

  use channels;
  use environment-protocols,
    rename: { close-project => env/close-project,
	      project-name => env/project-name,
	      project-source-location => env/project-source-location,
	      project-other-sources => env/project-other-sources,
	      open-project => env/open-project,
              save-project => env/save-project,
	      default-build-script => env/default-build-script,
	      default-build-script-setter => env/default-build-script-setter,
              project-target-type => env/project-target-type,
              <project-target-type> => env/<project-target-type>,
              project-target-type-setter => env/project-target-type-setter,
              project-major-version => env/project-major-version,
              project-major-version-setter => env/project-major-version-setter,
              project-minor-version => env/project-minor-version,
              project-minor-version-setter => env/project-minor-version-setter,
	      project-read-only? => env/project-read-only? };
  use dfmc-environment-database;
  use target-application;

  use debugger-manager,
    rename: { stop-application   => dm-stop-application,
	      kill-application   => dm-kill-application,
	      stack-frame-thread => dm-stack-frame-thread,
	      close-application  => dm-close-application,
	      <thread-profile>   => dm-<thread-profile>,
	      thread-state       => dm-thread-state,
	      thread-suspended?  => dm-thread-suspended?,
	      application-stopped? => dm-application-stopped?,
	      application-just-interacted? => dm-application-just-interacted?,
	      <application-profile> => dm-<application-profile>,
	      <application-snapshot> => dm-<application-snapshot>,
	      <thread-snapshot> => dm-<thread-snapshot>,
	      application-profile-threads => dm-application-profile-threads,
	     set-application-class-breakpoint => dm-set-application-class-breakpoint,
	     clear-application-class-breakpoint => dm-clear-application-class-breakpoint,
	     clear-application-class-breakpoints => dm-clear-application-class-breakpoints },
    exclude: { connection-open?, connection-open?-setter },
    export: {application-running-on-code-entry?,
	     application-running-on-code-entry?-setter};
  use tether-downloader;
  use interactive-downloader;
  use interactive-symbol-table;

  // DFMC application (should be environment protocols?)
  export <dfmc-application>,
         application-target-app,
         register-application-callbacks,
         application-just-initialized?,
         application-reached-interaction-point?,
         application-stopped-thread;

  // Stop reasons (needed for the options dialog)
  export stop-reason-types,
         stop-reason-action, stop-reason-action-setter,
         stop-reason-action-label,
         stop-reason-class,
         stop-reason-debug-action,
         stop-reason-ignore-action,
         stop-reason-report-action,
         stop-reason-label;

  // Utilities for dfmc-environment-projects
  export connect-tether-to-project;
end module dfmc-application;
