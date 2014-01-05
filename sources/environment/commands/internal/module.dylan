Module:    Dylan-User
Synopsis:  The internal-only commands provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module environment-internal-commands
  use environment-imports,
    exclude: { load-library };
  use environment-protocols,
    // Prevent name clashes with projects:projects, imported below
    // via project-manager-interface.  (Many of these are renamed but
    // not used.  Not sure why they weren't just excluded.  --cgay)
    rename: { close-project => env/close-project,
	      project-name => env/project-name,
	      project-source-location => env/project-source-location,
              project-other-sources => env/project-other-sources,
	      open-project => env/open-project,
              save-project =>  env/save-project,
              save-project-database =>  env/save-project-database,
              project-compiler-back-end => env/project-compiler-back-end,
              project-compiler-back-end-setter => env/project-compiler-back-end-setter,
              project-compilation-mode => env/project-compilation-mode,
              project-compilation-mode-setter => env/project-compilation-mode-setter,
              project-platform-name => env/project-platform-name,
              project-platform-name-setter => env/project-platform-name-setter,
              project-target-type => env/project-target-type,
              <project-target-type> => env/<project-target-type>,
              project-target-type-setter => env/project-target-type-setter,
              project-base-address => env/project-base-address,
              project-base-address-setter => env/project-base-address-setter,
              project-major-version => env/project-major-version,
              project-major-version-setter => env/project-major-version-setter,
              project-minor-version => env/project-minor-version,
              project-minor-version-setter => env/project-minor-version-setter,
	      project-read-only? => env/project-read-only?,
 	      do-library-modules => env/do-library-modules,
 	      source-record-top-level-forms => env/source-record-top-level-forms,
              session-property => env/session-property,
              session-property-setter => env/session-property-setter };
  use environment-commands;

  use commands;
  use command-lines;

  use project-manager-interface,
    exclude: { build-project };
  use registry-projects,
    import: { find-registries };

  use build-system;
  use dood;
  use dfmc-derived-information;

  export maybe-set-roots;
end module environment-internal-commands;
