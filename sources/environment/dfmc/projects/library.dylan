Module:    dylan-user
Synopsis:  DFMC environment project model
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-environment-projects
  use environment-protocols;
  use user-projects;
  use source-records;
  use file-source-records;

  use dfmc-browser-support;
  use dfmc-conditions;	//---*** for the warning classes
  use build-system;

  use dfmc-pentium-harp-cg;		// Pentium backend
  use dfmc-harp-browser-support;	// Harp browsing support
  use dfmc-debug-back-end;		// Compiler print methods

  use dfmc-environment-database;

  export dfmc-environment-projects;
end library dfmc-environment-projects;

define module dfmc-environment-projects
  use environment-imports,
    exclude: { load-library };
  use environment-protocols,
    rename: { close-project => env/close-project,
	      project-name => env/project-name,
	      project-source-location => env/project-source-location,
              project-other-sources => env/project-other-sources,
	      open-project => env/open-project,
              save-project =>  env/save-project,
              save-project-database =>  env/save-project-database,
	      default-build-script => env/default-build-script,
	      default-build-script-setter => env/default-build-script-setter,
              project-compilation-mode => env/project-compilation-mode,
              project-compilation-mode-setter => env/project-compilation-mode-setter,
              project-target-type => env/project-target-type,
              <project-target-type> => env/<project-target-type>,
              project-target-type-setter => env/project-target-type-setter,
              project-base-address => env/project-base-address,
              project-base-address-setter => env/project-base-address-setter,
              project-major-version => env/project-major-version,
              project-major-version-setter => env/project-major-version-setter,
              project-minor-version => env/project-minor-version,
              project-minor-version-setter => env/project-minor-version-setter,
	      project-read-only? => env/project-read-only? };
  use dfmc-environment-database;

  use project-manager-interface,
    exclude: { build-project };

  use dfmc-derived-information,
    import: { project-library-definition,
	      source-record-top-level-forms => dfmc/source-record-top-level-forms,
	      source-record-dispatch-decisions => dfmc/source-record-dispatch-decisions,
              <source-form> };
  use dfmc-conditions,
    import: { <program-condition> };

  use dfmc-progress-reports,
    import: { internal-reporting-setter };
  use build-system,
    import: { $personal-bin, system-release-path };

  export <native-project-object>,
         <dfmc-project-object>;
end module dfmc-environment-projects;
