Module:    Dylan-User
Synopsis:  DFM compiler database
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment-database
  use environment-imports;
  use print, import: { print };
  use environment-protocols,
    rename: { project-read-only? => env/project-read-only?,
	      project-target-type => env/project-target-type };

  use dfmc-derived-information,
    rename: { <source-location> => dfmc/<source-location>,
              do-library-modules => dfmc/do-library-modules,
	      source-record-top-level-forms => dfmc/source-record-top-level-forms };
  use project-manager-interface,
    import: { <project>,
              lookup-named-project,
              project-library-name,
	      all-used-projects,
	      directly-used-projects,
	      ensure-project-database,
              macroexpand-expression,
	      project-source-records,
	      project-dylan-sources,
	      project-source-canonical-source-record,
	      project-canonical-source-records,
	      project-browsing-context,
	      project-interaction-allowed?,
	      project-owners,
              project-location,
              project-build-location,
              project-executable-name,
              project-target-type,
	      project-read-only?, project-read-only?-setter };
  use dfmc-project-compilation,
    import: { all-known-compilation-contexts,
	     compilation-context-project };
  use dfmc-conditions,
    import: { <program-error>,
	      <serious-program-warning> };

  export <dfmc-database>;

  // Useful functions for dfmc-environment-projects to use
  export make-environment-object-for-source-form,
         find-source-form-location,
         project-executable-pathname,
         source-record-environment-object;

  // Useful functions for dfmc-environment-application to use
  export do-program-notes;
end module dfmc-environment-database;
