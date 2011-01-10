Module: dylan-user
Author: roman
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library user-projects
  use functional-dylan;
  use io;
  use system;
  use build-system;
  use release-info;
  use projects;
  use registry-projects;
  use source-records;
  use file-source-records;
  use tools-interface;
  use dfmc-browser-support;
  use dfmc-common;

  export user-projects;
  export project-manager-interface;
end library;

define module user-projects
  use functional-dylan;
  use threads;
  use machine-word-lowlevel,
    import: { machine-word-unsigned-shift-left, machine-word-unsigned-shift-right };
  use dylan-extensions,
    import: { <machine-word> };
  use format;
  use format-out;
  use streams;
  use locators;
  use operating-system,
    exclude: { load-library };
  use file-system;
  use date;
  use build-system;
  use release-info;
  use print;
  use projects;
  use projects-implementation;
  use lid-projects;
  use registry-projects;
  use source-records;
  use file-source-records;
  use tools-interface;
  use dfmc-project-compilation;
  use dfmc-interactive-execution;
  use dfmc-common;
  use dfmc-derived-information,
    import: { project-library-definition,
	     compilation-context-library-name };
  export
    <system-project>,
    <user-project>,
    $user-project-suffix,
    <project-not-found>,
    condition-project-name,
    <duplicate-file-name-error>,
    <yes-or-no-condition>,
    <duplicate-project-condition>,
    duplicate-project-key,
    <cannot-open-project-file-condition>,
    condition-project-file-location,
    search-for-project,
    project-file-location,
    <find-project-location-restart>,
    save-project-as-lid-file,
    new-user-project,
    project-add-file,
    project-remove-file,
    project-sort-files,
    project-other-sources,
    import-lid-project,
    project-user-projects,
    project-current-debug-target-setter,
    evaluate-expression,
    macroexpand-expression,
    parse-expression,
    project-id-interactive-record,
    project-execution-context,
    project-browsing-context,
    project-last-transaction-id,
    project-interactive-execution-notes,
    project-interaction-allowed?,
    project-flush-caches,
    library-name-from-file,
    %debugging,
    $driver-debug,
    $pm-debug,
    %debug-pm,
    project-source-record-location,
    *copy-canonical-sources?*;

end module;

define module project-manager-interface
  use projects, export: all;
  use user-projects, export: all;
end;
