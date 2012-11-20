Module: dylan-user
Synopsis: Library and module definitions for core projects protocol.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library projects
  use dylan;
  use common-dylan;
  use memory-manager;
  use build-system;
  use release-info;
  use dood;        //---*** andrewa: just for with-walk-progress
  // Probably don't need all this, sort it out later...
  use collections;
  use io;
  use system;

  use source-records;
  use file-source-records;
  use dfmc-common;

  use dfmc-browser-support;

  // for template projects
  use dfmc-macro-expander;
  use dfmc-namespace;

  export projects, projects-implementation, lid-projects;
end library;

define module projects
  create <project>,
    <project-target-type>,
    project-name,
    project-read-only?,
    project-read-only?-setter,
    project-location,
    project-canonical-source-records,
    project-source-records,
    project-dylan-sources,
    project-source-canonical-source-record,
    make-project,
    project-owners,
    project-top-level?,
    project-top-level?-setter,
    open-project,
    close-project,
    close-all-projects,
    close-unused-projects,
    project-remove-build-products,
    note-project-closed,
    all-used-projects,
    directly-used-projects,
    compile-library,
    link-library,
    save-project,
    save-project-database,
    update-libraries,
    build-project,
    target-platform-name,
    target-platform-name-setter,
    load-namespace,
    load-library,
    project-load-namespace,
    parse-project,
    project-progress-text,
    project-stage-text,
    project-internal-progress-text,
    project-progress-report,
    project-condition-report,
    project-dump-conditions,
    project-dump-emacs-dispatch-colors,
    <project-warning>,
    <project-serious-warning>,
    <project-fatal-error>,
    show-compiler-messages?, show-compiler-messages?-setter,
    show-internal-compiler-messages?, show-internal-compiler-messages?-setter,
    \with-compiler-transaction,
    \with-project-manager-transaction,
    *default-inter-library-binding*,
    *default-library-major-version*,
    *default-library-minor-version*,
    *default-library-library-pack*,
    *default-compilation-mode*,
    lookup-named-project,
    project-build-location,
    project-build-location-setter,
    project-database-location,
    project-database-location-setter,
    project-profile-location,
    project-profile-location-setter,
    project-source-location,
    project-compilation-mode,
    project-compilation-mode-setter,
    project-compiler-back-end,
    project-compiler-back-end-setter,
    project-operating-system,
    project-operating-system-setter,
    project-processor,
    project-processor-setter,
    project-library-loose-bindings,
    project-library-loose-bindings-setter,
    project-library-pack,
    project-library-pack-setter,
    project-library-tight-bindings,
    project-library-tight-bindings-setter,
    project-major-version,
    project-minor-version,
    project-major-version-setter,
    project-minor-version-setter,
    project-target-type,
    project-target-type-setter,
    project-executable-name,
    project-executable-name-setter,
    project-library-name,
    note-database-saved,
    note-database-unsaved,
    note-database-invalidated,
    ensure-project-database,
    session-property,
    session-property-setter,
    project-build-property,
    project-build-property-setter;
  create
    <system-project-not-usable>, condition-unusable-project;
  create // compiler testing support
    dylan-library-compilation-context,
    compile-template;
  // Callbacks - the define generics are in the compiler to avoid module
  // circularities, but we provide the implementation
  use dfmc-project-compilation,
    import: { used-library-context,
              project-record-id-source-record,
              library-progress-text,
              library-progress-report
            },
    export: all;
  use build-system,
    import: { default-build-script, default-build-script-setter },
    export: all;
end module;

define module projects-implementation
  use dylan;
  use common-extensions, exclude: { format-to-string };
  use threads;
  use memory-manager;
  use build-system;
  use release-info;
  use dood,
    import: { \with-walk-progress };

  // Probably don't need all this, sort it out later...
  use collectors;
  use set;
  use locators;
  use streams;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system, rename: {load-library => os/load-library};
  use source-records;
  use file-system;
  use dfmc-common;
  use dfmc-progress-reports;
  use dfmc-project-compilation;
  use dfmc-derived-information,
    import: { project-library-definition,
              compilation-context-library-name };

  // This is just for emacs dispatch coloring support
  use dfmc-derived-information,
    import: { source-record-dispatch-decisions,
              source-record-notes,
              program-note-location };

  // for template projects
  use dfmc-macro-expander;
  use dfmc-namespace,
    import: { library-description-compiler-back-end-name-setter,
              library-description-compilation-records };

  use projects, export: all;
  export
    user-warning,
    user-error,
    user-fatal-error,
    project-personal-library?,
      project-personal-library?-setter,
    project-current-compilation-context,
      project-current-compilation-context-setter,
    project-close-compilation-contexts,
    *default-project-class*,
    project-current-source-records,
    project-reset-database,
    note-project-made,
    note-loading-namespace,
    project-namespace-loaded,
    note-project-loaded,
    note-compiling-definitions,
    note-compiled-definitions,
    note-used-project,
    project-keyword-property,
    project-keyword-property-setter,
    project-build-settings,
    project-source-record-class,
    used-library-project-key,
    project-key?,
    make-used-project,
    project-compiler-setting, project-compiler-setting-setter,
    platform-namestring,
    platform-namestring-info,
    default-platform-info,
    set-default-platform-info,
    note-platform-change,
    *all-open-projects*,
    %close-project,
    %project-closed?,
    $pm-lock,
    project-add-owner,
    generate-makefile,
    makefile-exists?,
    project-verify-source-records,
    project-id-canonical-source-record,
    project-compiler-source-location,
    project-dynamic-environment,
    project-dynamic-environment-setter;
  export // for debugging only
    <string-template-source-record>;
  export // for unified projects
    \with-used-project-cache, do-with-used-project-cache,
    canonicalize-project-sources,
    find-platform-project;
end module;

define module lid-projects
  use dylan;
  use common-extensions, exclude: { format-to-string };
  use simple-debugging, import: { debug-out };
  // Probably don't need all this, sort it out later...
  use locators;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system, rename: {load-library => os/load-library};
  use file-system;
  use release-info;

  use file-source-records;
  use dfmc-project-compilation;
  use projects;
  use projects-implementation;
  export
    <lid-project>,
    $standard-lid-keyword,
    <project-layout>,
    $dylan-source-suffix,
    $dylan-database-suffix,
    $dylan-profile-suffix,
    project-registered-name,
    lid-build-settings,
    read-lid-data,
    project-lid-location,
    project-lid-location-setter,
    project-lid-date,
    project-lid-date-setter,
    project-lid-library-name,
    project-lid-library-name-setter,
    project-source-files,
    project-compiler-source-files,
    project-source-files-setter,
    update-project-files,
    update-project-location,
    reinitialize-lid-project,
    compute-compiler-source-records,
    project-files-to-source-records,
    project-lid-file-info,
    project-lid-file-info-setter,
    read-file-library-description;
end module;

