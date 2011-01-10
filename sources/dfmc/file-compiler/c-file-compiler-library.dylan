Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-c-file-compiler
  use functional-dylan;
  use metering;
  use file-source-records;
  use source-records;
  use dfmc-conversion;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-back-end;
  use dfmc-linker;
  use dfmc-management;
  use dfmc-execution;
  // use dfmc-testing;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;
  use dfmc-core;
  use dfmc-c-ffi;
  use dfmc-browser-support;

  use projects;
  use registry-projects;
  use user-projects;
  use build-system;

  export dfmc-c-file-compiler, dfmc-debug;
end library;

define module dfmc-debug
  use functional-dylan;
  use metering;

  use dfmc-conversion;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-optimization;
  use dfmc-typist;
  use dfmc-back-end;
  use dfmc-linker, rename: { <linker> => dfmc/<linker> };
  use dfmc-management, export: { *dfmc-profile-allocation?*, *combine-object-files?* };
  use dfmc-execution;

  // use dfmc-testing;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-c-ffi;
  use dfmc-project-compilation;
  use dfmc-interactive-execution;
  use dfmc-derived-information, exclude: {<macro-definition>, <program-note>};

  use projects-implementation,
    exclude: { link-library };
  use lid-projects;
  use registry-projects,
    exclude: { link-library };
  use user-projects;
  use file-source-records;
  use source-records-implementation;
  use build-system, rename: { <linker> => build/<linker> };

  export
    unify-project,
    make-tags,
    show-warnings,
    recompile-library,
    update-library-definitions,
    compile-library-to-models,
    heap-library, emit-library, link-library, 
    type-estimate-library, optimize-library, interpret-project, 
    save-library, save-library-namespace,
    report-library-database-statistics,
    report-recursive-library-database-statistics,
    report-diff-library-database-statistics,
    report-library-heap-statistics,
    report-recursive-library-heap-statistics,
    report-diff-library-heap-statistics,
    link-glue,
    compile-source-record, recompile-source-record,
    emit-source-record, link-source-record,
    run-command-on-sources, run-command-on-source-directories;

  export
    value-in, definition-in, 
    string-complete?, 
    \with-c;

  export
    execute-string;

end module;

define module dfmc-c-file-compiler
  use functional-dylan;
  use dfmc-management;
  use dfmc-execution;
  // use dfmc-testing;
  use dfmc-debug-back-end;
  use dfmc-c-back-end;
  use dfmc-c-linker;
  use dfmc-core;
  use dfmc-imports;
  use registry-projects;
  use dfmc-c-ffi;
end module;
