module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-management
  use functional-dylan;
  use memory-manager;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-conversion;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-linker;
  use dfmc-typist;
  use dfmc-c-ffi;
  export dfmc-management;
end library;

define module dfmc-management
  use functional-dylan;
  use memory-manager;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-conversion;
  use dfmc-optimization;
  use dfmc-back-end;
  use dfmc-linker;
  use dfmc-typist;
  use dfmc-c-ffi;
  export
    install-project-sources,
    parse-project-sources,
    note-definitions-updated,
    compile-library-from-definitions,
     verify-library-before-compile,
     ensure-library-models-computed,
     ensure-library-models-finished,
     ensure-library-models-checked,
     ensure-library-dfm-computed,
     ensure-library-bindings-checked,
      <binding-defined-but-not-used>,
     interpret-top-level-form,
     unregister-interpreter-transaction,
     ensure-library-interpreted,
     ensure-library-type-estimated,
     ensure-library-optimized,
     ensure-library-heaps-computed,
       compute-install-link-library-heaps,
     ensure-library-glue-linked,
    <abort-compilation>,

    execute-source,
    source-complete?,
    macroexpand-source,
    execute-definition-removal,

    <template-source-record>,

    *retract-models-after-compilation?*,
    *retract-types-after-compilation?*,
    // trace-pass,
    // untrace-pass,
    // untrace-passes,
    // enable-pass,
    // disable-pass,
    // *trace-compilation-passes*,
    *progress-stream*, // remove uses in other libs
    \with-progress-reports, // this is for use by the project manager
    \with-library-progress, // this is for use by the project manager
    do-with-library-progress, // needed for emulator hygiene
    *progress-library*,
    *current-progress*,
    *previous-progress*,
    *library-increment*,
    current-progress,
    progress-line,
    progress-report,
    internal-reporting-setter,
    library-progress-text,
    library-stage-text,
    internal-progress-text,
    library-progress-report,
    library-condition-report,
    conditions-for,

    \timing-compilation-phase,
    do-timing-compilation-phase,
    *dfmc-profile-allocation?*,
    *combine-object-files?*

    ;
    // *always-check-after?*,
    // *always-check-before?*
end module;
