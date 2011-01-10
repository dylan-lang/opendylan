Module:    dylan-user
Synopsis:  Dylan Compiler Shell
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module dfmc-shell
  use functional-dylan;
  use dylan-extensions, import: {raw-as-integer, integer-as-raw};
  use dylan-primitives;
  use threads;
  use locators,
    exclude: { subdirectory-locator };
  use file-system;
  use operating-system, prefix: "os/";
  use streams;
  use standard-io;
  use format;
  use format-out;
  use variable-search;
  use dood;
  use build-system,
    import: { system-registry-path,
              user-registry-path, user-build-path, user-install-path,
              default-build-script,
	      $override-default-linker, $override-default-dll-policy };
  use projects, rename: { link-library => projects/link-library };
  use projects-implementation;
  use registry-projects;
  use lid-projects;
  use user-projects;
  use memory-manager;
  use dfmc-namespace;
  use dfmc-definitions,
    import: { *heap-statistics?* };
  use dfmc-back-end;
  use dfmc-conversion,
    import: { *strip-enabled?* };
  use dfmc-optimization;
  use dfmc-runtime-execution,
    import: { interpreter-transaction-value };
  use dfmc-management, 
    import: { internal-reporting-setter };
  use dfmc-debug, rename: { link-library => debug/link-library };
  use dfmc-derived-information,
    import: { project-library-definition,
	      library-definition-name };

  use command-shell, export: all;

  export

    *trace-asm?*,

    $open-command,
    $import-command,
    $close-command,
    $close-all-command,
    $build-command,

    $parse-command,
    $link-command,
    $collect-garbage-command,
    $room-command,
    $profile-command,
    $break-command, $breaks-command,
    $clear-command, $clear-all-command,
    $compile-library-command,
    $watchpoint-class-command,
    $set-watchpoint-class-command,
    $watchpoint-dood-command,
    $set-watchpoint-dood-command,
    $set-dood-number-of-buffers-command,
    $set-dood-buffer-size-command,
    $dood-number-of-buffers-command,
    $dood-buffer-size-command,
    $trace-optimizations-command,
    $untrace-optimizations-command,
    $dump-dfm-command,
    $undump-dfm-command,
    $enable-stripping-command,
    $disable-stripping-command,
    $interpret-library-command,
    $interpret-command,
    $interpret-top-level-command,
    $watchpoint-dood-command,
    $statistics-command,
    $heap-statistics-command,
    $recursive-heap-statistics-command,
    $enable-heap-statistics-command,
    $disable-heap-statistics-command,
    $enable-inlining-command,
    $disable-inlining-command,
    $enable-call-upgrading-command,
    $disable-call-upgrading-command,
    $recursive-statistics-command,
    $diff-statistics-command,
    $save-command,
    $save-namespace-command,
    $flush-command,
    $build-locations-command,
    $find-library-command,
    $all-open-projects-command,
    $registries-command,
    $update-libraries-command,
    $run-command-on-sources-command,
    $run-command-on-source-directories-command,
    $unify-command,
    $tags-command,
    $warnings-command,
    $remove-build-products-command,

    project-not-found-handler, \with-project-location-handler,
    open-project-from-name, ensure-project-open,
    link-library-with-options
  ;

  export
    interpret-library-command,				    
    interpret-string-command,				    
    interpret-top-level-command;

end module;  

