Module:    dylan-user
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library build-system
  use functional-dylan;
  use system;
  use io;
  use file-source-records;
  use release-info;
  use coff-builder;

  export
    build-system,
    path-utilities;
end library build-system;

define module path-utilities
  use functional-dylan;
  use locators;
  use file-system,
    rename: { link-target => fs/link-target};

  export 
    filename-with-extension,
    filename-with-extensions,
    new-filename-extension,
    file-in-directory;
end module path-utilities;

define module build-system

  use functional-dylan;
  use dylan-primitives;
  use threads;
  use operating-system;
  use file-system,
    rename: { link-target => fs/link-target };
  use streams-internals;
  use standard-io;
  use format-out;
  use format;
  use file-source-records;
  use locators;
  use path-utilities;
  use settings;
  use release-info;

  use coff-builder, exclude: { current-position, current-position-setter  };

  export
    <build>,
    build,
    build-system,
    build-error,

    <build-error>,
    <linker>,

    *build*,
    *builds-cache*,
    *linker*,
    *link-stream*,

    *dll-installation*, *lib-installation*,

    // slot getters
    dylanapp, dylanlib, libfile, dll, app, objs, libs, 
    c-objs, c-src-objs, rc-objs, c-libs, rtlibs,
    base-address, linkopts, image-version,
    exports-only?,
    next-phase,
    linkable?,
    targets,
    build-directory, %build-directory,
    link?, link?-setter,
    force?,
    build-main-object?,
    library-file-name,
    library-filename-with-extension,
    library-filename-with-extensions,

    next-target, shift-targets,
    default-linker, default-linker-setter,
    default-dll-policy, default-dll-policy-setter,
    object-filename-from-base-filename,
    make-linker, check-linker-installation,
    execute-shell-command,
    do-link-dll, do-link-exe,
    do-install-dll,
    execute-shell-commands, read-linker,
    exports-file-extension,
    dll-file-extension,
    exe-file-extension,
    search-for-file,
    make-command-line,
    echo, echo?,
    uninstall,
    newer-file?,
    substitute-environment-variables,
    build-environment-variable, build-environment-variable-setter,
    linker-library-name, linker-resource-object-file,
    linker-executable-keyword,
    linker-library-path-name,
    linker-path-separator,
    do-dll-unification,
    do-builds-target,
    make-build-libraries,
    maybe-install,
    check-build-dependencies,
    check-build-dependencies-target,
    do-unify-dll, do-unify-exe,
    process-builds-for-unification,
    create-archive,
    system-build?,
    install-dylanmakefile,
    fake-imports,
    read-build-imports, read-imports-in-file,
    build-linker-imports,
    write-fake-imports,
    open-import-fixups-outputter,
    write-import-fixups, write-build-import-fixups,
    output-coff-builder-footer,
    dylan-library?, runtime-library?,
    linker-mangle,
    merged-project-name,
    merged-project-libraries,
    build-up-to-date?,
    configure-build-system,
    note-library-modified,
    copy-build-file,
    c-compile,
    link-objects,
    link-runtime-libraries,
    static-linking?,
    link-stream-class,

    $personal-lib,
    $personal-bin,
    $system-lib,
    $system-bin,
    $system-build,
    $release-root,
    $gnu-linker,
    $ccl-linker,
    $elf-linker,
    $override-default-linker,
    $override-default-dll-policy,
    $dylan-executable-name,
    $dylan-support,

    system-install-path,
    system-registry-path,
    system-release-path,
    system-build-path,
    user-registry-path,
    user-projects-path,
    user-projects-path-setter,
    user-install-path,
    user-build-path,

    \with-stream-input, \with-build-directory,
    \with-import-fixups;
end module build-system;
