Module:    dylan-user
Synopsis:  Linker-Support for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library linker-support
  use functional-dylan;
  use system;
  use io;
  use build-system;

  export linker-support;

end library;

define module linker-support
  use functional-dylan;
  use dylan-extensions;
  use threads;
  use operating-system;
  use file-system;
  use streams;
  use standard-io;
  use format-out;
  use format;
  use locators;
  use build-system;
  use path-utilities;

  export

    <dynamic-linker>,
    imports,

    read-imports, read-exports,
    read-build-exports,
    write-imports, write-exports,
    finalize-imports, finalize-exports,
    write-imports-for-executable,
    write-glue-imports,
    assemble-library, assemble-file,
    global-exports,
    create-imports-file, create-exports-file,
    create-imports-file-for-source,

    $dylan-entry-point,
    $link-script,
    $dylan-support-imports,
    $dylan-imports-asm-file

  ;

end module;
