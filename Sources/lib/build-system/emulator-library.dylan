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

  export build-system;
  export path-utilities;

end library;

define module path-utilities
  use functional-dylan;
  use locators;
  use file-system,
    rename: { link-target => fs/link-target };

  export 
    filename-with-extension;
end module;


define module build-system

  use functional-dylan;
  use threads;
  use operating-system;
  use file-system,
    rename: { link-target => fs/link-target };
  use streams;
  use standard-io;
  use format-out;
  use format;
  use file-source-records;
  use locators;
  use path-utilities;

  export
    system-install-path,
    system-registry-path,
    system-release-path,
    user-registry-path,
    user-projects-path,
    user-install-path,
    user-build-path,
    build,
    build-system,
    <build-error>,
    <linker>;

  //--- For the projects library, in the emulator
  export
    default-linker, default-linker-setter;
end module;
