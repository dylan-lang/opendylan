Module:    dylan-user
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-build

  use functional-dylan;
  use operating-system;
  use file-system;
  use streams;
  use standard-io;
  use format-out;
  use format;
  use file-source-records;
  use locators;

  export dylan-build;
  export path-utilities;

end library;

define module path-utilities
  use functional-dylan;
  use locators;
  use file-system;

  export 
    ensure-directory-name,
    ensure-directory-exists,
    filename-with-extension,
    filename-without-extension,
    parent-directory,
    subdirectory-locator;
end module;


define module dylan-build

  use functional-dylan;
  use threads;
  use operating-system;
  use file-system;
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
    <build-error>;

end module;
