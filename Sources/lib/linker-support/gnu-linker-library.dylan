Module:    dylan-user
Synopsis:  GNU Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gnu-linker
  use functional-dylan;
  use system;
  use io;
  use build-system;
  use linker-support;
  use coff-builder;

  export gnu-linker;
end library;

define module gnu-linker
  use functional-dylan;
  use threads;
  use operating-system;
  use file-system;
  use streams;
  use format;
  use locators;
  use build-system;
  use path-utilities;
  use linker-support;
  use coff-builder;

  export
    <gnu-linker>;
end module;
