Module:    dylan-user
Synopsis:  Dylan Compiler Shell
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library hd2c-dfmc-shell
  use functional-dylan;
  use system;
  use io;
  use memory-manager;
  use variable-search;
  use dood;
  use build-system;
  use projects;
  use registry-projects;
  use user-projects;
  use dfmc-browser-support;

  // Load a back-end
  use dfmc-management;
  use dfmc-namespace;
  use dfmc-definitions;
  use dfmc-conversion;
  use dfmc-optimization;
  use dfmc-execution;
  use dfmc-back-end;
  use dfmc-debug-back-end;
  use dfmc-c-file-compiler;

  // Load the C Compiler & Linker linker
  use ccl-linker;

  use command-shell;

  export dfmc-shell;
end library hd2c-dfmc-shell;
