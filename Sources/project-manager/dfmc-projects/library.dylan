Module:    dylan-user
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-projects
  use functional-dylan;
  use system;
  use io;
  use collections;

  use memory-manager;
  use build-system;
  use release-info;
  use source-records;
  use file-source-records;
  use tools-interface;
  use dood;

  use dfmc-common;
  use dfmc-browser-support;
  use dfmc-macro-expander;

  use project-protocols;

  export dfmc-projects;
end library dfmc-projects;
