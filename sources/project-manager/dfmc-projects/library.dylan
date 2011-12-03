Module:    dylan-user
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-projects
  use dylan;
  use common-dylan;
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

  use projects-protocol;

  export dfmc-projects;
end library dfmc-projects;
