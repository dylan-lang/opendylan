Module:    dylan-user
Synopsis:  Dylan compiler
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define module dw
  use functional-dylan;
  use dylan-extensions, // JB: TEMPORARY HACK
    exclude: { <module>, <library>, <namespace>, namespace-name, home-library,
	       <ordered-object-set>, <ordered-object-table> }; 
  use dispatch-engine;  // JB: TEMPORARY HACK
  use threads;
  use locators;
  use file-system;
  use operating-system, prefix: "os/";
  use streams;
  use standard-io;
  use format;
  use format-out;
  use dood;
  use dispatch-profiler;
  use projects, rename: { link-library => projects/link-library };
  use projects-implementation;
  use registry-projects;
  use lid-projects;
  use user-projects;
  use release-info;
  use dfmc-common;
  use memory-manager;
  use dfmc-namespace;
  use dfmc-optimization;
  use source-records;
  use file-source-records;
  use dfmc-debug, rename: { link-library => debug/link-library };
  use dfmc-derived-information,
    import: { project-library-definition,
	      library-definition-name };
  use dfmc-shell;

end module;  

