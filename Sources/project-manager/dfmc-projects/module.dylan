Module:    dylan-user
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-projects
  use functional-dylan;
  use machine-words;
  use machine-word-lowlevel,
    import: { machine-word-unsigned-shift-left, machine-word-unsigned-shift-right };
  use threads;
  // Probably don't need all this, sort it out later...
  use collectors;
  use set;
  use locators;
  use streams;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system;
  use file-system;

  use source-records;
  use memory-manager;
  use build-system;
  use release-info;
  use path-utilities;

  use dood,
    import: { \with-walk-progress };

  use dfmc-common;
  use dfmc-progress-reports;
  use dfmc-project-compilation;
  use dfmc-interactive-execution;
  use dfmc-derived-information,
    import: { project-library-definition,
	      compilation-context-library-name };
  use dfmc-macro-expander; // for template projects

  use project-protocols-internals;
end module dfmc-projects;
