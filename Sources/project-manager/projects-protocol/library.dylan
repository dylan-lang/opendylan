Module:    dylan-user
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library projects-protocol
  use functional-dylan;
  use system;
  use io;
  use build-system;
  use release-info;
  use source-records;
  use file-source-records;
  use tools-interface;

  export projects-protocol;
  export project-templates;
  export projects-protocol-internals;
end library projects-protocol;
