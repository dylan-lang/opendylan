module:    dylan-user
author:    Keith Dennison
synopsis:  Module definition for the test profiler
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module test-profiler
  use dylan;
  use streams;
  use debugger-manager;
  use format;
  use format-out;
  use standard-io;
  use operating-system;
  use table-extensions;
end module;
