module:   dylan-user
author:   Paul Haahr
synopsis: Library and module definitions for a sealing prototyping world.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sealing-workbench
  use functional-dylan;
  use streams;
  use streams-format;
  export sealing-workbench;
end library sealing-workbench;

define module sealing-workbench
  use functional-dylan,
    rename: { error => dylan/error, signal => dylan/signal };
  use streams, exclude: { name };
  use streams-format;
  use streams-printing;
  use standard-io-streams;
end module sealing-workbench;
