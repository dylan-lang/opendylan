module:         dylan-user
synopsis:       The tty user interface for the devel debugger
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library devel-dbg-ui
       use functional-dylan;
       use system;
       use io;
       use debugger-manager;
       export devel-dbg-ui;
end library;

define module devel-dbg-ui
       use functional-dylan;
       use operating-system;
       use streams;
       use standard-io;
       use format-out;
       use format;
       use debugger-manager;
end library;

