Module:    Dylan-User
Synopsis:  The non-GUI core of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-core
  use environment-protocols,  export: all;
  use environment-reports,    export: all;
  use environment-manager,    export: all;
  use source-control-manager, export: all;

  use dfmc-environment;

  use dfmc-pentium-harp-cg;		// Pentium backend
  use dfmc-harp-browser-support;	// Harp browsing support
  use dfmc-debug-back-end;		// Compiler print methods
  use gnu-linker;			// GNU linking
  use elf-linker;                       // ELF linking
end library environment-core;
