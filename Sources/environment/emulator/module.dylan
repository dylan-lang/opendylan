Module:    Dylan-User
Synopsis:  Emulator Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module emulator-environment
  use environment-imports;
  use date;
  use file-system;

  use variable-search,
    import: { locate-variable };

  use capi-duim;
  use duim-internals,
    exclude: { position,
	       get-property,
	       \put-property!, do-put-property!,
	       \remove-property!, do-remove-property!,
	       remove-keywords, \with-keywords-removed };

  use editor-manager;

  use sectionizer;
  use environment-protocols;
  use environment-framework;
  use environment-deuce;
  use environment-tools, export: all;
  use emulator-environment-backend;
end module emulator-environment;
