Module:    Dylan-User
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module emulator-environment-backend
  use environment-imports;
  use date;
  use file-system;

  use variable-search,
    import: { locate-variable };

  use environment-protocols;

  // For use by emulator-environment
  export <emulator-project>,
         <emulator-database>,
         <emulator-application>;
end module emulator-environment-backend;
