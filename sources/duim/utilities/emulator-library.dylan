Module:       Dylan-User
Synopsis:     DUIM utilities, for the emulator
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-utilities
  use functional-dylan;
  use collections;

  //--- For debugging in the emulator only
  use io;

  export duim-utilities;
  export duim-imports;
end library duim-utilities;
