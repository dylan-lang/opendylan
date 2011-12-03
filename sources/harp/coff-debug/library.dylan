module:    dylan-user
Synopsis:  The library definition for the COFF debugger
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library coff-debug
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use coff-manager;
  use collections;

  export coff-reader,
         coff-print;
end library;
