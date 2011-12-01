module:    dylan-user
Synopsis:  The module definition for the OUTPUT-COFF module
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library coff-manager
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use collections;
  use binary-manager;

  export coff-representation,
         coff-constants,
         coff-sizes,
         coff-writer;
end library;
