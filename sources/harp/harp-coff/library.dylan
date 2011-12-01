module:    dylan-user
Synopsis:  The library definition for the HARP-COFF library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp-coff
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use collections;
  use io;
  use coff-manager;
  use coff-builder;
  use binary-outputter;
  use harp;

  export cv4-builder,
         harp-coff;
end library;
