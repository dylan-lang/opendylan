module:    dylan-user
Synopsis:  The library definition for POWERPC-HARP
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library powerpc-harp
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use harp;
  use dfmc-back-end-protocol;
  use native-harp;
  use harp-coff;
  use gnu-outputter;
  use source-records;
  export 
    powerpc-harp,
    powerpc-harp-test;
end library;

