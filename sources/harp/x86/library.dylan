module:    dylan-user
Synopsis:  The library definition for HARP-X86
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp-x86
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use harp;
  use dfmc-back-end-protocol;
  use harp-native;
  use harp-coff;
  use gnu-as-outputter;
  use source-records;
  export 
    harp-x86,
    harp-x86-test;
end library;

