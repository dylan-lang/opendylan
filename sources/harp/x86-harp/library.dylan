module:    dylan-user
Synopsis:  The library definition for X86-HARP
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library x86-harp
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use io;
  use system;
  use harp;
  use dfmc-back-end-protocol;
  use native-harp;
  use harp-coff;
  use gnu-outputter;
  use source-records;
  export 
    x86-harp,
    x86-harp-test;
end library;

