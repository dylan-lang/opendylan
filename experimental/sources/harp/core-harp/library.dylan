module:    dylan-user
Synopsis:  The library definition for the HARP library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp 
  use functional-dylan;
  use generic-arithmetic;
  use big-integers;
  use harp-cg-back-end;
  use io;
  use locators;
  use collections;
  use source-records;
  use dood;

  export harp;                // The interface for HARP clients

  export harp-for-extenders;  // The interface for specializing - e.g.
                              // by defining back ends

end library;

