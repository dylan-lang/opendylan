module:    dylan-user
Synopsis:  The library definition for the HARP library
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define library harp 
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
  use harp-cg-back-end;
  use io;
  use system;
  use collections;
  use source-records;
  use dood;

  export harp;                // The interface for HARP clients

  export harp-for-extenders;  // The interface for specializing - e.g.
                              // by defining back ends

end library;

