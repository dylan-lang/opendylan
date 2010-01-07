Module:    dylan-user
Synopsis:  Define the DOSS library
Author:    Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library doss
  use functional-dylan;
  use set;

  use simple-streams;
  use simple-format;
  use simple-print;

  use io;
  use variable-search;

  export doss, doss-internals;
end library doss;
