Module:    Dylan-User
Synopsis:  Library & module definitions for factorial
Author:    Steve Rowley
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library factorial-big
  // Single top-level library, so it exports nothing.
  use common-dylan;
  use generic-arithmetic;
  use big-integers;
end;

define module factorial
  // Single top-level module, so it exports nothing.
  use generic-arithmetic-common-dylan;
  use simple-io;
  use simple-profiling;
end;
