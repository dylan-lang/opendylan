Module:    Dylan-User
Synopsis:  Library & module definitions for factorial
Author:    Steve Rowley
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library factorial-small
  // Single top-level library, so it exports nothing.
  use common-dylan;
end;

define module factorial
  // Single top-level module, so it exports nothing.
  use common-dylan;
  use simple-io;
  use simple-profiling;
end;
