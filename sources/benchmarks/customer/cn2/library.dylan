Author:	  Bruce Tobin ([btobin@infinet.com)
Synopsis: A simple version of the CN2 rule induction algorithm
Module:   dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library cn2
  use common-dylan;
  use io;

  export cn2;
end library cn2;

define module cn2
  use common-dylan;
  use dylan-extensions, import: { without-bounds-checks };
  use streams;
  use format;
  use format-out;
  use transcendentals;
  use simple-profiling;

  export load-cn2, run-cn2, cn2;
end module cn2;
