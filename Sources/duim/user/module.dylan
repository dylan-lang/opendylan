Module:       Dylan-User
Synopsis:     DUIM user library
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Useful module for hacking around in listeners...
define module duim-user
  use functional-dylan;
  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;
  use duim;
end module duim-user;

// Useful for getting dirt under the fingernails...
define module duim-internals-user
  use functional-dylan,
    exclude: { position,
	       \without-bounds-checks };
  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;
  use duim-internals;
end module duim-internals-user;
