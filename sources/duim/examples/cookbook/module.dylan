Module:       dylan-user
Author:       Andy Armstrong, Scott McKay
Synopsis:     DUIM example code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module duim-examples
  use common-dylan;
  use format;
  use format-out;
  use operating-system;

  use duim-internals,
    exclude: { command-arguments };

  // The start up function
  export start-examples;
end module duim-examples;
