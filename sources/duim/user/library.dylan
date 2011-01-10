Module:       Dylan-User
Synopsis:     DUIM user library
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A DUIM user to make it easy to write sample code
define library duim-user
  use functional-dylan;

  // Commonly useful libraries
  use io;

  // Use all of DUIM, of course
  use duim;

  export duim-user;
end library duim-user;
