Module:    Dylan-User
Synopsis:  HTTP streams
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module http-streams
  use dylan;
  use streams;
  use format;
  use standard-io-streams;
  use tcp-streams;

  export <http-stream>,
         http-get,
         print-html-page;
end module http-streams;
