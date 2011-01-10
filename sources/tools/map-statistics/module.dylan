Module:    Dylan-User
Synopsis:  Library for handling Win32 map files
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module map-statistics
  use functional-dylan;
  use simple-format;
  use operating-system;
  use file-system;
  use streams;

  export map-library-breakdown,
         print-library-breakdown;
end module map-statistics;
