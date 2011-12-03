Module: dylan-user
Synopsis: Test application for debugger
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module checkmate
  use common-dylan, exclude: { format-to-string };
  use big-integers, prefix: "generic-";
  use streams;
  use standard-io;
  use format;
  use operating-system;
  use duim;
  use threads;
  use c-ffi;
  use win32-common, rename: {<point> => win32/<point>};
end module;


