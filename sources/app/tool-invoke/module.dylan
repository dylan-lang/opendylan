Module:    dylan-user
Synopsis:  A standalone program to invoke Functional Developer tools on 
	   specification files.
Author:    7/98 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module tool-invoke
  use common-dylan, exclude: { format-to-string };
  use operating-system;
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use simple-random;
  use locators;
  use tools-interface;
end module tool-invoke;
