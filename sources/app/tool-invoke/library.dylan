Module:    dylan-user
Synopsis:  A standalone program to invoke Functional Developer tools on 
	   specification files.
Author:    7/98 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library tool-invoke
  use functional-dylan;
  use system;
  use io;
  use collections;
  use tools-interface; 
  
  use motley;
end library tool-invoke;
