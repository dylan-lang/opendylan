Module:    dylan-user
Synopsis:  Wrapper for executing .dll projects.
Author:    1998/7/31 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dll-wrap
  use common-dylan;
  use system;
  use io;
  use win32-kernel;
end library dll-wrap;
