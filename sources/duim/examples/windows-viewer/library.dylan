Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library windows-viewer
  use common-dylan;
  use io;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;
  use win32-dde;

  use duim;

  use windows-hook;

  export windows-viewer;
end library windows-viewer;
