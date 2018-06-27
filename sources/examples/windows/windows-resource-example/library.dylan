Module:    dylan-user
Synopsis:  Windows resource example
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  This library is an application program which serves as a simple
//  example of a Windows program implemented in Dylan using the low-level
//  interface libraries to the Win32 API.

define library windows-resource-example
  use common-dylan;
  use equal-table;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
  use win32-resource-database;
  export example-support;
  export windows-resource-example;
end;

define module example-support
  use common-dylan;
  use equal-table;
  use Win32-common;
  use Win32-user;
  use Win32-kernel;
  use patches, export: all;
  use win32-resource-database;
  export 
    initialize-application,
    create-window,
    create-modal-window,
    show-window,
    message-loop,
    <command-table>,
    add-command,
    register-command-handler,
    register-message-handler,
    register-command-table,
    register-message-table;
end;

define module windows-resource-example
  use common-dylan;
  use Win32-common;
  use Win32-user;
  use Win32-GDI;
  use Win32-kernel;
  use win32-resource-database;
  use example-support;
end;
