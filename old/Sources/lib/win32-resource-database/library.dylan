Module:    dylan-user
Synopsis:  library definition
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-resource-database
  use functional-dylan;
  use Collections;
  use System;
  use IO;
  use Win32-common;
  use Win32-user;
  use Win32-kernel;
  export win32-resource-database;
  export patches;
end;

define module win32-resource-database
  use functional-dylan;
  create
    <resource>,
    resource-id,
    <window-resource>,
    window-class,
    window-position, 
    window-size,
    <top-window-resource>,
    number-of-gadgets,
    <predefined-resource-type>,
    <resource-type>, resource-type,
    <resource-id>,
    encode-resource,
    decode-resource,
    load-default-resources,
    lookup-resource,
    lookup-dialog,
    lookup-control,
    describe-database,
    describe-resource;
end;


define module win32-resource-database-internal
  use functional-dylan;
  use table-extensions;
  use Machine-integer-user;
  use streams;
  use format;
  use print;
  use c-ffi;
  use Win32-common;
  use Win32-user;
  use Win32-kernel;
  use win32-resource-database;
  export debug-out, print-last-error, ErrorHandler;
end;

define module patches
  use win32-resource-database-internal,
    export: {debug-out, print-last-error, ErrorHandler};
end;
