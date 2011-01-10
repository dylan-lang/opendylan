Module:    dylan-user
Synopsis:  Definition of the library and module
Author:    Seth LaForge
Synopsis:  OLE typeinfo walker and Dylan client stub generator
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library motley
  export motley;
  use functional-dylan;
  use com;
  use ole-automation;
  use win32-kernel;
  use win32-common;
  use win32-registry;
  use io;
  use c-ffi;
  use system;
  use tools-interface;
end library motley;

define module motley
  export get-registry-type-libraries, <registry-type-library-info>, 
	 registry-type-library-name, registry-type-library-path;
  export get-type-library-short-name;	// DEPRECATED
  export get-type-library-information;

  use functional-dylan;
  use com;
  use ole-automation;
  use win32-kernel;
  use win32-common;
  use win32-registry;
  use file-system;
  use format;
  use format-out;
  use streams;
  use standard-io;
  use c-ffi;
  use operating-system;
  use date;
  use locators;
  use tools-interface;
end module motley;
