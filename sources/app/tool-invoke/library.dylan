Module:    dylan-user
Synopsis:  A standalone program to invoke Open Dylan tool plugins, good for
           use while developing new plugins.
Author:    7/98 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library tool-invoke
  use collections;
  use common-dylan;
  use io;
  use system;
  use tools-interface;

  // Replace this with your plugin library when developing a new plugin.
  use protobuf-tool;
end library;

define module tool-invoke
  use common-dylan;
  use format-out;
  use locators;
  use operating-system;
  use standard-io;
  use streams;
  use tools-interface;
end module;
