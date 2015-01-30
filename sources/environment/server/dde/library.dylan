Module:    Dylan-User
Author:    Hugh Greene
Synopsis:  A DDE-based frontend for the environment server.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-dde-server
  use dylan;
  use common-dylan;

  use channels;
  use environment-manager;
  use environment-server-parsers;

  use c-ffi;
  use win32-common;
  use win32-dde;

  export environment-dde-server;
end library environment-dde-server;

define module environment-dde-server
  use common-dylan;
  use threads;

  use dylan-extensions, import: { <byte-character> };

  use channels;
  use environment-manager;
  use string-parser;

  use c-ffi;
  use win32-common;
  use win32-dde;

  export server-start,
         server-stop,
         <dde-condition>,
         <dde-warning>,
         <dde-serious-condition>,
         *service-and-topic-name*;
end module environment-dde-server;
