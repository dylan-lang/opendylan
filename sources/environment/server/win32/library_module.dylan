Module:    Dylan-User
Author:    Hugh Greene
Synopsis:  Controlling the Environment from external sources.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library environment-server
  use common-dylan;
  use win32-user;
  use win32-common;
  use win32-kernel;

  use channels;
  use environment-manager;

  use environment-dde-server;
/* ---*** These could be added later:
  use environment-ole-automation-server;
  use environment-tcp-server;  // For Unix.
*/

  export environment-server;
end library environment-server;


/// All external servers, plus commands and the call hooks.

define module environment-server
  use common-dylan;
  use win32-user;
  use win32-common;
  use win32-kernel;
  use channels;
  use environment-manager;

  use environment-dde-server,
	prefix: "dde/",
	export: { dde/<dde-condition>, dde/<dde-error>,
		  dde/*service-and-topic-name* };

  export server-start,
	 server-stop;
end module environment-server;
