module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sockets
  use dylan;
  use functional-extensions;
  use byte-vector;
  use streams;
  use tcp-streams;
  use standard-io-streams;
  use format;

  export sockets;
end library sockets;


define module sockets
  use dylan;
  use functional-extensions;
  use byte-vector;
  use streams-internals;
  use tcp-streams;
  use standard-io-streams, import: {*standard-input*, *standard-output*};
  use format;

 export <abort-evaluator>,
        <closed-socket>,

        connect-to-server,
	connect-to-application,
	listen-to-application,
	close-application,
	receive-string-from-application,
	send-command,

	*sockets-report?*,
	sockets-report?,
	*remote-application*,
	*remote-application-control*;
end module sockets;
