Module:    dylan-user
Synopsis:  Thin wrapper around nntp
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module nntp-client
  use common-dylan;
  use streams;
  use format-out;
  use format;
  use sockets;

  export *debug-nntp*;

  export <nntp-error>,
	 nntp-error-response;

  export \with-nntp-stream,
	 open-nntp-stream,
	 close-nntp-stream;

end module nntp-client;
