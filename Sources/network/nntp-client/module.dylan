Module:    dylan-user
Synopsis:  Thin wrapper around nntp
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module nntp-client
  use functional-dylan;
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
