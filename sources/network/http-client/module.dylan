Module:    dylan-user
Synopsis:  Thin wrapper around POP3
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module http-client
  use functional-dylan;
  use streams;
  use format-out;
  use format;
  use sockets;

  export *debug-http*;

  export <http-error>,
	 http-error-response;

  export \with-http-stream,
	 open-http-stream,
	 close-http-stream,

         write-http-get,
         read-http-response-header-as,
           read-http-response-header;

end module http-client;
