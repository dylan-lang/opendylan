Module:    dylan-user
Synopsis:  Thin wrapper around SMTP
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module smtp-client
  use functional-dylan;
  use streams;
  use format-out;
  use format;
  use sockets;

  export *debug-smtp*;

  export <smtp-error>,
	 <transient-smtp-error>,
	 <permanent-smtp-error>,
	 smtp-error-response;

  export \with-smtp-stream,
	 open-smtp-stream,
	 close-smtp-stream,
	 write-smtp-from,
	 write-smtp-recipient,
	 write-smtp-data-start,
	 write-smtp-data-end;

  export \with-smtp-message-stream,
	 open-smtp-message-stream,
	 close-smtp-message-stream,
	 send-smtp-message;
end module smtp-client;
