Module:    dylan-user
Synopsis:  Thin wrapper around SMTP
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module ssl-smtp-client
  use common-dylan;
  use format-out;
  use format,
    import: { format };
  use standard-io;
  use regular-expressions;
  use sockets;
  use ssl-sockets;
  use streams;

  export *debug-smtp*;

  export $default-smtp-port;

  export <smtp-error>,
	 <transient-smtp-error>,
	 <permanent-smtp-error>,
	 smtp-error-code,
         $invalid-response-error-code,
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
end module ssl-smtp-client;
