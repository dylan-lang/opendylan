Module:    dylan-user
Synopsis:  Thin wrapper around POP3
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module pop-client
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format-out;
  use format;
  use sockets;

  export *debug-pop*;

  export <pop-error>,
	 pop-error-response;

  export \with-pop-stream,
	 open-pop-stream,
	 close-pop-stream,
         pop-login,
         pop-logout,
         read-pop-list,
           <pop-list-entry>,
             pop-list-entry-id,
             pop-list-entry-bytes,
         read-pop-header,
         read-pop-body,
         read-pop-message,
         // delete-pop-message
  ;

  /*
  export \with-pop-header-stream,
  	   open-pop-header-stream,
	   close-pop-header-stream;

  export \with-pop-body-stream,
  	   open-pop-body-stream,
	   close-pop-body-stream;
  */

end module pop-client;
