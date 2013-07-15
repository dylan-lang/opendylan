Module:       daytime-client
Author:       Toby
Synopsis:     Simple daytime client example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//  Interesting values for host:
//   "gemini.tuc.noao.edu",
//   "webhost.functionalobjects.com",
//
//  You can also use "service: daytime" instead of "port: 13".
//

define method daytime-client () => ();
  start-sockets();
  let client-socket = make(<TCP-socket>, host: "127.0.0.1",  port: 13);
  block()
    let date-result = read-line(client-socket);
    format-out("%s\n", date-result);
  cleanup
    close(client-socket);
  end block;
end method;

daytime-client();


