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
//  With-socket makes a socket of class <socket> which is an abstract
//  instantiable class which delegates to <TCP-socket>.  Equivalent
//  alternative syntax which specifies the class of socket explicitly
//  is:
//
//    with-socket (client-socket :: <TCP-socket>, ...)
//
//  or
//
//    with-socket (client-socket, protocol: "tcp", ...)  // "TCP", #"TCP"
//

define method daytime-client () => ();
  start-sockets();
  with-socket (client-socket, host: "127.0.0.1",  port: 13)
    format-out("%s\n", read-line(client-socket));
  end with-socket;
end method;

daytime-client();


