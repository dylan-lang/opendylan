Module:       daytime-client
Author:       Toby
Synopsis:     Simple daytime client example sockets code
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//  Interesting values for host: 
//   "gemini.tuc.noao.edu",
//   "webhost.functionalobjects.com",
//   $loopback-address (with daytime-server running) 
//   $local-host (with daytime-server running) 
// 
//  You can also use "service: daytime" instead of "port: 13".
// 

define method daytime-client () => ();
  start-sockets();
  let client-socket = make(<TCP-socket>, host: $local-host,  port: 13);
  block()
    let date-result = read-line(client-socket);
    format-out("%s\n", date-result);
  cleanup
    close(client-socket);
  end block;
end method;

daytime-client();


