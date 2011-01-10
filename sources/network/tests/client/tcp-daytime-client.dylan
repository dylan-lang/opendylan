Module:       sockets-tests-client
Author:       Toby Weinberg, Jason Trenouth
Synopsis:     TCP Daytime Client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method tcp-daytime-client () => ()
  start-sockets();
  with-socket (client-socket, host: $local-host,  port: 13)
    test-daytime(client-socket)
  end with-socket;
end method;

register-client("TCP Daytime", tcp-daytime-client);

define method test-daytime (client-socket) => ()
  let server-time = read-line(client-socket);
  let client-time = human-readable-date-string(current-date());
  check("Daytime", daytime-equal, client-time, server-time);
end method;

define method daytime-equal (time1, time2)
  ignoring-tail(time1) = ignoring-tail(time2)
end method;

define method ignoring-tail (time :: <string>)
  let dot = find-key(time, curry(\=, '.'));
  copy-sequence(time, end: dot)
end method;

define constant $months-of-the-year = 
  #[ "Thermidor", // month returned by date library is never 0
     "Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ];

// Convert the date into a string which looks like the usual result
// from other daytime servers.

define method  human-readable-date-string 
    (date :: <date>) => (result :: <string>)
  let (year, month, day, hours, minutes, seconds, day-of-week) =
    decode-date(date);
  let day-as-string = 
    copy-sequence(as(<string>, day-of-week), start: 0, end: 3);
  day-as-string[ 0 ] := as-uppercase(day-as-string[ 0 ]);
  format-to-string("%s %s %d %d.%d.%d %d",
		   day-as-string,
		   $months-of-the-year[month],
		   day, hours, minutes, seconds, year)
end method;

