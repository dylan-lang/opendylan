Module:       sockets-tests-server
Author:       Toby Weinberg, Jason Trenouth
Synopsis:     TCP daytime server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $tcp-daytime-port = 13;

define method tcp-daytime-server () => ();
  start-sockets();
  with-server-socket(my-server, port: $tcp-daytime-port)
    start-server(my-server, reply-socket)
      block ()
        write-line(reply-socket, human-readable-date-string(current-date()));
        close(reply-socket);
      exception (non-fatal-condition :: <recoverable-socket-condition>)
        // Close the socket but don't try to force out any unwritten buffers
        // since the connection may not be working properly.
        close (reply-socket, abort?: #t);
      end block;
    end start-server;
  end with-server-socket;
end method;

register-server("TCP Daytime", tcp-daytime-server);

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

