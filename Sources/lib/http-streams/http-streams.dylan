Module:    http-streams
Synopsis:  HTTP streams
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method http-get
    (request :: <string>, #key host = *default-host*, port = 80, version = #f)
 => (string :: <string>)
  let big-string = make(<string>, size: 8000);
  let i = 0;
  let stream = make(<tcp-stream>,
		    direction: #"input-output",
		    port: port, host: host);
  let useragent = "User-Agent: foopy.dylan\r\n";
  let from = "From: craigs@romulus.functionalobjects.com\r\n";
  block ()
    // until (tcp-connection-state-symbol(stream.tcp-connection) = #"established")
    // end;
    // signal("Finger responding");
    if (version)
      write(stream, "GET ");
      write(stream, request);
      write(stream, " HTTP/1.0\r\n");
      write(stream, useragent);
      write(stream, from);
      write-line(stream, "\r\n");
      force-output(stream)
    else
      write-line(stream, concatenate("GET ", request, "\r\n"));
      force-output(stream);
      for (ch = read-element(stream) then read-element(stream),
           while: ch)
        if (ch)
  	  big-string[i] := ch;
	  i := i + 1
        end
      end
    end;
    cleanup
      // close(stream);
  end;
  copy-sequence(big-string, end: i)
end method http-get;


define class <http-stream> (<stream>)
  slot %tcp-stream = #f;
end class <http-stream>;

define method http-port-and-filename (string :: <string>)
  if (subsequence-position(string, "http://"))
    string := copy-sequence(string, start: 7)
  end;
  let separator = find-key(string,
                           method (character)
                             character = '/'
                           end);
  if (separator)
    values(copy-sequence(string, end: separator),
           copy-sequence(string, start: separator))
  end
end method http-port-and-filename;

define method initialize (stream :: <http-stream>, #key location)
  let port = 80;
  let (host, filename) = http-port-and-filename(location);
  let tcp-stream = make(<tcp-stream>,
                        direction: #"input-output",
                        port: port, host: host);
  stream.%tcp-stream := tcp-stream;
  write-line(stream, concatenate("GET ", filename, "\r\n"));
  force-output(stream)
end method initialize;

define method read-line
    (stream :: <http-stream>, #rest keys)
 => (string-or-eof :: <object>, newline? :: <boolean>)
  apply(read-line, stream.%tcp-stream, keys)
end method read-line;

define method write-line 
    (stream :: <http-stream>, elements, #rest keys) => ()
  apply(write-line, stream.%tcp-stream, elements, keys)
end method write-line;

define method force-output (stream :: <http-stream>) => ()
  force-output(stream.%tcp-stream)
end method force-output;

define method close (stream :: <http-stream>, #rest keys) => ()
  apply(close, stream.%tcp-stream, keys)
end method close;

define method print-html-page (location :: <string>, stream :: <stream>)
  let http-stream = make(<http-stream>, location: location);
  block (return)
    while (#t)
      let line = read-line(http-stream, on-end-of-stream: #f);
      unless (line) return() end;
      write-line(stream, line)
    end
  cleanup
    close(stream)
  end
end method print-html-page;
