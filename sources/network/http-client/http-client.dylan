Module:    http-client
Synopsis:  Thin wrapper around http
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parameters.

define variable *debug-http* :: <boolean> = #f;

define constant $default-http-port :: <integer> = 80;


/// Conditions

define class <http-error> (<error>)
  constant slot http-error-response :: <string>, 
    required-init-keyword: response:;
end class;

define function check-http-response
    (stream :: <stream>) => ()
  let response = read-line(stream);
  when (*debug-http*)
    format-out("%s\n", response);
  end;
  assert(size(response) > 3, "Error code missing from HTTP response");
  select (response[0])
    '-' => error(make(<http-error>, response: response));
    '+' => #t;
    otherwise => error(make(<http-error>, response: response));
  end;
end function;


/// Session-level interface.

// Interface macro.
define macro with-http-stream
  { with-http-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let http-stream = #f;
         block ()
           http-stream := open-http-stream(?host, ?args);
           let ?variable = http-stream;
           ?body
         cleanup
           if (http-stream)
             close-http-stream(http-stream);
           end;
         end; }
end macro;

// Interface function.
define method open-http-stream 
    (host, #key port = $default-http-port) => (stream :: <stream>)
  let stream = make(<tcp-socket>, host: host, port: port);
  stream
end method;

// Interface function.
define method close-http-stream
    (stream :: <stream>) => ()
  close(stream);
end method;

// Interface function.
define method write-http-get
    (stream :: <stream>, 
       host :: <byte-string>, path :: <byte-string>, #rest headers)
 => ()
  format-http-line(stream, "GET %s HTTP/1.1", path);
  format-http-line(stream, "Host: %s", host);
  for (i from 0 below size(headers) by 2)
    let key = headers[i];
    let val = headers[i + 1];
    format-http-line(stream, "%s: %s", key, val);
  end;
  format-http-line(stream, "");
end method;

// Interface function.
define method read-http-response-header
    (stream :: <stream>) => ()
  read-http-response-header-as(<string>, stream);
end method;

// Interface function.
define method read-http-response-header-as
    (type :: subclass(<string>), stream :: <stream>) => ()
  with-output-to-string (string-stream)
    let line = #f;
    while ((line := read-line(stream)) ~= "")
      if (*debug-http*)
        format-out("%s\n", line);
      end;
      write-line(string-stream, line);
    end;
  end;
end method;

define method format-http-line 
    (stream :: <stream>, template :: <string>, #rest args) => ()
  when (*debug-http*)
    apply(format-out, template, args);
    format-out("\n");
  end;
  apply(format, stream, template, args);
  write(stream, "\r\n");
end method;

// eof
