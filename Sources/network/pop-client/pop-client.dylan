Module:    pop-client
Synopsis:  Thin wrapper around POP3
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parameters.

define variable *debug-pop* :: <boolean> = #f;

define constant $default-pop-port :: <integer> = 110;


/// Conditions

define class <pop-error> (<error>)
  constant slot pop-error-response :: <string>, 
    required-init-keyword: response:;
end class;

define function check-pop-response
    (stream :: <stream>) => ()
  let response = read-line(stream);
  when (*debug-pop*)
    format-out("%s\n", response);
  end;
  assert(size(response) > 3, "Error code missing from POP response");
  select (response[0])
    '-' => error(make(<pop-error>, response: response));
    '+' => #t;
    otherwise => error(make(<pop-error>, response: response));
  end;
end function;


/// Session-level interface.

// Interface macro.
define macro with-pop-stream
  { with-pop-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let pop-stream = #f;
         block ()
           pop-stream := open-pop-stream(?host, ?args);
           let ?variable = pop-stream;
           ?body
         cleanup
           if (pop-stream)
             close-pop-stream(pop-stream);
           end;
         end; }
end macro;

// Interface function.
define method open-pop-stream 
    (host, #key port = $default-pop-port) => (stream :: <stream>)
  let stream = make(<tcp-socket>, host: host, port: port);
  check-pop-response(stream);
  stream
end method;

// Interface function.
define method close-pop-stream
    (stream :: <stream>) => ()
  close(stream);
end method;

// Interface function.
define method pop-login
    (stream :: <stream>, 
       login :: <byte-string>, password :: false-or(<byte-string>)) 
 => ()
  format-pop-line(stream, "USER %s", login);
  check-pop-response(stream);
  format-pop-line(stream, "PASS %s", password);
  check-pop-response(stream);
end method;

// Interface function.
define method pop-logout
    (stream :: <stream>) => ()
  format-pop-line(stream, "QUIT");
  check-pop-response(stream);
end method;

define class <pop-list-entry> (<object>)
  constant slot pop-list-entry-id :: <integer>,
    required-init-keyword: id:;
  constant slot pop-list-entry-bytes :: <integer>,
    required-init-keyword: bytes:;
end class;

// Interface function.
define method read-pop-list
    (stream :: <stream>) => (entries :: <sequence>)
  format-pop-line(stream, "LIST");
  check-pop-response(stream);
  let entries = #();
  let line = #f;
  while ((line := read-line(stream)) ~= ".")
    let (id, bytes-start) = string-to-integer(line);
    let bytes = string-to-integer(line, start: bytes-start);
    entries := pair(make(<pop-list-entry>, id: id, bytes: bytes), entries);
  end;
  reverse!(entries);
end method;

// Interface function.
define method read-pop-header
    (stream :: <stream>, id :: <integer>) => (entries :: <sequence>)
  format-pop-line(stream, "TOP %d 0", id);
  check-pop-response(stream);
  let header 
    = with-output-to-string (header)
        let line = #f;
        while ((line := read-line(stream)) ~= ".")
          write-line(header, line);
        end;
      end;
  header
end method;

// Interface function.
define method read-pop-body
    (stream :: <stream>, id :: <integer>) => (entries :: <sequence>)
  format-pop-line(stream, "RETR %d", id);
  check-pop-response(stream);
  let body 
    = with-output-to-string (body)
        let line = #f;
        // Skip the header
        while ((line := read-line(stream)) ~= "")
        end;
        // Grab the body
        while ((line := read-line(stream)) ~= ".")
          write-line(body, line);
        end;
      end;
  body
end method;

// Interface function.
define method read-pop-message
    (stream :: <stream>, id :: <integer>) => (entries :: <sequence>)
  format-pop-line(stream, "RETR %d", id);
  check-pop-response(stream);
  let body 
    = with-output-to-string (body)
        let line = #f;
        while ((line := read-line(stream)) ~= ".")
          write-line(body, line);
        end;
      end;
  body
end method;

define method format-pop-line 
    (stream :: <stream>, template :: <string>, #rest args) => ()
  when (*debug-pop*)
    apply(format-out, template, args);
    format-out("\n");
  end;
  apply(format, stream, template, args);
  write(stream, "\r\n");
end method;

// eof
