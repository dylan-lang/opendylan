Module:    nntp-client
Synopsis:  Thin wrapper around nntp
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parameters.

define variable *debug-nntp* :: <boolean> = #f;

define constant $default-nntp-port :: <integer> = 80;


/// Conditions

define class <nntp-error> (<error>)
  constant slot nntp-error-response :: <string>, 
    required-init-keyword: response:;
end class;

define function check-nntp-response
    (stream :: <stream>) => ()
  let response = read-line(stream);
  when (*debug-nntp*)
    format-out("%s\n", response);
  end;
  assert(size(response) > 3, "Error code missing from NNTP response");
  select (response[0])
    '-' => error(make(<nntp-error>, response: response));
    '+' => #t;
    otherwise => error(make(<nntp-error>, response: response));
  end;
end function;


/// Session-level interface.

// Interface macro.
define macro with-nntp-stream
  { with-nntp-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let nntp-stream = #f;
         block ()
           nntp-stream := open-nntp-stream(?host, ?args);
           let ?variable = nntp-stream;
           ?body
         cleanup
           if (nntp-stream)
             close-nntp-stream(nntp-stream);
           end;
         end; }
end macro;

// Interface function.
define method open-nntp-stream 
    (host, #key port = $default-nntp-port) => (stream :: <stream>)
  let stream = make(<tcp-socket>, host: host, port: port);
  check-nntp-response(stream);
  stream
end method;

// Interface function.
define method close-nntp-stream
    (stream :: <stream>) => ()
  close(stream);
end method;
