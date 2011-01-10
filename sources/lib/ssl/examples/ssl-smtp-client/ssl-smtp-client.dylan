Module:    ssl-smtp-client
Synopsis:  Thin wrapper around SMTP
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Parameters.

define variable *debug-smtp* :: <boolean> = #t;

define constant $default-smtp-port :: <integer> = 25;

define constant $invalid-response-error-code :: <integer> = -1;

/// Conditions

define abstract class <smtp-error> (<error>)
  constant slot smtp-error-code :: <integer>,
    required-init-keyword: code:;
  // Note that the response usually contains the error code at the
  // beginning of the string.
  constant slot smtp-error-response :: <string>, 
    required-init-keyword: response:;
end class;

define method condition-to-string
    (cond :: <smtp-error>)
 => (string :: false-or(<string>))
  format-to-string("%=: %s", cond.object-class, cond.smtp-error-response)
end;

define sealed class <transient-smtp-error> (<smtp-error>) end;
define sealed class <permanent-smtp-error> (<smtp-error>) end;

define constant $whitespace-regex = compile-regex("[ \t]+");

define function check-smtp-response
    (stream :: <stream>) => ()
  let response = read-line(stream);
  when (*debug-smtp*)
    format-out("checking %s\n", response);
    force-output(*standard-output*);
  end;
  let (code, message) = apply(values, split(response, $whitespace-regex, count: 2));
  when (*debug-smtp*)
    format-out("code mess %s %s\n", code, message);
    force-output(*standard-output*);
  end;
  let code = block ()
               string-to-integer(code);
             exception (ex :: <serious-condition>)
               let msg = format-to-string("Invalid response from SMTP server: %s",
                                          response);
               error(make(<permanent-smtp-error>,
                          code: $invalid-response-error-code,
                          response: msg));
             end;
  when (*debug-smtp*)
    format-out("code %d\n", code);
    force-output(*standard-output*);
  end;
  if (code >= 500 & code <= 599)
    error(make(<permanent-smtp-error>, code: code, response: response));
  elseif (code >= 400 & code <= 499)
    error(make(<transient-smtp-error>, code: code, response: response));
  else
    #t;   // OK
  end;
end function check-smtp-response;


/// Session-level interface.

// Interface macro.
define macro with-smtp-stream
  { with-smtp-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let smtp-stream = #f;
         block ()
           smtp-stream := open-smtp-stream(?host, ?args);
           let ?variable = smtp-stream;
           ?body
         cleanup
           if (smtp-stream)
             close-smtp-stream(smtp-stream);
           end;
         end; }
end macro;

// Interface function.
define method open-smtp-stream 
    (host, #key port = $default-smtp-port, ssl? = #t) => (stream :: <stream>)
  let helo-name = host-name($local-host);
  let stream = make(<tcp-socket>, host: host, port: port);
  check-smtp-response(stream);
  format-smtp-line(stream, "HELO %s", helo-name);
  check-smtp-response(stream);
  if (ssl?)
    format-smtp-line(stream, "STARTTLS");
    check-smtp-response(stream);
    make(<ssl-socket>, lower: stream)
  else
    stream
  end
end method;

// Interface function.
define method close-smtp-stream
    (stream :: <stream>) => ()
  format-smtp-line(stream, "QUIT");
  check-smtp-response(stream);
  close(stream);
end method;

// Interface function.
define method write-smtp-from
    (stream :: <stream>, from :: <string>) => ()
  format-smtp-line(stream, "MAIL FROM: %s", from);
  check-smtp-response(stream);
end method;

// Interface function.
define method write-smtp-recipient
    (stream :: <stream>, to :: <string>) => ()
  format-smtp-line(stream, "RCPT TO: %s", to);
  check-smtp-response(stream);
end method;

// Interface function.
define method write-smtp-data-start
    (stream :: <stream>) => ()
  format-smtp-line(stream, "DATA");
  check-smtp-response(stream);
end method;

// Interface function.
define method write-smtp-data-end
    (stream :: <stream>) => ()
  format-smtp-line(stream, ".");
  check-smtp-response(stream);
end method;

define method format-smtp-line 
    (stream :: <stream>, template :: <string>, #rest args) => ()
  when (*debug-smtp*)
    format-out("send ");
    apply(format-out, template, args);
    format-out("\n");
    force-output(*standard-output*);
  end;
  apply(format, stream, template, args);
  write(stream, "\r\n");
end method;


/// Message-level interface.

// Interface macro.
define macro with-smtp-message-stream
  { with-smtp-message-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let smtp-stream = #f;
         block ()
           smtp-stream := open-smtp-message-stream(host: ?host, ?args);
           let ?variable = smtp-stream;
           ?body
         cleanup
           if (smtp-stream)
             close-smtp-message-stream(smtp-stream);
           end;
         end; }
end macro;

// Interface function.
define method open-smtp-message-stream
    (#key host, port = $default-smtp-port, 
          from :: <string>, recipients :: <sequence>)
 => (stream :: <stream>)
  assert(host, "Host required in 'open-smtp-message-stream'");
  let stream = open-smtp-stream(host, port: port);
  write-smtp-from(stream, from);
  for (recipient :: <string> in recipients)
    write-smtp-recipient(stream, recipient);
  end;
  write-smtp-data-start(stream);
  stream
end method;

// Interface function.
define method close-smtp-message-stream
    (stream :: <stream>) => ()
  write-smtp-data-end(stream);
  close-smtp-stream(stream);
end method;

// Interface function.
define method send-smtp-message 
    (#key host, port = $default-smtp-port, 
          from :: <string>, recipients :: <sequence>, 
          body :: <string>)
  assert(host, "Host required in 'send-smtp-message'");
  with-smtp-message-stream (stream to host,
			    port: port, from: from, recipients: recipients)
    let line-start :: <integer> = 0;
    for (i from 0 below size(body))
      let c = body[i];
      if (c == '\n')
        write(stream, body, start: line-start, end: i);
        write(stream, "\r\n");
        line-start := i + 1;
      end;
    finally
      if (line-start ~== i)
        write(stream, "\r\n");        
      end;
    end;
  end;
end method;


/// Sample code


define method test () => ()
  let recipients = #["keith@functionalobjects.com"];
  // Send a string...
  send-smtp-message
    (host: "localhost",
     port: 1025,
     from: "keith@functionalobjects.com",
     recipients: recipients,
     body: "To: ...\nSubject: SMTP Test\n\nTest\nFrom\nTest\n");
  // Send using a stream...
  with-smtp-message-stream (stream to "mailhost",
			    from: "keith@functionalobjects.com",
			    recipients: recipients)
    // Header
    format(stream, "To: %s\r\n", join(recipients, ", "));
    format(stream, "Subject: SMTP Stream Test\r\n");
    format(stream, "\r\n");
    // Body
    for (i from 1 to 10)
      format(stream, "Line %d\r\n", i);
    end;
  end;
end method test;

begin
  start-sockets();
  test();
end;

