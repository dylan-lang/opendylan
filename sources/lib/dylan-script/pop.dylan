Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Example: #pop://keith:pwd@functionalobjects.com

define class <pop-locator> (<locator>)
  slot locator-host :: <byte-string>,
    required-init-keyword: host:;
  slot locator-port :: <integer> = 110,
    init-keyword: port:;
  slot locator-login-name,
    required-init-keyword: login-name:;
  slot locator-login-password,
    required-init-keyword: login-password:;
end class;

define method pop-parser 
    (string :: <byte-string>) => (locator :: <pop-locator>)
  assert(subsequence-position(string, "//") == 0,
         "POP URL begins //");
  let host-start = subsequence-position(string, "@");
  assert(host-start, "POP URL contains @");
  let name-and-pwd = copy-sequence(string, start: 2, end: host-start);
  let pwd-start = subsequence-position(name-and-pwd, ":");
  assert(pwd-start, "POP URL contains : for password");
  let name = copy-sequence(name-and-pwd, end: pwd-start);
  let pwd = copy-sequence(name-and-pwd, start: pwd-start + 1);
  let host = copy-sequence(string, start: host-start + 1);
  make(<pop-locator>, 
       host: host,
       login-name:, name,
       login-password: pwd);
end method;

define class <pop-message> (<message>)
  slot message-pop-id :: <integer>,
    required-init-keyword: id:;
  slot message-bytes :: <integer>,
    required-init-keyword: bytes:;
end class;

define method print-object (message :: <pop-message>, stream :: <stream>) => ()
  if (*print-escape?*)
    next-method();
  else
    write-text(stream, message-header(message));
    new-line(stream);
    write-text(stream, message-body(message));
  end;
end method;

define method contents
    (locator :: <pop-locator>, #key max-messages = #f)
 => (contents)
   start-sockets();
   with-pop-stream (stream to locator-host(locator))
     pop-login
       (stream, locator-login-name(locator), locator-login-password(locator));
     let entries = read-pop-list(stream);
     collecting ()
       for (entry in entries, i from 0, until: max-messages & i > max-messages)
         let header = read-pop-header(stream, pop-list-entry-id(entry));
         let body = read-pop-body(stream, pop-list-entry-id(entry));
         collect(make(<pop-message>,
                      id: pop-list-entry-id(entry),
                      bytes: pop-list-entry-bytes(entry),
                      header: header,
                      body: body));
       end;
     end;
   end;
end method;

define method find 
    (pattern :: <object>, container :: <pop-message>, #rest options, #key return)
 => (result)
  apply(find, pattern, 
        concatenate(message-header(container), "\n", message-body(container)), 
        options);
end method;

// Pseudo streams

define open class <generator-stream> (<basic-stream>)
end class;

define open generic generate-next-element
    (stream :: <generator-stream>) => (element :: <object>);

define method read-element
    (stream :: <generator-stream>, #key on-end-of-stream)
 => (element :: <object>)
  if (stream-at-end?(stream))
    on-end-of-stream
  else
    generate-next-element(stream);
  end;
end method;

define class <message-stream> (<generator-stream>) 
  slot stream-socket,
    required-init-keyword: socket:;
  slot stream-pop-list,
    required-init-keyword: pop-list:;
end class;

define method generate-next-element
    (stream :: <message-stream>) => (message :: <pop-message>)
  let socket = stream-socket(stream);
  let pop-list = stream-pop-list(stream);
  let entry = head(pop-list);
  let header = read-pop-header(socket, pop-list-entry-id(entry));
  let body = read-pop-body(socket, pop-list-entry-id(entry));
  let message = make(<pop-message>,
                     id: pop-list-entry-id(entry),
                     bytes: pop-list-entry-bytes(entry),
                     header: header,
                     body: body);
  stream-pop-list(stream) := tail(stream-pop-list(stream));
  message
end method;

define method stream-at-end? 
    (stream :: <message-stream>) => (well? :: <boolean>)
  empty?(stream-pop-list(stream))
end method;

define method open
    (locator :: <pop-locator>, #key) => (stream :: <message-stream>)
  start-sockets();
  let socket 
    = open-pop-stream(locator-host(locator), port: locator-port(locator));
  pop-login
     (socket, locator-login-name(locator), locator-login-password(locator));
  let entries = read-pop-list(socket);
  make(<message-stream>,
       socket: socket,
       pop-list: entries);
end method;

define method close
    (stream :: <message-stream>, #key) => ()
  close-pop-stream(stream-socket(stream));
end method;

// eof

