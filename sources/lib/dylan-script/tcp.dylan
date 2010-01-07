Module: dylan-script-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <tcp-locator> (<locator>)
  slot locator-host :: false-or(<byte-string>),
    required-init-keyword: host:;
  slot locator-port :: false-or(<integer>),
    required-init-keyword: port:;
end class;

define method tcp-parser 
    (string :: <byte-string>) => (locator :: <tcp-locator>)
  let (host, port, path) = split-url(string);
  assert(~path, "No path specified in a tcp locator");
  make(<tcp-locator>,
       host: ~empty?(host) & host,
       port: port & string-to-integer(port));
end method;

define method locator-as-string
    (type == <string>, locator :: <tcp-locator>)
 => (string :: <byte-string>)
  locator-as-string(<byte-string>, locator)
end method;

define method locator-as-string
    (type == <byte-string>, locator :: <tcp-locator>)
 => (string :: <byte-string>)
   concatenate
     ("tcp://", locator-host(locator) | "", ":", locator-port(locator) | "");
end method;

define method open 
    (locator :: <tcp-locator>, #rest options, #key, #all-keys) 
 => (socket)
  start-sockets();
  let host = locator-host(locator);
  let port = locator-port(locator);
  if (host)
    apply(make, <tcp-socket>, 
                host: host,
                port: port,
                options);
  else
    apply(make, <tcp-server-socket>,
                port: port,
                options);
  end;
end method;
