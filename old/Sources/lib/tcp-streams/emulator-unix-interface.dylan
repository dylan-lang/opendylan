Module:      tcp-streams
Synopsis:    An interface to file-related unix system calls for the Dylan emulator.
Author:      Eliot Miranda
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

import-cl-functions(
    system(unix-close)      (as: unix-close),
    system(unix-read)       (as: unix-read),
    system(unix-write)      (as: unix-write),
    system(get-unix-error)  (as: get-unix-error),
    system(errno-value)     (as: unix-errno-value),
    system(connect-to-named-server)   (as: lisp/connect-to-named-server),
    system(connect-to-server-patched) (as: lisp/connect-to-numbered-server)
);

// standard unix error definitions
define constant $e_access = 13;

define method unix-error (syscall :: <string>, #key errno = #f) => ();
  let message :: <string>
    = get-unix-error(if (~ errno) unix-errno-value() else errno end);
    error("~A ~A", syscall, message);
end method unix-error;

define method unix-open
    (host :: <string>, service :: <string>) => (fd :: <integer>)
  lisp/connect-to-named-server(host, service)
end method connect-to-named-server;

define method unix-open
    (host :: <string>, service :: <integer>) => (fd :: <integer>)
  lisp/connect-to-numbered-server(host, service)
end method connect-to-named-server;
