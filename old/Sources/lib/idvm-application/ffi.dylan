module: idvm-application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// C-FFI SUPPORT

define C-callable-wrapper C-load-code-from of wrapped-load-code-from-tcp-stream
  c-name: "load_code_from_tcp_stream";
end C-callable-wrapper;

// `send-char' needs to be defined in two steps because 
// "socket-streams/socket-streams.dylan" has declared it to be a 
// generic function, and `define C-function' now creates a function
// instead of adding a method.   -- DNG 12/28/95
define C-function %send-char
  parameter s :: <C-character>;
  result    code :: <C-void>;
  c-name: "send_char";
end C-function;

define method send-char(char) => ();
  %send-char(char)
end method send-char;

define C-function flush-socket
  c-name: "flush_socket";
end C-function;

define C-function read-command
  result    command :: <C-string>;
  c-name: "read_command";
end C-function;

define method send-string(s :: <string>)
 for (c in s)
  send-char(c);
 end for;
end method send-string;
