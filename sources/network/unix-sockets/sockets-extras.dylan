Module:       unix-sockets
Synopsis:     A couple of convenience wrappers for use with Dylan streams
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define simple-C-mapped-subtype <C-buffer-offset> (<C-char*>)
  export-map <machine-word>, export-function: identity;
end;

define C-struct <timeval>
  slot tv-sec-value :: <C-int>;
  slot tv-usec-value :: <C-int>;
  c-name: "timeval";
end;

define inline-only C-function unix-recv-buffer
  parameter socket :: <C-int>;
  parameter buffer :: <C-buffer-offset>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "recv";
end C-function;

define inline-only C-function unix-send-buffer
  parameter socket :: <C-int>;
  parameter buffer :: <C-buffer-offset>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "send";
end C-function;

define inline-only C-function unix-recv-buffer-from
  parameter socket :: <C-int>;
  parameter buffer :: <C-buffer-offset>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t*>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "recvfrom";
end C-function;

define inline-only C-function unix-send-buffer-to
  parameter socket :: <C-int>;
  parameter message :: <C-buffer-offset>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  parameter dest-addr :: <sockaddr*>;
  parameter dest-len :: <socklen-t>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "sendto";
end C-function;
