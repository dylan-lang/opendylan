Module:       unix-sockets
Synopsis:     Baseline socket interface functions as per Unix 98 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Source: http://www.opengroup.org/onlinepubs/007908799/xnsix.html

define inline-only C-function ioctl
  parameter socket :: <C-int>;
  parameter request-type :: <C-int>;
  parameter param :: <C-void*>;
  result val :: <C-int>;
  c-name: "ioctl";
end C-function;

define inline-only C-function accept
  parameter socket :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t*>;
  result val :: <C-int>;
  c-name: "accept";
end C-function;

define inline-only C-function bind
  parameter socket :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t>;
  result val :: <C-int>;
  c-name: "bind";
end C-function;

// TODO: Shared: close
define inline-only C-function close
  parameter socket :: <C-int>;
  result val :: <C-int>;
  c-name: "close";
end C-function;

define inline-only C-function connect
  parameter socket :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t>;
  result val :: <C-int>;
  c-name: "connect";
end C-function;

// Shared: fcntl
// Shared: fgetpos
// Shared: fsetpos
// Shared: ftell

define inline-only C-function getpeername
  parameter socket :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t*>;
  result val :: <C-int>;
  c-name: "getpeername";
end C-function;

define inline-only C-function getsockname
  parameter socket :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t*>;
  result val :: <C-int>;
  c-name: "getsockname";
end C-function;

define inline-only C-function getsockopt
  parameter socket :: <C-int>;
  parameter level :: <C-int>;
  parameter option-name :: <C-int>;
  parameter option-value :: <C-void*>;
  parameter option-len :: <socklen-t*>;
  result val :: <C-int>;
  c-name: "getsockopt";
end C-function;

define inline-only C-function listen
  parameter socket :: <C-int>;
  parameter backlog :: <C-int>;
  result val :: <C-int>;
  c-name: "listen";
end C-function;

// Shared: lseek
// Shared: poll
// Shared: read

define inline-only C-function recv
  parameter socket :: <C-int>;
  parameter buffer :: <C-void*>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "recv";
end C-function;

define inline-only C-function recvfrom
  parameter socket :: <C-int>;
  parameter buffer :: <C-void*>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  parameter address :: <sockaddr*>;
  parameter address-len :: <socklen-t*>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "recvfrom";
end C-function;

define inline-only C-function recvmsg
  parameter socket :: <C-int>;
  parameter message :: <msghdr*>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "recvmsg";
end C-function;

// Shared: select

define inline-only C-function send
  parameter socket :: <C-int>;
  parameter buffer :: <C-void*>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "send";
end C-function;

define inline-only C-function sendmsg
  parameter socket :: <C-int>;
  parameter message :: <msghdr*>;
  parameter flags :: <C-int>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "sendmsg";
end C-function;

define inline-only C-function sendto
  parameter socket :: <C-int>;
  parameter message :: <C-void*>;
  parameter length :: <size-t>;
  parameter flags :: <C-int>;
  parameter dest-addr :: <sockaddr*>;
  parameter dest-len :: <socklen-t>;
  result val :: <C-int>; // TODO: ssize_t 
  c-name: "sendto";
end C-function;

define inline-only C-function setsockopt
  parameter socket :: <C-int>;
  parameter level :: <C-int>;
  parameter option-name :: <C-int>;
  parameter option-value :: <C-void*>;
  parameter option-len :: <socklen-t>;
  result val :: <C-int>;
  c-name: "setsockopt";
end C-function;

define inline-only C-function shutdown
  parameter socket :: <C-int>;
  parameter how :: <C-int>;
  result val :: <C-int>;
  c-name: "shutdown";
end C-function;

define inline-only C-function socket
  parameter domain :: <C-int>;
  parameter type :: <C-int>;
  parameter protocol :: <C-int>;
  result val :: <C-int>;
  c-name: "socket";
end C-function;

define inline-only C-function socketpair
  parameter domain :: <C-int>;
  parameter type :: <C-int>;
  parameter protocol :: <C-int>;
  parameter socket-vector :: <C-int*>;
  result val :: <C-int>;
  c-name: "socketpair";
end C-function;

// Shared: write

// eof
