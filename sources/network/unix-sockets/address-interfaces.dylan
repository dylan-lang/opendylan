Module:       unix-sockets
Synopsis:     Baseline socket interface functions as per Unix 98 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Source: http://www.opengroup.org/onlinepubs/007908799/xnsix.html

// Network host database functions

define inline-only C-function endhostent
  c-name: "endhostent";
end C-function;

define inline-only C-function gethostbyaddr
  parameter addr :: <C-void*>;
  parameter len :: <size-t>;
  parameter type :: <C-int>;
  result val :: <hostent*>;
  c-name: "gethostbyaddr";
end C-function;

define inline-only C-function gethostbyname
  parameter name :: <C-char*>;
  result val :: <hostent*>;
  c-name: "gethostbyname";
end C-function;

define inline-only C-function gethostent
  result val :: <hostent*>;
  c-name: "gethostent";
end C-function;

define inline-only C-function sethostent
  parameter stayopen :: <C-int>;
  c-name: "sethostent";
end C-function;

// Network database functions

define inline-only C-function endnetent
  c-name: "endnetent";
end C-function;

define inline-only C-function getnetbyaddr
  parameter net :: <in-addr-t>;
  parameter type :: <C-int>;
  result val :: <netent*>;
  c-name: "getnetbyaddr";
end C-function;

define inline-only C-function getnetbyname
  parameter name :: <C-char*>;
  result val :: <netent*>;
  c-name: "getnetbyname";
end C-function;

define inline-only C-function getnetent
  result val :: <netent*>;
  c-name: "getnetent";
end C-function;

define inline-only C-function setnetent
  parameter stayopen :: <C-int>;
  c-name: "setnetent";
end C-function;

// Network services database functions

define inline-only C-function endservent
  c-name: "endservent";
end C-function;

define inline-only C-function getservbyname
  parameter name :: <C-char*>;
  parameter proto :: <C-char*>;
  result val :: <servent*>;
  c-name: "getservbyname";
end C-function;

define inline-only C-function getservbyport
  parameter port :: <C-int>;
  parameter proto :: <C-char*>;
  result val :: <servent*>;
  c-name: "getservbyport";
end C-function;

define inline-only C-function getservent
  result val :: <servent*>;
  c-name: "getservent";
end C-function;

define inline-only C-function setservent
  parameter stayopen :: <C-int>;
  c-name: "setservent";
end C-function;

// Network protocol database functions

define inline-only C-function endprotoent
  c-name: "endprotoent";
end C-function;

define inline-only C-function getprotobyname
  parameter name :: <C-char*>;
  result val :: <protoent*>;
  c-name: "getprotobyname";
end C-function;

define inline-only C-function getprotobynumber
  parameter proto :: <C-int>;
  result val :: <protoent*>;
  c-name: "getprotobynumber";
end C-function;

define inline-only C-function getprotoent
  result val :: <protoent*>;
  c-name: "getprotoent";
end C-function;

define inline-only C-function setprotoent
  parameter stayopen :: <C-int>;
  c-name: "setprotoent";
end C-function;

// Network address manipulation

define inline-only C-function inet-addr
  parameter cp :: <C-char*>;
  result val :: <in-addr-t>;
  c-name: "inet_addr";
end C-function;

define inline-only C-function inet-lnaof
  parameter in :: <in-addr>;
  result val :: <in-addr-t>;
  c-name: "inet_lnaof";
end C-function;

define inline-only C-function inet-makeaddr
  parameter net :: <in-addr-t>;
  parameter lna :: <in-addr-t>;
  result val :: <in-addr>;
  c-name: "inet_makeaddr";
end C-function;

define inline-only C-function inet-netof
  parameter in :: <in-addr>;
  result val :: <in-addr-t>;
  c-name: "inet_netof";
end C-function;

define inline-only C-function inet-network
  parameter cp :: <C-char*>;
  result val :: <in-addr-t>;
  c-name: "inet_network";
end C-function;

define inline-only C-function inet-ntoa
  parameter in :: <in-addr>;
  result val :: <C-char*>;
  c-name: "inet_ntoa";
end C-function;
