Module:       unix-sockets
Synopsis:     Baseline Unix 98 address resolution structs, types, and constants 
              functions as defined in Linux
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Source: http://www.opengroup.org/onlinepubs/007908799/xnsix.html

define constant <in-port-t> = <C-unsigned-short>;
define constant <in-port-t*> = <C-unsigned-short*>;
define constant <in-addr-t> = <C-raw-unsigned-long>;
define constant <in-addr-t*> = <C-raw-unsigned-long*>;

// TODO: Abide by the letter of the law?
/*
define C-struct <in-addr>
  slot s-addr-value :: <in-addr-t>;
  pointer-type-name: <in-addr*>;
end C-struct;
*/

define constant <in-addr> = <in-addr-t>;
define constant <in-addr*> = <in-addr-t*>;

define method s-addr-value 
    (addr :: <in-addr*>) => (addr-val :: <machine-word>)
  pointer-value(addr)
end method;

define method s-addr-value-setter
    (addr-val :: <machine-word>, addr :: <in-addr*>) 
 => (addr-val :: <machine-word>)
  error("The type struct in_addr has been mapped to Dylan as an immutable "
        "machine word value and its field cannot be set.");
end method;

define C-pointer-type <in-addr**> => <in-addr*>;

define C-struct <sockaddr-in>
  slot sin-family-value :: <sa-family-t>;
  slot sin-port-value :: <in-port-t>;
  slot sin-addr-value :: <in-addr>;

  // NOTE: A filler required on linux to make all address structs the same
  // size as struct sockaddr.
  array slot sin-data :: <C-char>, length: 8;

  pointer-type-name: <sockaddr-in*>;
end C-struct;

ignore(sin-data, sin-data-setter);

// IP_PROTO*

define constant $INADDR-ANY = as(<machine-word>, 0);
define constant $INADDR-BROADCAST = as(<machine-word>, #xffffffff);
define constant $INADDR-NONE = as(<machine-word>, #xffffffff);

define constant <uint16-t> = <C-unsigned-short>;
define constant <uint32-t> = <C-raw-unsigned-long>;

define inline-only C-function ntohl 
  parameter netlong :: <uint32-t>;
  result val :: <uint32-t>;
  c-name: "ntohl";
end C-function;

define inline-only C-function ntohs 
  parameter netshort :: <uint16-t>;
  result val :: <uint16-t>;
  c-name: "ntohs";
end C-function;

define inline-only C-function htonl 
  parameter hostlong :: <uint32-t>;
  result val :: <uint32-t>;
  c-name: "htonl";
end C-function;

define inline-only C-function htons 
  parameter hostshort :: <uint16-t>;
  result val :: <uint16-t>;
  c-name: "htons";
end C-function;

/// From netdb.h

define C-pointer-type <C-char**> => <C-char*>;

define C-struct <hostent>
  slot h-name-value :: <C-char*>;
  slot h-aliases-value :: <C-char**>;
  slot h-addrtype-value :: <C-int>;
  slot h-length-value :: <C-int>;
  slot h-addr-list-value :: <C-char**>;
  pointer-type-name: <hostent*>;
end C-struct;

define C-struct <netent>
  slot n-name-value :: <C-char*>;
  slot n-aliases-value :: <C-char**>;
  slot n-addrtype-value :: <C-int>;
  slot n-net-value :: <C-raw-unsigned-long>;
  pointer-type-name: <netent*>;
end C-struct;

define C-struct <protoent>
  slot p-name-value :: <C-char*>;
  slot p-aliases-value :: <C-char**>;
  slot p-proto-value :: <C-int>;
  pointer-type-name: <protoent*>;
end C-struct;

define C-struct <servent>
  slot s-name-value :: <C-char*>;
  slot s-aliases-value :: <C-char**>;
  slot s-port-value :: <C-int>;
  slot s-proto-value :: <C-char*>;
  pointer-type-name: <servent*>;
end C-struct;

/// From unistd.h

define C-function gethostname
  parameter name :: <C-char*>;
  parameter namelen :: <size-t>;
  result val :: <C-int>;
  c-name: "gethostname";
end C-function;

// eof
