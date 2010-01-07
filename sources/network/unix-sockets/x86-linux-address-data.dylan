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

// link layer socket structure
define C-struct <sockaddr-ll>
  slot sll-family :: <C-unsigned-short>;
  slot sll-protocol :: <C-unsigned-short>;
  slot sll-ifindex :: <C-int>;
  slot sll-hatype :: <C-unsigned-short>;
  slot sll-pkttype :: <C-unsigned-char>;
  slot sll-halen :: <C-unsigned-char>;
  array slot sll-addr :: <C-unsigned-char>, length: 8;

  pointer-type-name: <sockaddr-ll*>;
end C-struct;

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

/// from net/if.h

define constant $IFF-UP          =    #x1;
define constant $IFF-BROADCAST   =    #x2;
define constant $IFF-DEBUG       =    #x4;
define constant $IFF-LOOPBACK    =    #x8;
define constant $IFF-POINTOPOINT =   #x10;
define constant $IFF-NOTRAILERS  =   #x20;
define constant $IFF-RUNNING     =   #x40;
define constant $IFF-NOARP       =   #x80;
define constant $IFF-PROMISC     =  #x100;
define constant $IFF-ALLMULTI    =  #x200;
define constant $IFF-MASTER      =  #x400;
define constant $IFF-SLAVE       =  #x800;
define constant $IFF-MULTICAST   = #x1000;
define constant $IFF-PORTSEL     = #x2000;
define constant $IFF-AUTOMEDIA   = #x4000;

define C-union <ifr-ifru>
  slot ifru-flags :: <C-short>;
  slot ifru-ivalue :: <C-int>;
end C-union;

define constant $IF-NAMESIZE = 16; 

define C-struct <ifreq>
  array slot %ifr-name :: <C-char>, length: $IF-NAMESIZE;
  slot ifr-ifru :: <ifr-ifru>;
  pointer-type-name: <ifreq*>;
end C-struct;

define method ifr-name (ifreq :: <ifreq*>)
 => (name :: <string>);
  let res = "";
  let i = 0;
  while (%ifr-name(ifreq, i) ~= 0 & i < $IF-NAMESIZE)
    res := add(res, as(<character>, %ifr-name(ifreq, i)));
    i := i + 1;
  end;
  res;
end method ifr-name;

define method ifr-name-setter (name :: <string>, ifreq :: <ifreq*>)
 => ()
  for(c in name,
      i from 0 below $IF-NAMESIZE - 1)
    %ifr-name(ifreq, i) := as(<integer>, c);
  finally
    %ifr-name(ifreq, i) := 0;
  end for;
end method ifr-name-setter;

define method ifr-ifindex (ifreq :: <ifreq*>)
 => (interface-index :: <integer>)
  ifreq.ifr-ifru.ifru-ivalue
end method ifr-ifindex;

define method ifr-ifindex-setter (value :: <integer>, ifreq :: <ifreq*>)
 => (interface-index :: <integer>)
  ifreq.ifr-ifru.ifru-ivalue := value
end method ifr-ifindex-setter;

define method ifr-flags (ifreq :: <ifreq*>)
 => (interface-index :: <integer>)
  ifreq.ifr-ifru.ifru-flags
end method ifr-flags;

define method ifr-flags-setter (value :: <integer>, ifreq :: <ifreq*>)
 => (interface-index :: <integer>)
  ifreq.ifr-ifru.ifru-flags := value
end method ifr-flags-setter;


define constant $SIOCGIFINDEX = #x8933;
define constant $SIOCGIFNAME  = #x8910;
define constant $SIOCGIFFLAGS = #x8913;
define constant $SIOCSIFFLAGS = #x8914;
