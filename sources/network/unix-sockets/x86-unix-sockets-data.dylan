Module:       unix-sockets
Synopsis:     Baseline Unix 98 socket structs, types, and constants functions as
              defined in Linux
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Source: http://www.opengroup.org/onlinepubs/007908799/xnsix.html

define constant <size-t> = <C-int>;

define constant <socklen-t> = <C-unsigned-int>;
define constant <socklen-t*> = <C-unsigned-int*>;

define C-struct <linger>
  slot l-onoff-value :: <C-int>;
  slot l-linger-value :: <C-int>;
  pointer-type-name: <linger*>;
end C-struct;

define C-struct <msghdr>
  slot msg-name-value :: <C-void*>;
  slot msg-namelen-value :: <C-int>;
  slot msg-iov-value :: <C-void*>; // TODO: struct iovec
  slot msg-iovlen-value :: <size-t>; // TODO: __kernel_size_t
  slot msg-control-value :: <C-void*>;
  slot msg-controllen-value :: <size-t>; // TODO: __kernel_size_t
  slot msg-flags-value :: <C-unsigned-int>;
  pointer-type-name: <msghdr*>;
end C-struct;

define C-struct <cmsghdr>
  slot cmsg-len-value :: <size-t>; // TODO: __kernel_size_t
  slot cmsg-level-value :: <C-int>;
  slot cmsg-type-value :: <C-int>;
  pointer-type-name: <cmsghdr*>;
end C-struct;

define constant $SOCK-STREAM = 1;
define constant $SOCK-DGRAM = 2;
define constant $SOCK-RAW = 3;
define constant $SOCK-SEQPACKET = 5;

define constant $PF-UNSPEC = 0;
define constant $PF-LOCAL = 1;
define constant $PF-UNIX = $PF-LOCAL;
define constant $PF-INET = 2;

define constant $AF-UNIX =	$PF-UNIX;
define constant $AF-INET =	$PF-INET;
define constant $AF-UNSPEC =	$PF-UNSPEC;

define constant $SHUT-RD = 0;
define constant $SHUT-WR = 1;
define constant $SHUT-RDWR = 2;

