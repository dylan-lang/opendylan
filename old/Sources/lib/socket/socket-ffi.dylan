Module: socket-internals
Author: James Casey
Synopsis: Interface to FFI functions for net library
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// <socket> and family
define constant $SOCK-STREAM = 1;
define constant $SOCK-DGRAM = 2;

// Same values as $AF constants
define constant $PF-UNSPEC = 0;
define constant $PF-UNIX = 1;
define constant $PF-INET =  2;

define C-function c-socket-create
  result descriptor :: <C-int>;
  c-name: "c_socket_create";
end C-function c-socket-create;

define C-function c-socket-close
  input parameter descriptor :: <C-int>;
  result retval :: <C-int>;
  c-name: "c_socket_close";
end C-function c-socket-close;

define C-function c-socket-connect
  input parameter descriptor :: <C-int>;  
  input parameter remote :: <sys-in-addr>;
  input parameter port :: <C-unsigned-short>;
  output parameter localport :: <C-unsigned-short*>;
  result retval :: <C-int>;
  c-name: "c_socket_connect";
end C-function c-socket-connect;

define C-function c-socket-send
  input parameter descriptor :: <C-int>;
  input parameter ptr :: <C-string>;
  input parameter offset :: <C-int>;
  input parameter nbytes :: <C-int>;
  input parameter flag :: <C-int>;
  result retval :: <C-int>;
  c-name: "c_socket_send";
end C-function c-socket-send;

define C-function c-socket-recv
  input parameter descriptor :: <C-int>;
  input parameter ptr :: <C-string>;
  input parameter offset :: <C-int>;
  input parameter nbytes :: <C-int>;
  input parameter flag :: <C-int>;
  result retval :: <C-int>;
  c-name: "c_socket_recv";
end C-function c-socket-recv;

/*
define C-function c-socket-listen
  input parameter this :: <sys-socket>;
  input parameter backlog :: <C-int>;
  c-name: "c_socket_listen";
end C-function socket-listen;

define C-function c-socket-bind
  input parameter this :: <socket>;
  input output parameter where :: <sys-in-addr*>;
  input parameter lport :: <C-unsigned-short>;
  c-name: "c_socket_bind";
end C-function c-socket-bind;

define C-function c-socket-accept
  input  parameter this :: <sys-socket>;
  input output parameter what :: <sys-socket*>;
  c-name: "c_socket_accept";
end C-function c-socket-accept;
*/

// This will initialize winsock on win32 for us
define variable socket-ffi-initialized? :: <boolean> = #f;

define C-function c-socket-ffi-initialize
  result retval :: <C-int>;
  c-name: "c_socket_ffi_initialize";
end C-function c-socket-ffi-initialize;

// Mainly do winsock initialization
define function socket-ffi-initialize()
 =>()
  if (~ socket-ffi-initialized?)
    let ret = c-socket-ffi-initialize();
    if (ret == -1)
      error(make(<socket-error>,
		 format-string: "Can't initialize native socket interface", 
		 format-arguments: #f));
    end;
  socket-ffi-initialized? := #t;
  end;
end function socket-ffi-initialize;

define C-function c-socket-ffi-finalize
  result retval :: <C-int>;
  c-name: "c_socket_ffi_finalize";
end C-function c-socket-ffi-finalize;

define function socket-ffi-finalize()=>()
  if (socket-ffi-initialized?)
    let ret = c-socket-ffi-finalize();
    if (ret == -1)
      error(make(<socket-error>,
		 format-string: "Can't shutdown native socket interface", 
		 format-arguments: #f));
    end;
    socket-ffi-initialized? := #f;
  end;
end function socket-ffi-finalize;

// Do it here so that it is transparent. The user still needs to call 
// socket-ffi-finalize for completeness, let them put in a dummy 
// socket-ffi-initialize too.
//
socket-ffi-initialize();

//
// Utility functions to access errno (WSALastError on Win)
//
define C-function c-socket-errno
  result errno :: <C-int>;
  c-name: "c_socket_errno";
end C-function c-socket-errno;

define C-function c-socket-error-string
  parameter errno :: <C-int>;
  result string :: <C-string>;
  c-name: "c_socket_strerror";
end C-function c-socket-error-string;

define function socket-error-string(where :: <string>,  #key errno = #f) 
 => (retval :: <string>)
  let message = c-socket-error-string(if (~errno) c-socket-errno() else errno end);
  format-to-string("%s; %s", where, message);
end function socket-error;





