module:     remote-access-path
synopsis:   Implementation of the <access-path> class
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define method make-access-connection
    (ap :: <access-path>, conn :: <remote-debugger-connection-implementation>,
     #key description = ap.access-path-application)
 => (conn :: <remote-access-connection>)
  make(<remote-access-connection>,
       debugger-connection: conn,
       description: description,
       server: conn.connection-server,
       orb: conn.connection-orb)
end method;

define function %spy-function-argument-remote-vector(ap :: <access-path>)
 => (remote-vector :: <RTARGET-ADDRESS-SEQ>)
  ap.spy-function-argument-remote-vector
  | (ap.spy-function-argument-remote-vector :=
       make(<RTARGET-ADDRESS-SEQ>, size: $max-spy-function-arguments, fill: 0));
end function;

define function %stepping-locations-remote-vector(ap :: <access-path>)
 => (remote-vector :: <RTARGET-ADDRESS-SEQ>)
  ap.stepping-locations-remote-vector
  | (ap.stepping-locations-remote-vector :=
       make(<RTARGET-ADDRESS-SEQ>, size: $max-stepping-locations, fill: 0));
end function;
