module:          remote-access-path
synopsis:        Special access-path functions that take advantage of low-level
                 knowledge of the dylan implementation.
author:          Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DYLAN-CALCULATE-DESTINATION-FOR-STEP-INTO

define method calculate-step-into-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
  => (address :: <remote-value>,
      use-function-register? :: <boolean>,
      success? :: <boolean>)
  let (address :: <RTARGET-ADDRESS>,
       use-freg :: <integer>,
       ok :: <integer>) =
    Rtmgr/RemoteNub/dylan-calculate-step-into(conn.nub, thread.rnub-descriptor);
  values (as-remote-value(address),
          use-freg == 1,
          ok == 1)
end method;


///// DYLAN-THREAD-ENVIRONMENT-BLOCK-ADDRESS

define method teb-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
       => (teb :: <remote-value>)
  let (teb-pointer :: <RTARGET-ADDRESS>, valid :: <integer>)
    = Rtmgr/RemoteNub/dylan-thread-environment-block-address
        (conn.nub, thread.rnub-descriptor);
  if (valid == 1)
    as-remote-value(teb-pointer);
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CURRENT-FUNCTION

define method current-function-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
       => (remote-lambda :: <remote-value>)
  as-remote-value
  (Rtmgr/RemoteNub/dylan-current-function(conn.nub, thread.rnub-descriptor));
end method;


///// DYLAN-THREAD-MV-BUFFER-LIVE?

define method mv-buffer-live-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
  => (code :: <integer>)
  Rtmgr/RemoteNub/dylan-thread-mv-buffer-live(conn.nub, thread.rnub-descriptor)
end method;
