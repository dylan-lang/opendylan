module:          access-path-implementation
synopsis:        Special access-path functions that take advantage of low-level
                 knowledge of the dylan implementation.
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DYLAN-CALCULATE-DESTINATION-FOR-STEP-INTO
//    Given a thread that poised at a recorded location, calculates
//    the address that should be breakpointed in order to effect
//    a "step-into" operation. It may also decide that the function
//    register should be used instead. (This can be obtained with the
//    DYLAN-CURRENT-FUNCTION utility).

define method calculate-step-into-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  => (address :: <remote-value>,
      use-function-register? :: <boolean>,
      success? :: <boolean>)
  let (address :: <remote-value>,
       use-freg :: <integer>,
       ok :: <integer>) =
    nub-dylan-calculate-step-into(conn.connection-process, thread.nub-descriptor);
  values (address,
          use-freg == 1,
          ok == 1)
end method;


///// DYLAN-THREAD-ENVIRONMENT-BLOCK-ADDRESS
//    Gets the thread-local pointer to the dylan-level thread environment
//    block.

define method teb-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
       => (teb :: <remote-value>)
  let (teb-pointer :: <remote-value>, valid :: <integer>)
    = nub-dylan-thread-environment-block-address
        (conn.connection-process, thread.nub-descriptor);
  if (valid == 1)
    teb-pointer;
  else
    as-remote-value(0);
  end if
end method;


///// DYLAN-CURRENT-FUNCTION
//    Returns the value of the function register. This cannot determine
//    liveness of the function register. That check should already have been
//    made before calling this method.

define method current-function-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
       => (remote-lambda :: <remote-value>)
  nub-dylan-current-function(conn.connection-process, thread.nub-descriptor);
end method;


///// DYLAN-THREAD-MV-BUFFER-LIVE?
//    Queries the necessary flags in the context of a dylan thread to
//    decide whether the contents of the MV buffer are current.

define method mv-buffer-live-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
  => (code :: <integer>)
  nub-dylan-thread-mv-buffer-live(conn.connection-process, thread.nub-descriptor)
end method;
