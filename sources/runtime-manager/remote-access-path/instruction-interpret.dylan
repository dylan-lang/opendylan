module:    remote-access-path
synopsis:  Intepretation of machine instructions on the dummy simulator
author:    Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// INTERPRET-INSTRUCTION-AT-CURRENT-LOCATION

define method interpret-instruction-on-connection
    (conn :: <remote-access-connection>, thread :: <remote-thread>)
       => (flow,
           destination :: false-or(<remote-value>),
           length :: false-or(<integer>))
  let (flow-code :: <integer>,
       target :: <RTARGET-ADDRESS>,
       size-in-bytes :: <integer>) =
    Rtmgr/RemoteNub/interpret-instruction-at-current-location
       (conn.nub, thread.rnub-descriptor);

  if (flow-code == $flowIllegal)
    values(flow-code, #f, #f)
  else
    values(flow-code, as-remote-value(target), size-in-bytes)
  end if
end method;
