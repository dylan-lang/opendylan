module:    access-path-implementation
synopsis:  Intepretation of machine instructions on the dummy simulator
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// INTERPRET-INSTRUCTION-AT-CURRENT-LOCATION
//    Abstracts over simple instruction disassembly. Returns enough
//    information to decide what kind of instruction this is (flow-wise),
//    and what the destination address is.
//    All addresses are absolute <remote-address> instances. PC-relative
//    addresses will have been pre-calculated and resolved.

define method interpret-instruction-on-connection
    (conn :: <local-access-connection>, thread :: <remote-thread>)
       => (flow,
           destination :: false-or(<remote-value>),
           length :: false-or(<integer>))
  let (flow-code :: <integer>,
       target :: <remote-value>,
       size-in-bytes :: <integer>) =
    nub-interpret-instruction-at-current-location
       (conn.connection-process, thread.nub-descriptor);

  if (flow-code == $flowIllegal)
    values(flow-code, #f, #f)
  else
    values(flow-code, target, size-in-bytes)
  end if
end method;
