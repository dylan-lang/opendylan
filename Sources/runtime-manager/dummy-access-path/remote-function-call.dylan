module:         access-path-implementation
synopsis:       Dummy implementations of remote function calls
author:         Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// TODO: (Maybe)
// We could be more adventurous and actually attempt remote calls in
// the fake machine state. There wouldn't be much point, but it might work
// if we really wanted to try it out.


///// REMOTE-CALL
//    Dummy implementation

define method remote-call
    (ap :: <access-path>, thread :: <remote-thread>,
     entry-point :: <remote-value>, #rest arguments)
      => (return-address :: <remote-value>, cookie :: <object>)
  values(as-remote-value(0), 0);
end method;


///// REMOTE-CALL-RESULT
//    Dummy implementation

define method remote-call-result
    (ap :: <access-path>, thread :: <remote-thread>)
      => (call-result :: <remote-value>)
  as-remote-value(0);
end method;


///// REMOTE-RESTORE-CONTEXT
//    Dummy implementation

define method remote-restore-context
    (ap :: <access-path>, thread :: <remote-thread>,
     cookie :: <object>)
      => ()
end method;


///// REMOTE-CALL-SPY
//    Dummy implementation

define method remote-call-spy
    (ap :: <access-path>, thread :: <remote-thread>,
     entry-point :: <remote-value>, #rest arguments)
      => (spy-result :: <remote-value>)
  as-remote-value(0);
end method;


