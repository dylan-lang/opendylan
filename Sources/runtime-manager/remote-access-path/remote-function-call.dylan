module:        remote-access-path
synopsis:      Making remote function calls in the application
author:        Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// REMOTE-CALL-ON-CONNECTION

define method remote-call-on-connection
    (conn :: <remote-access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, 
     thread-was-suspended? :: <boolean>,
     #rest arguments)
       => (ra :: <remote-value>, cookie :: <object>)

  // The arguments need to be converted from the #rest sequence
  // into a vector of <RTARGET-ADDRESS> objects.

  let arg-count :: <integer> = size(arguments);
  let arg-vector = make (<RTARGET-ADDRESS-SEQ>,
			 size: arg-count, fill: 0);
  for (i from 0 below arg-count)
    arg-vector[i] := as-integer(arguments[i]);
  end for;

  // We have everything we need to make the call. The return
  // value is a thread context cookie. The nub function is called
  // as a side-effect.


  let (ret-addr :: <RTARGET-ADDRESS>, context-cookie :: <RNUBHANDLE>)
    = Rtmgr/RemoteNub/setup-function-call
      (conn.nub, thr.rnub-descriptor,
       as-integer(function), arg-count, arg-vector);

  values (as-remote-value(ret-addr),
          make (<THREAD-CONTEXT>,
                suspended?: thread-was-suspended?,
                nub-descriptor: as-remote-pointer(context-cookie)));
end method;


///// REMOTE-CALL-RESULT-ON-CONNECTION

define method remote-call-result-on-connection
    (conn :: <remote-access-connection>, thr :: <remote-thread>)
      => (result :: <remote-value>)
  as-remote-value(Rtmgr/RemoteNub/get-function-result(conn.nub, thr.rnub-descriptor));
end method;


///// REMOTE-RESTORE-CONTEXT-ON-CONNECTION

define method remote-restore-context-on-connection
    (conn :: <remote-access-connection>, thr :: <remote-thread>, 
     ctx :: <THREAD-CONTEXT>)
      => ()
  Rtmgr/RemoteNub/restore-context(conn.nub, thr.rnub-descriptor,
			     as-integer(ctx.nub-descriptor));
end method;


///// REMOTE-CALL-SPY-ON-CONNECTION

define method remote-call-spy-on-connection
    (ap :: <access-path>, conn :: <remote-access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
       => (result :: <remote-value>, errcode :: <integer>)

  let arg-vector :: <RTARGET-ADDRESS-SEQ> = ap.%spy-function-argument-remote-vector;

  // Construct the vector of arguments

  let arg-count :: <integer>
    = size(arguments);

  if (arg-count > $max-spy-function-arguments)
    error("Serious internal debugger error: Exceeded maximum arg count "
          "in a spy call.")
  end if;

  for (i from 0 below arg-count)
    arg-vector[i] := as-integer(arguments[i]);
  end for;

  // And make the call, returning the results from the nub.

  let (result :: <RTARGET-ADDRESS>, errcode) =
    Rtmgr/RemoteNub/remote-call-spy(conn.nub, thr.rnub-descriptor,
			       as-integer(function), arg-count, arg-vector);
  values(as-remote-value(result), errcode)
end method;
