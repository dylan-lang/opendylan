module:        access-path-implementation
synopsis:      Making remote function calls in the application
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// REMOTE-CALL-ON-CONNECTION

define method remote-call-on-connection
    (conn :: <local-access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, 
     thread-was-suspended? :: <boolean>,
     #rest arguments)
       => (ra :: <remote-value>, cookie :: <object>)

  // The arguments need to be converted from the #rest sequence
  // into a primitive vector of <remote-value> objects.

  let arg-count :: <integer>
    = size(arguments);
  let arg-vector
    = make (<REMOTE-ARG-ARRAY>,
            element-count: arg-count);
  for (i from 0 below arg-count)
    pointer-value(arg-vector, index: i) := arguments[i];
  end for;

  // We have everything we need to make the call. The return
  // value is a thread context cookie. The nub function is called
  // as a side-effect.

  let (ret-addr, context-cookie)
    = nub-setup-function-call(conn.connection-process, thr.nub-descriptor,
                              function, arg-count, arg-vector);

  // Since the debugger nub will now have copied the arguments onto the
  // runtime stack, we can destroy the allocated vector.

  destroy(arg-vector);

  values (ret-addr,
          make (<THREAD-CONTEXT>,
                suspended?: thread-was-suspended?,
                nub-descriptor: context-cookie));
end method;


///// REMOTE-CALL-RESULT-ON-CONNECTION

define method remote-call-result-on-connection
    (conn :: <local-access-connection>, thr :: <remote-thread>)
      => (result :: <remote-value>)
  nub-get-function-result(conn.connection-process, thr.nub-descriptor);
end method;


///// REMOTE-RESTORE-CONTEXT-ON-CONNECTION

define method remote-restore-context-on-connection
    (conn :: <local-access-connection>, thr :: <remote-thread>, 
     ctx :: <THREAD-CONTEXT>)
      => ()
  nub-restore-context(conn.connection-process, thr.nub-descriptor,
                      ctx.nub-descriptor);
end method;


///// REMOTE-CALL-SPY-ON-CONNECTION

define method remote-call-spy-on-connection
    (ap :: <access-path>, conn :: <local-access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
       => (result :: <remote-value>, errcode :: <integer>)

  let arg-vector :: <REMOTE-ARG-ARRAY> = conn.spy-function-argument-vector;

  // Construct the vector of arguments

  let arg-count :: <integer>
    = size(arguments);

  if (arg-count > $max-spy-function-arguments)
    error("Serious internal debugger error: Exceeded maximum arg count "
          "in a spy call.")
  end if;

  for (i from 0 below arg-count)
    pointer-value(arg-vector, index: i) := arguments[i];
  end for;

  // And make the call, returning the results from the nub.

  nub-remote-call-spy(conn.connection-process, thr.nub-descriptor,
                      function, arg-count, arg-vector);
end method;

