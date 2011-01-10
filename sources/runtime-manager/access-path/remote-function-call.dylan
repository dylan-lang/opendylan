module:        access-path-implementation
synopsis:      Making remote function calls in the application
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// EXPORTED GENERIC FUNCTIONS

define generic remote-call 
    (ap :: <access-path>, thread :: <remote-thread>, 
     function :: <remote-value>, #rest arguments)
  => (return-address :: <remote-value>, context :: <object>);

define generic remote-call-result 
    (ap :: <access-path>, thread :: <remote-thread>)
  => (result :: <remote-value>);

define generic remote-restore-context
    (ap :: <access-path>, thread :: <remote-thread>,
     context :: <object>) => ();

define generic remote-call-spy
    (ap :: <access-path>,
     thread :: <remote-thread>, function :: <remote-value>,
     #rest arguments)
  => (result :: <remote-value>, aborted? :: <boolean>);


///// REMOTE-CALL

define method remote-call 
    (ap :: <access-path>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
       => (ret-addr :: <remote-value>, cookie :: <object>)

  debugger-message("remote-call %= %=", thr, function);

  let thread-was-suspended? = thr.thread-suspended?;
  if (thread-was-suspended?)
    dylan-resume-thread(ap, thr);
  end if;
  apply (remote-call-on-connection, ap.connection, thr, function, 
         thread-was-suspended?, arguments);
end method;


///// REMOTE-CALL-ON-CONNECTION

define open generic remote-call-on-connection
    (conn :: <access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, 
     thread-was-suspended? :: <boolean>,
     #rest arguments)
       => (ra :: <remote-value>, cookie :: <object>);

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


///// REMOTE-CALL-RESULT

define method remote-call-result 
    (ap :: <access-path>, thr :: <remote-thread>)
      => (result :: <remote-value>)
  debugger-message("remote-call-result %=", thr);
  remote-call-result-on-connection(ap.connection, thr);
end method;


///// REMOTE-CALL-RESULT-ON-CONNECTION

define open generic remote-call-result-on-connection
    (conn :: <access-connection>, thr :: <remote-thread>)
      => (result :: <remote-value>);

define method remote-call-result-on-connection
    (conn :: <local-access-connection>, thr :: <remote-thread>)
      => (result :: <remote-value>)
  nub-get-function-result(conn.connection-process, thr.nub-descriptor);
end method;


///// REMOTE-RESTORE-CONTEXT

define method remote-restore-context
    (ap :: <access-path>, thr :: <remote-thread>, ctx :: <THREAD-CONTEXT>)
      => ()
  debugger-message("remote-restore-context %=", thr);
  remote-restore-context-on-connection(ap.connection, thr, ctx);
  if (ctx.thread-was-suspended-by-debugger?)
    // The thread was released only for the duration of this remote
    // call, so suspend it again!
    suspend-thread(ap, thr);
  end if;
end method;


///// REMOTE-RESTORE-CONTEXT-ON-CONNECTION

define open generic remote-restore-context-on-connection
    (conn :: <access-connection>, thr :: <remote-thread>, 
     ctx :: <THREAD-CONTEXT>)
 => ();

define method remote-restore-context-on-connection
    (conn :: <local-access-connection>, thr :: <remote-thread>, 
     ctx :: <THREAD-CONTEXT>)
      => ()
  nub-restore-context(conn.connection-process, thr.nub-descriptor,
                      ctx.nub-descriptor);
end method;


///// REMOTE-CALL-SPY

define method remote-call-spy
    (ap :: <access-path>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
       => (result :: <remote-value>, aborted? :: <boolean>)

  debugger-message("remote-call-spy %= %= %=", thr, function, arguments);

  // If the selected thread is suspended, release it for the duration
  // of the remote call.

  // Relax permanent suspension temporarily so that these threads can
  // continue to be used for spy calls while interacting on other threads

  let thread-was-permanently-suspended? = thread-permanently-suspended?(ap, thr);
  if (thread-was-permanently-suspended?)
    debugger-message("Releasing permanent suspension on %= for spy call %=", thr, function);
    thread-permanently-suspended?(ap, thr) := #f;
  end if;

  let thread-was-suspended? = thr.thread-suspended?;
  if (thread-was-suspended?)
    dylan-resume-thread(ap, thr);
  end if;

  block ()

  // Do the call.

  let (result, errcode)
    = apply(remote-call-spy-on-connection, ap, ap.connection, thr,
            function, arguments);

  values(result, errcode == 1);

  cleanup

  // And re-suspend the thread if necessary.

  if (thread-was-suspended?)
    suspend-thread(ap, thr);
  end if;

  if (thread-was-permanently-suspended?)
    debugger-message("Restoring permanent suspension on %= for spy call %=", thr, function);
    thread-permanently-suspended?(ap, thr) := #t;
  end if;

  end block;

end method;


///// REMOTE-CALL-SPY-ON-CONNECTION

define open generic remote-call-spy-on-connection
    (ap :: <access-path>, conn :: <access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
 => (result :: <remote-value>, errcode :: <integer>);

define method remote-call-spy-on-connection
    (ap :: <access-path>, conn :: <local-access-connection>, thr :: <remote-thread>,
     function :: <remote-value>, #rest arguments)
       => (result :: <remote-value>, errcode :: <integer>)

  let arg-vector :: <REMOTE-ARG-ARRAY> = ap.spy-function-argument-vector;

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

