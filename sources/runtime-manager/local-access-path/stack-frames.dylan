module:     access-path-implementation
synopsis:   Descriptions of stack frames, and the implementation of building
            the stack trace, and reading lexical variables.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// UPDATE-THREAD-STACK-SIZE-ON-CONNECTION
//    Ensures that the debugger nub's stack trace is initialized. This
//    will tell us the size of the stack trace, which we can cache.

define method update-thread-stack-size-on-connection
    (connection :: <local-access-connection>, thread :: <remote-thread>) => ()
  thread.stack-size := 
    nub-initialize-stack-vectors(connection.connection-process, thread.nub-descriptor);
  thread.stack-size-valid? := #t;
end method;


///// UPDATE-THREAD-STACK-TRACE-ON-CONNECTION
//    Ensures that the stack trace is fully modelled for the thread.

define method update-thread-stack-trace-on-connection
    (connection :: <local-access-connection>, thread :: <remote-thread>) => ()

  // We gain knowledge of the stack in two stages. The first stage is for
  // the debugger nub to do its initializations. A useful side-effect of that
  // stage is that we get to know how big the stack trace is.
  // Do that stage now, if we haven't already.

  unless (thread.stack-size-valid?)
    update-thread-stack-size-on-connection(connection, thread)
  end unless;

  // Now we know that the debugger nub is harbouring an up-to-date stack
  // trace, and also that we know the size of that trace.

  let stack-frame-vector = make(<vector>, size: thread.stack-size);

  // Construct three primitive vectors that we can pass through the
  // FFI. One for frame pointers, one for return addresses, and one for
  // instruction pointers.

  let fp-vector = make(<POINTER-VECTOR>, element-count: thread.stack-size);
  let ip-vector = make(<POINTER-VECTOR>, element-count: thread.stack-size);
  let ra-vector = make(<POINTER-VECTOR>, element-count: thread.stack-size);

  // Call the debugger nub to actually fill in the required data for each
  // stack frame.

  nub-read-stack-vectors(connection.connection-process,
                         thread.nub-descriptor,
                         thread.stack-size,
                         fp-vector,
                         ip-vector,
                         ra-vector);

  let last-frame = thread.stack-size - 1;

  // Construct the higher-level <function-frame> objects themselves.

  for (i from 0 to last-frame)
    stack-frame-vector[i] :=
      make(<function-frame>, index: i, thread: thread,
           pointer: pointer-value(fp-vector, index: i),
           return-address: pointer-value(ra-vector, index: i),
           next-instruction: pointer-value(ip-vector, index: i))
  end for;

  // Now see that the frames are correctly chained together. The frames are
  // chained in a two-way linked list.

  stack-frame-vector[0].link-next := #f;
  stack-frame-vector[last-frame].link-previous := #f;
  unless (last-frame == 0)
    stack-frame-vector[0].link-previous := stack-frame-vector[1];
    stack-frame-vector[last-frame].link-next 
       := stack-frame-vector[last-frame - 1];
  end unless;
  for (i from 1 below last-frame)
    stack-frame-vector[i].link-previous := stack-frame-vector[i + 1];
    stack-frame-vector[i].link-next := stack-frame-vector[i - 1];
  end for;

  // Put a reference to the head of the chain into the <remote-thread>
  // itself. This serves as a cache that will remain valid until the
  // thread is allowed to continue and run some more code.

  thread.thread-stack := stack-frame-vector[0];
  thread.stack-trace-valid? := #t;

  // Finally, destroy those foreign vectors.
  destroy(fp-vector);
  destroy(ip-vector);
  destroy(ra-vector);

end method;



///// READ-FRAME-LEXICALS

define method read-frame-lexicals 
  (conn :: <local-access-connection>, frame :: <function-frame>) => ()

  unless (frame.partial-lexicals-read?)
    partial-read-frame-lexicals(conn, frame);
  end unless;

  let count = frame.lexicals-count;
  let lookups = frame.lexicals-nub-table;

  frame.lexicals := make (<vector>, size: count);

  for (i from 0 below frame.lexicals-count)
     // Find out all information about this lexical variable
     // from the debugger nub.

     let name-length :: <integer> 
       = nub-get-lexical-variable-name-length 
           (conn.connection-process, lookups, i + 1);

     let variable-name = make (<byte-string>, size: name-length);

     let variable-type = #f; // Until we figure <remote-type> out!

     let variable-location = #f;

     let (variable-address :: <remote-value>,
          registers? :: <ffi-integer>,
          high-register-index :: <ffi-integer>,
          low-register-index :: <ffi-integer>,
          is-argument :: <ffi-integer>)
       = nub-lexical-variable-address
           (conn.connection-process, frame.stack-frame-pointer, lookups, i + 1);

     nub-get-lexical-variable-name 
       (conn.connection-process, lookups, i + 1, name-length, variable-name);

     if (registers? == 1)
       // TODO: Make a register
       let unassigned-register = 
         find-register(frame.frame-thread.thread-access-path,
                       low-register-index);
       variable-location := 
         active-register(frame.frame-thread.thread-access-path,
                         frame.frame-thread,
                         unassigned-register);
     else
       variable-location := variable-address;
     end if;

     frame.lexicals[i] := make (<lexical-variable>,
                                name: variable-name,
                                address: variable-location,
                                argument?: (is-argument == 1));
           
  end for;

  frame.full-lexicals-read? := #t;
  frame.lexicals-nub-table := #f;
  nub-dispose-lookups(conn.connection-process, lookups);
end method;


define method partial-read-frame-lexicals
  (conn :: <local-access-connection>, frame :: <function-frame>) => ()

  let (first :: <integer>, last :: <integer> , lookups :: <NUBHANDLE>)
    = nub-all-frame-lexicals(conn.connection-process, frame.stack-frame-pointer,
                             frame.next-instruction);

  let count = last - first + 1;

  frame.partial-lexicals-read? := #t;
  frame.full-lexicals-read? := #f;
  frame.lexicals-count := count;
  frame.lexicals-nub-table := lookups;
end method;



///// OLDER-STACK-FRAME?
//    Decides whether one stack frame is older than another, purely by
//    considering their frame pointers. Note that this decision has to be
//    made by the debugger nub, because that is the only component that
//    "knows" the direction of stack growth.

define method older-stack-frame-on-connection?
  (conn :: <local-access-connection>, this-one :: <remote-value>,
   than-this-one :: <remote-value>)
     => (answer :: <boolean>)
  let ans :: <integer> =
    nub-older-stack-frame(conn.connection-process, this-one, than-this-one);
  if (ans == 1)
    #t
  else
    #f
  end if
end method;


///// REGISTER-INTERACTIVE-CODE-SEGMENT
//    A function via which the stack tracer can be informed that a region
//    of code has been dynamically created, and was not present in any
//    executable or library when the target application was started up.
//    This enables the debugger nub to do whatever is necessary to ensure
//    that the stack can still be reliably traced, given that there will
//    be no debug tables for this code.

define method register-interactive-segment-on-connection
    (conn :: <local-access-connection>,
     from :: <remote-value>, to :: <remote-value>) 
       => ()
  nub-register-interactive-code-segment(conn.connection-process, from, to)
end method;
