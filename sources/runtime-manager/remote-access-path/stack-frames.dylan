module:     remote-access-path
synopsis:   Descriptions of stack frames, and the implementation of building
            the stack trace, and reading lexical variables.
author:     Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// UPDATE-THREAD-STACK-SIZE-ON-CONNECTION

define method update-thread-stack-size-on-connection
    (connection :: <remote-access-connection>, thread :: <remote-thread>) 
    => ()
  thread.stack-size := 
    Rtmgr/RemoteNub/initialize-stack-vectors
      (connection.nub, thread.rnub-descriptor);
  thread.stack-size-valid? := #t;
end method;


///// UPDATE-THREAD-STACK-TRACE-ON-CONNECTION

define method update-thread-stack-trace-on-connection
    (connection :: <remote-access-connection>, thread :: <remote-thread>) 
      => ()

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

  // Call the debugger nub to actually fill in the required data for each
  // stack frame.

  let (fp-vector, ip-vector, ra-vector) =
    Rtmgr/RemoteNub/read-stack-vectors(connection.nub,
				  thread.rnub-descriptor,
				  thread.stack-size);

  let last-frame = thread.stack-size - 1;

  // Construct the higher-level <function-frame> objects themselves.

  for (i from 0 to last-frame)
    stack-frame-vector[i] :=
      make(<function-frame>, index: i, thread: thread,
           pointer: as-remote-value(fp-vector[i]),
           return-address: as-remote-value(ra-vector[i]),
           next-instruction: as-remote-value(ip-vector[i]))
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
end method;


///// READ-FRAME-LEXICALS

define method read-frame-lexicals 
  (conn :: <remote-access-connection>, frame :: <function-frame>) => ()

  unless (frame.partial-lexicals-read?)
    partial-read-frame-lexicals(conn, frame);
  end unless;

  let count = frame.lexicals-count;
  let lookups = as-integer(frame.lexicals-nub-table);

  frame.lexicals := make (<vector>, size: count);

  for (i :: <integer> from 0 below frame.lexicals-count)
     // Find out all information about this lexical variable
     // from the debugger nub.

     let variable-type = #f; // Until we figure <remote-type> out!

     let variable-location = #f;

     let (variable-address :: <abstract-integer>,
          registers? :: <integer>,
          high-register-index :: <integer>,
          low-register-index :: <integer>,
          is-argument :: <integer>)
       = Rtmgr/RemoteNub/lexical-variable-address
           (conn.nub, as-integer(frame.stack-frame-pointer), lookups, i + 1);

     let variable-name =
      Rtmgr/RemoteNub/get-lexical-variable-name 
      (conn.nub, lookups, i + 1);

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
       variable-location := as-remote-value(variable-address);
     end if;

     frame.lexicals[i] := make (<lexical-variable>,
                                name: variable-name,
                                address: variable-location,
                                argument?: (is-argument == 1));
           
  end for;

  frame.full-lexicals-read? := #t;
  frame.lexicals-nub-table := #f;
  Rtmgr/RemoteNub/dispose-lookups(conn.nub, lookups);
end method;


define method partial-read-frame-lexicals
  (conn :: <remote-access-connection>, frame :: <function-frame>) => ()

  let (first :: <integer>, last :: <integer> , lookups :: <RNUBHANDLE>)
    = Rtmgr/RemoteNub/all-frame-lexicals
      (conn.nub, as-integer(frame.stack-frame-pointer),
       as-integer(frame.next-instruction));

  let count = last - first + 1;

  frame.partial-lexicals-read? := #t;
  frame.full-lexicals-read? := #f;
  frame.lexicals-count := count;
  frame.lexicals-nub-table := as-remote-pointer(lookups);
end method;


///// OLDER-STACK-FRAME?

define method older-stack-frame-on-connection?
  (conn :: <remote-access-connection>, this-one :: <remote-value>,
   than-this-one :: <remote-value>)
     => (answer :: <boolean>)
  let ans :: <integer> =
    Rtmgr/RemoteNub/older-stack-frame
    (conn.nub, as-integer(this-one), as-integer(than-this-one));
  if (ans == 1)
    #t
  else
    #f
  end if
end method;


///// REGISTER-INTERACTIVE-CODE-SEGMENT

define method register-interactive-segment-on-connection
    (conn :: <remote-access-connection>,
     from :: <remote-value>, to :: <remote-value>) 
       => ()
  Rtmgr/RemoteNub/register-interactive-code-segment
    (conn.nub, as-integer(from), as-integer(to))
end method;
