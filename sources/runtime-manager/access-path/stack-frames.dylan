module:     access-path-implementation
synopsis:   Descriptions of stack frames, and the implementation of building
            the stack trace, and reading lexical variables.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <STACK-FRAME>
//    The abstract description of a stack frame.

define sealed abstract class <stack-frame> (<object>)

  constant slot nub-vector-frame-index :: <integer>,
    required-init-keyword: index:;

  constant slot stack-frame-pointer :: <remote-value>,
    required-init-keyword: pointer:;

  constant slot return-address :: <remote-value>,
    required-init-keyword: return-address:;

  constant slot next-instruction :: <remote-value>,
    required-init-keyword: next-instruction:;

  constant slot frame-thread :: <remote-thread>,
    required-init-keyword: thread:;

  slot link-next :: false-or(<stack-frame>),
    init-value: #f,
    init-keyword: link-next:;

  slot link-previous :: false-or(<stack-frame>),
    init-value: #f,
    init-keyword: link-previous:;

end class;


///// <FUNCTION-FRAME>
//    A stack frame that actually represents a function call. As far as this
//    level of the architecture is concerned, all concrete stack frames are
//    function frames.

define sealed class <function-frame> (<stack-frame>)

  slot partial-lexicals-read? :: <boolean>,
    init-value: #f;

  slot full-lexicals-read? :: <boolean>,
    init-value: #f;

  slot lexicals-count :: <integer> = 0;

  slot lexicals-nub-table :: false-or(<NUBHANDLE>),
    init-value: #f;

  slot lexicals :: <vector>,
    init-value: #[];

end class;


define method print-object
  (f :: <stack-frame>, stream :: <stream>) => ()
  format(stream, "{Function Frame [SP:%=, RET:%=, EIP:%=, RETN:%=, RETP:%=]}",
	 f.stack-frame-pointer, f.return-address, f.next-instruction,
	 f.link-next & f.link-next.return-address,
	 f.link-previous & f.link-previous.return-address);
end;



///// <LEXICAL-VARIABLE>
//    The class that describes a variable local to a stack frame.

define abstract class <lexical-variable> (<object>)

  constant slot C-name :: <byte-string>,
    required-init-keyword: name:;

  constant slot lexical-variable-address :: <remote-location>,
    required-init-keyword: address:;

  constant slot lexical-variable-is-argument? :: <boolean>,
    required-init-keyword: argument?:;

end class;

define class <simple-lexical-variable> (<lexical-variable>)
end class;

define method make (class == <lexical-variable>, #rest keys, #key, #all-keys)
     => (lv)
  apply (make, <simple-lexical-variable>, keys);
end method;


///// UPDATE-THREAD-STACK-SIZE-ON-CONNECTION
//    Ensures that the debugger nub's stack trace is initialized. This
//    will tell us the size of the stack trace, which we can cache.

define open generic update-thread-stack-size-on-connection
    (connection :: <access-connection>, thread :: <remote-thread>) => ();

define method update-thread-stack-size-on-connection
    (connection :: <local-access-connection>, thread :: <remote-thread>) => ()
  thread.stack-size := 
    nub-initialize-stack-vectors(connection.connection-process, thread.nub-descriptor);
  thread.stack-size-valid? := #t;
end method;


///// UPDATE-THREAD-STACK-TRACE-ON-CONNECTION
//    Ensures that the stack trace is fully modelled for the thread.

define open generic update-thread-stack-trace-on-connection
    (connection :: <access-connection>, thread :: <remote-thread>) => ();

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



///// NUMBER-OF-FRAMES-ON-STACK
//    Returns the number of frames in a thread's stack trace.

define method number-of-frames-on-stack
    (path :: <access-path>, thread :: <remote-thread>)
       => (count :: <integer>)
  unless (thread.stack-size-valid?)
    update-thread-stack-size-on-connection(path.connection, thread)
  end unless;
  thread.stack-size
end method;


///// INITIALIZE-STACK-TRACE
//    Ensures we know everything we need to know about the stack, and then
//    return the topmost frame.

define method initialize-stack-trace
    (path :: <access-path>, thread :: <remote-thread>)
        => (top-frame :: <function-frame>)
  unless (thread.stack-trace-valid?)
    // This will do all the work.
    update-thread-stack-trace-on-connection(path.connection, thread)
  end unless;
  thread.thread-stack;
end method;


///// PREVIOUS-FRAME
//    Given a function frame, return the function frame that it was called
//    from.

define method previous-frame
    (path :: <access-path>, frame :: <function-frame>)
       => (maybe-frame :: false-or(<function-frame>))
  frame.link-previous
end method;


///// NEXT-FRAME
//    Given a function frame, return the function frame that it called.

define method next-frame
    (path :: <access-path>, frame :: <function-frame>)
       => (maybe-frame :: false-or(<function-frame>))
  frame.link-next
end method;


///// FRAME-POINTER
//    Given a frame, returns the address of its frame pointer.

define method frame-pointer
    (path :: <access-path>, frame :: <function-frame>)
       => (fp :: <remote-value>)
  frame.stack-frame-pointer
end method;


///// FRAME-RETURN-ADDRESS
//    Given a frame, returns its return address - ie. the address of the
//    instruction that will be executed as soon as the frame has
//    returned.

define method frame-return-address
    (path :: <access-path>, frame :: <function-frame>)
       => (ret-addr :: <remote-value>)
  frame.return-address
end method;


///// FRAME-INSTRUCTION-ADDRESS
//    Given a frame, returns the address of the next instruction that will
//    be executed within that frame. In the case of the topmost frame,
//    this is the value of the thread's program counter. In the case of
//    an arbitrary frame, this should be equal to the return address of
//    the called frame.

define method frame-instruction-address
    (path :: <access-path>, frame :: <function-frame>)
       => (ip :: <remote-value>)
  frame.next-instruction
end method;


///// DO-FRAME-ARGUMENTS

define method do-frame-arguments 
  (function :: <function>, ap :: <access-path>, frame :: <function-frame>) 
     => ()

  // Since we are lazily pulling debug info, we may not yet have
  // examined the lexicals for this frame. Read them now if
  // necessary.

  unless(frame.full-lexicals-read?)
    read-frame-lexicals (ap.connection, frame)
  end unless;

  // Apply the function only to frame arguments.

  for (lexvar in frame.lexicals)
    if (lexvar.lexical-variable-is-argument?)
      function (lexvar)
    end if
  end for;
end method;


///// DO-FRAME-LEXICALS

define method do-frame-lexicals 
  (function :: <function>, ap :: <access-path>, frame :: <function-frame>) 
     => ()

  // Again, now is the time to pull lexical variable information if
  // we need it.

  unless (frame.full-lexicals-read?)
    read-frame-lexicals (ap.connection, frame)
  end unless;

  // Now we apply "function" to all lexicals.

  for (lexvar in frame.lexicals)
    function (lexvar)
  end for;
end method;


///// LEXICAL-VARIABLE-NAME
//    This is a virtual slot accessor. The name is stored as a C-string. 
//    This function converts the name to a dylan <string>.

define method lexical-variable-name 
  (v :: <lexical-variable>) 
    => (name :: <string>)
  v.C-name;
end method;


///// FIND-LEXICAL-VARIABLE

define method find-lexical-variable 
  (ap :: <access-path>, frame :: <function-frame>, name :: <string>)
     => (lx :: false-or(<lexical-variable>))

  // Maybe this function should be implemented in terms of do-frame-lexicals
  // (in the same way as find-symbol), except that I'll run into the
  // closure bug again...

  let target = #f;

  // Once again, get the lexicals vector if necessary.

  unless (frame.full-lexicals-read?)
    read-frame-lexicals (ap.connection, frame)
  end unless;

  // Point target to a matching lexical if it is found. (There can only
  // be at most one matching lexical variable, otherwise the program
  // being debugged is completely knackered anyway!)

  for (lexvar in frame.lexicals)
    if (lexvar.lexical-variable-name = name)
      target := lexvar
    end if
  end for;

  // Just return whatever we managed to find.
  target;
end method;

/*
///// LIVE-LEXICALS-COUNT

define method live-lexicals-count
  (ap :: <access-path>, frame :: <function-frame>)
    => (i :: <integer>)

  unless (frame.partial-lexicals-read?)
    partial-read-frame-lexicals(ap.connection, frame);
  end unless;

  frame.lexicals-count;
end method;
*/

///// READ-FRAME-LEXICALS

define open generic read-frame-lexicals 
  (conn :: <access-connection>, frame :: <function-frame>) => ();

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

define method older-stack-frame?
  (ap :: <access-path>, this-one :: <remote-value>, 
   than-this-one :: <remote-value>)
     => (answer :: <boolean>)
  older-stack-frame-on-connection?(ap.connection, this-one, than-this-one);
end method;

define open generic older-stack-frame-on-connection?
  (conn :: <access-connection>, this-one :: <remote-value>,
   than-this-one :: <remote-value>)
     => (answer :: <boolean>);

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

define method register-interactive-code-segment
    (path :: <access-path>, from :: <remote-value>, to :: <remote-value>)
       => ()
  register-interactive-segment-on-connection(path.connection, from, to)
end method;

define open generic register-interactive-segment-on-connection
    (conn :: <access-connection>,
     from :: <remote-value>, to :: <remote-value>) 
 => ();

define method register-interactive-segment-on-connection
    (conn :: <local-access-connection>,
     from :: <remote-value>, to :: <remote-value>) 
       => ()
  nub-register-interactive-code-segment(conn.connection-process, from, to)
end method;
